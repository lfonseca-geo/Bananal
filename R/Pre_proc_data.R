library(mgcv); library(tidyverse); library(lubridate); library(visreg); library(RcppRoll)

source('R/utils/metutils.R')
#--- Load data
ef <- read_csv("data/cleaned/clean_ban_dados_2004_2016_v4.csv", col_types = cols(
  UmidSol_80cm = col_double(),
  UmidSol_220cm = col_double(),
  UmidSol_293cm = col_double()
) )

era5 <- readRDS("data/ERA5/ERA5_multivar_2000_2017_v2.rds")

ef <- ef %>% mutate(UmidSol_80cm= as.numeric(UmidSol_80cm),
                    UmidSol_220cm= as.numeric(UmidSol_220cm),
                    UmidSol_293cm= as.numeric(UmidSol_293cm))

# see which variables i need to extract from this file:
# gpp <- read_csv("data/BANday_CfluxBF.csv")
# gpp <- gpp %>% mutate(date=as.Date.POSIXct(dateloc, tz= 'UTC', origin= "2002-12-31"))
#
# Insert data from Gabriel - GPP 30min time step

# check times between observed and reanalysis data
ef %>% mutate(hour=hour(date)) %>% group_by(hour) %>% summarize(u=mean(Tar, na.rm=T)) %>%
  ggplot(data=., aes(hour, u))+geom_line()

era5 %>% mutate(hour=hour(date)) %>% group_by(hour) %>% summarize(u=mean(ssrd, na.rm=T)) %>%
  ggplot(data=., aes(hour, u))+geom_line()

# subtracting 4 hours from ERA5 to match local eddy flux time
era5 <- era5 %>% mutate(date=date-hours(4))

era5 %>% mutate(hour=hour(date)) %>% group_by(hour) %>% summarize(u=mean(ssrd, na.rm=T)) %>%
  ggplot(data=., aes(hour, u))+geom_line()

# calculate ws from u10 and v10 componets

era5 <- era5 %>% mutate(ws_era5 =sqrt(u10^2+v10^2))

# Aggregate tower (eddy flux) data from 30-min to hourly --------------------
# ERA5 is kept untouched at its native hourly timestamp. Instead of
# interpolating ERA5 down to a 30-min rolling window, we bring the tower data
# UP to the hourly scale by summing the two 30-min records inside each hour
# for accumulation-type variables (precipitation, ET) and averaging them for
# state/rate variables (radiation, temperature, humidity, fluxes, wind,
# soil moisture). Note: 'Dir'/'wd' are wind directions (degrees) - a plain
# arithmetic mean is not circularly correct near the 0/360 wrap, but this
# matches the level of treatment used elsewhere in this script.
sum_vars <- c("Prec.new", "Prec.old", "ET")
mean_vars <- setdiff(
  c("Ki", "Net", "Dir", "Vh", "Pres", "Tar", "q", "H", "co2.new", "h2o.new",
    "LE", "UmidSol_10cm", "UmidSol_20cm", "UmidSol_40cm", "UmidSol_80cm",
    "UmidSol_150cm", "UmidSol_220cm", "UmidSol_293cm", "co2.old", "h2o.old",
    "ws", "wd", "Press", "ta", "UmidEspec", "PressVapor", "Rg_in2"),
  sum_vars
)

# sum(x, na.rm=TRUE) returns 0 (not NA) when BOTH 30-min records in an hour
# are missing - that would silently turn a fully-missing hour of ET/rain
# into a false "0 observed" instead of a real gap. This wrapper preserves
# NA when there is truly nothing to sum that hour.
sum_na_aware <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

ef_hourly <- ef %>%
  mutate(date = floor_date(date, unit = "hour")) %>%
  group_by(date) %>%
  summarise(across(all_of(sum_vars), sum_na_aware),
            across(all_of(mean_vars), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  # recompute date components from the new hourly timestamp (old ones were
  # carried at the 30-min scale and no longer apply after aggregation)
  mutate(Dia = day(date), mês = month(date), ano = year(date),
         doy = yday(date), year = year(date), hour = hour(date),
         hora = format(date, "%H:%M:%S")) %>%
  arrange(date)

# Checking precipitation and solar units
tmp <- era5 %>% dplyr::filter(near(latitude, -9.75))

tmp_int <- tmp %>% mutate(ssr2=ssr/3600, # transforming in W m2
               strd2=strd/3600,
               tp2 = tp*1000) %>%
  select(date, ssr2,strd2,tp2)

tmp_int <- full_join(tmp,tmp_int, by="date") %>% arrange(date)

# Join TRMM  --------------------------------------------------------------
# TRMM is already hourly, so we just guarantee a continuous hourly grid and
# fill missing precipitation records with 0 - no sub-hourly interpolation
# needed anymore.

dat2 <- read_csv("data/TRMM_ban_19980101_20181001.csv")
dat2$date <- parse_date_time(substr(dat2$`system:index`,6,16),"ymd_h")
tmp_hour <- tibble(date = seq(min(dat2$date), max(dat2$date), by="1 hour"))

trmm <- full_join(dat2, tmp_hour, by="date") %>% arrange(date)
trmm <- trmm %>% select(date, trmm1 = precipitation)
trmm <- trmm %>% mutate(trmm1 = if_else(is.na(trmm1), 0, trmm1))

# join flux data and RS data----------------------------------------------------------

tmp_int <- left_join(tmp_int, trmm, by='date')
df <- left_join(tmp_int, ef_hourly, by='date')

#load GRACE
dat <- read_csv("data/GRACE_ban_20000101_20180101.csv")
dat$date <- parse_date_time(substr(dat$`system:index`,1,8),"ymd")
tmp_hour_grc <- tibble(date = seq(min(dat$date), max(dat$date), by="1 hour"))
grc <- full_join(dat, tmp_hour_grc, by="date") %>% arrange(date)
grc <- grc %>%
  mutate(lwe_thickness_csr=approx(x=date, y=lwe_thickness_csr, n=nrow(grc))$y) %>%
  mutate(lwe_thickness_gfz=approx(x=date, y=lwe_thickness_gfz, n=nrow(grc))$y) %>%
  mutate(lwe_thickness_jpl=approx(x=date, y=lwe_thickness_jpl, n=nrow(grc))$y)
grc <- grc %>% select(-starts_with("system:index")) %>%
  select(-starts_with(".geo"))

#join GRACE
df <- left_join(df, grc, by='date')


# Calculate RH and VPD ----------------------------------------------------
# function and paper with parameters in  metutils.R
Rw = 461.5
L = 2.501*10^6
#   if (T = 273.15) return (l = 2.501*10^6) | if (T= 373.15) return (l = 2.257*10^6)
df <- df %>% mutate(rh = get.rh(t2m,d2m))
# convert temperature to celsius before calculate VPD
df <- df %>% mutate(t2m_cel = t2m-273.15)
df <- df %>% mutate(vpd=get.vpd(rh, t2m_cel),
                    vpd=vpd/10) # vpd from mb to Kpa

#calculating vpd from flux tower
# first convert specific humidity (q) in relative humidity (rh_tow)
press = 1013.25 #mb milibar
df <- df %>% mutate(q = q*0.001) # converting g/kg to g/g
df <- df %>% mutate(rh_tow = qair2rh(q, Tar, press),
                    rh_tow = rh_tow*100) # from ratio to %
df <- df %>% mutate(vpd_tow = get.vpd(rh_tow, Tar),
                    vpd_tow = vpd_tow/10) # vpd from mb to Kpa

df <- df %>%
  mutate(hour=hour(date), month=month(date)) %>%
  mutate(diurnal_period = cut(hour, c(0,6,12,19,24),
                              labels = c('night','morn_day','aft_day','night'))) %>%
  mutate(season = cut(month, c(0,1,5,9,12),
                       labels = c('rainy','flood','drought','rainy'))) %>%
  mutate(periods = cut(month, c(0,1,4,7,10,12),
                        labels = c('rising-water','full flooding','falling-water','drainage', 'rising-water')))

#df %>% filter(year==2004) %>% ggplot(aes(date,season))+geom_point()
#df %>% ggplot(aes(month,periods))+geom_point()
# creating derivative variables -------------------------------------------

df <- df %>%
  #filter(year==2005 & month==1) %>%
  mutate(ssr_deriv = signal::sgolayfilt(ssr, p=3, n=13, m=1)) %>%
  mutate(lwe_gfz_deriv_5 = signal::sgolayfilt(lwe_thickness_gfz, p=3, n=5, m = 1)) %>%
  mutate(trmm_deriv_5 = signal::sgolayfilt(lwe_thickness_gfz, p=3, n=5, m = 1))


# Some other conversions

df <- df %>% mutate(ssrd = ssrd/3600, # From Joules to Watts
              ssr = ssr/3600,
              ssr2 = ssr2/3600)

