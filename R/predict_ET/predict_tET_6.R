library(mgcv); library(tidyverse); library(lubridate)
require(parallel)  
nc <- 2   ## cluster size, set for example portability
if (detectCores()>1) { ## no point otherwise
  cl <- makeCluster(nc) 
  ## could also use makeForkCluster, but read warnings first!
} else cl <- NULL

# load the bias corrected ERA5 data
dat_bc <- feather::read_feather(path = 'data/bananal_era5_biasCorrected_2000_2018.feather')

tmp_df <- df 

tmp_df <- tmp_df %>%  
  dplyr::filter ((date >= as.POSIXct('2004-01-01', tz='UTC') & 
                    date <= as.POSIXct('2006-06-30', tz='UTC')) |
                   (date >= as.POSIXct('2006-09-30', tz='UTC') &
                      date <= as.POSIXct('2007-01-31', tz='UTC')) | 
                   (date >= as.POSIXct('2007-09-01', tz='UTC') & 
                      date <= as.POSIXct('2008-05-31', tz='UTC')) | 
                   (date >= as.POSIXct('2008-12-01', tz='UTC') &  
                      date <= as.POSIXct('2009-03-31', tz='UTC')) | 
                   (date >= as.POSIXct('2009-06-01', tz='UTC') &  
                      date <= as.POSIXct('2010-09-30', tz='UTC'))) %>%
  filter(is.na(ET)==F & ET>0) %>% 
  #mutate(ET=ET+0.01) %>% 
  filter(hour>=7 & hour<=17)


# filter data from the tower --------------------------------------

tmp_df<- tmp_df %>% 
  filter(Ki >= 50) %>% 
  filter(Net >= 0) %>% 
  # mutate(ifelse(UmidSol_20cm<=0.22, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_20cm>=0.6, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm<=0.3, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm>=0.58, NA, UmidSol_20cm)) %>% 
  mutate(ssrd = ssrd/3600) %>% 
  mutate(ssr = ssr/3600) %>% 
  mutate(tp = tp*1000)

# temporary filtering because it siminish a lot the number of observations
# tmp_df <- tmp_df %>% filter(is.na(UmidSol_293cm)==F &
#                     is.na(UmidSol_220cm)==F&
#                     is.na(UmidSol_20cm)==F)



# split into test and train subsets
tmp_df %>% select(vpd_tow, Net, Ki, Vh, UmidSol_20cm, UmidSol_80cm, UmidSol_150cm, UmidSol_220cm, UmidSol_293cm, lwe_thickness_csr) %>% 
  cor %>% corrplot::corrplot(method='number')

tmp_train <- tmp_df %>% sample_n(10000)
tmp_test <- anti_join(tmp_df, tmp_train, by="date") %>% # filtering out all rows in the training data
  sample_n(10000)


tET_6_alt <- bam(ET ~ 
               te(vpd_tow,Ki,hour) +
               te(Ki,UmidSol_20cm, UmidSol_293cm),
               # s(Prec.new),  
             data=tmp_train,
             method='REML',
             select=T,
             # rho = 0.7,
             control=ctrl,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_6_alt)

tET_6 <- bam(ET ~ 
               te(vpd_tow,Ki,hour) +
               te(Ki,UmidSol_20cm, UmidSol_293cm),  
             data=tmp_train,
             method='REML',
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_6)
bbmle::AICtab(tET_6, tET_6_alt)
res_tET_6 <- residuals(tET_6)
sqrt(mean(res_tET_6^2))
gam.check(tET_6); abline(0,1,col='red')
nobs(tET_6)

# check scale of ERA5 and tower met vars
df %>% sample_n(10) %>% select(vpd, vpd_tow)
df %>% sample_n(10) %>% select(Ki, ssrd, tp) %>% mutate(ssrd=ssrd/3600, tp = tp*1000)
df %>% sample_n(10) %>% select(UmidSol_20cm, swvl2)
df %>% sample_n(10) %>% select(UmidSol_293cm, swvl4)
df %>% filter(is.na(Prec.new)==F) %>% sample_n(200) %>% select(Prec.new, tp)



dat_pred <- dat_bc %>% 
  mutate(hour = lubridate::hour(date)) %>% 
  select(date, hour,
         vpd, ssrd, swvl2, swvl4) %>% 
  rename(vpd_tow = vpd, 
         Ki = ssrd, 
         UmidSol_20cm = swvl2, 
         UmidSol_293cm = swvl4)%>%
  mutate(Ki = Ki/3600) %>% 
  mutate(ET_pred_lp = predict(tET_6, newdata = ., type='link'),
         ET_pred_se = predict(tET_6, newdata = ., type='link', se.fit = TRUE)$se.fit)#####


dat_agg <- dat_pred %>% 
  mutate(year=year(date), 
         month = month(date),
         ET_pred_lp = exp(ET_pred_lp),
         ET_pred_p2se = exp(ET_pred_lp + 2*ET_pred_se),
         ET_pred_n2se = exp(ET_pred_lp - 2*ET_pred_se)) %>%
  group_by(year,month) %>% 
  summarize(ET = sum(ET_pred_lp, na.rm=T),
            ET_u = mean(ET_pred_p2se, na.rm=T),
            ET_l = mean(ET_pred_n2se, na.rm=T)) %>%
  ungroup() %>%
  mutate(date = ymd(paste(year,month,15))) %>% 
  filter(year<2018)

dat_agg %>% 
  filter(year==2015) %>% 
  ggplot(data=., aes(date, ET))+
  geom_ribbon(aes(date, ymax=ET_u, ymin=ET_l), lty=0, alpha=0.25)+
  geom_line()

dat_norms <- dat_agg %>% 
  group_by(month) %>% 
  summarize(u_et = mean(ET)) %>% 
  ungroup()


dat_agg %>% 
  left_join(., dat_norms, by='month') %>% 
  mutate(ET_anom = ET-u_et) %>% 
  ggplot(data=., aes(date, ET_anom))+
  geom_line()


dat_agg %>% 
  mutate(year=year(date), 
         doy = yday(date), 
         month = month(date)) %>% 
  group_by(year,month) %>% 
  summarize(tot = sum(ET)) %>% 
  ungroup() %>% 
  filter(year %in% c(2014,2015,2016)) %>% 
  ggplot(data=., aes(month, tot, color=as.factor(year)))+
  geom_line()



df %>% 
  filter(is.na(Ki)==F & 
         is.na(ET)==F & 
         Ki > 50) %>% 
  filter(Ki >= 50) %>% 
  filter(Net >= 0) %>% 
  mutate(ifelse(UmidSol_20cm<=0.22, NA, UmidSol_20cm)) %>% 
  mutate(ifelse(UmidSol_20cm>=0.6, NA, UmidSol_20cm)) %>% 
  mutate(ifelse(UmidSol_80cm<=0.3, NA, UmidSol_20cm)) %>% 
  mutate(ifelse(UmidSol_80cm>=0.58, NA, UmidSol_20cm)) %>% 
  sample_n(1000) %>% 
  # select(vpd_tow, Ki, UmidSol_20cm, UmidSol_293cm, Prec.new)
  select(date, hour, ET,
         vpd, ssrd, swvl2, swvl4) %>% 
  rename(vpd_tow = vpd, 
         Ki = ssrd, 
         UmidSol_20cm = swvl2, 
         UmidSol_293cm = swvl4) %>%
  mutate(Ki = Ki/3600) %>% 
  mutate(ET_pred = predict(tET_6_alt, newdata = ., type='response')) %>% 
  ggplot(data=., aes(ET, ET_pred))+
  geom_point()+
  geom_abline(aes(intercept=0,slope=1),col='red')+
  geom_smooth(method='lm')



predict(tET_6)[1]
predict(tET_6, type='response')[1]
predict(tET_6)[1] %>% exp


pred_df2 <- pred_df %>% 
  select(date,hour,et_pred, ET,LE,periods,doy, year) %>%
  dplyr::filter ((date >= as.POSIXct('2004-01-01', tz='UTC') &
                    date <= as.POSIXct('2006-06-30', tz='UTC')) |
                   (date >= as.POSIXct('2006-09-30', tz='UTC') &
                      date <= as.POSIXct('2007-01-31', tz='UTC')) |
                   (date >= as.POSIXct('2007-09-01', tz='UTC') &
                      date <= as.POSIXct('2008-05-31', tz='UTC')) |
                   (date >= as.POSIXct('2008-12-01', tz='UTC') &
                      date <= as.POSIXct('2009-03-31', tz='UTC')) |
                   (date >= as.POSIXct('2009-06-01', tz='UTC') &
                      date <= as.POSIXct('2010-09-30', tz='UTC'))) %>%
  filter(hour>=6 & hour<=18) %>%
  filter(is.na(ET)==F & ET> 0) %>% 
  group_by(year, doy,periods) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T),
            ET_m = sum(ET, na.rm =T),
            LE = mean(LE, na.rm=T),
            nobs = n()) %>%
  ungroup() %>% 
  mutate(date = parse_date_time(paste(year,doy), 'yj'))


pred_df2 %>% 
  ggplot(data=.,aes(x=ET_m, y=tot_pred_et)) + geom_point()+
  ylab("ET predicted")+ xlab("ET measured")+
  theme_bw()+
  facet_wrap(~periods, scales = "fixed")+
  geom_hex()+
  scale_fill_viridis_c()+
  geom_abline(aes(intercept=0, slope=1),col="red")+
  theme(text = element_text(size=14))
pred_df2 %>% 
  ggplot(data=.,aes(date, ET_m)) + 
  geom_point()+geom_point(aes(date, tot_pred_et, col='red'))

