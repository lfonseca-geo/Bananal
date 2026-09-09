# checking correlations between ERA5 and tower data -----------------------

# run pre processed data
source('R/utils/metutils.R')
source('R/Pre_proc_data.R')

# ploting tower data x ERA5

df <- df %>% mutate(UmidSol_20cm = ifelse(UmidSol_20cm<=0.22, NA, UmidSol_20cm)) %>%
  mutate(UmidSol_20cm = ifelse(UmidSol_20cm>=0.6, NA, UmidSol_20cm)) %>%
  mutate(UmidSol_80cm = ifelse(UmidSol_80cm<=0.3, NA, UmidSol_80cm)) %>%
  mutate(UmidSol_80cm = ifelse(UmidSol_80cm>=0.58, NA, UmidSol_80cm)) %>%
  mutate(UmidSol_293cm = ifelse(UmidSol_293cm>=0.45, NA, UmidSol_293cm))

## Soil moisture 20 cm and swvl2 (0 - 0.28 cm)
swvl2_20cm <- df %>% 
  select(date, swvl2, UmidSol_20cm) %>% 
  ggplot(data=., aes(date, swvl2, col= 'SM ERA5 layer2'))+geom_line()+
  geom_line(aes(date, UmidSol_20cm, col='SM tower 20cm'))+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('SM ERA5 layer2'="black", 'SM tower 20cm'= 'red'))+
  theme_bw()+
  scale_x_datetime(breaks = '1 year', date_labels = "%Y")+
  xlab(' ')+
  ylab(expression(paste("Soil moisture (SM) (",' ', m^3*m^-3,")")))+
  theme(legend.justification=c(1,0.2),
        legend.position=c(0.29, 0.9),
        legend.background = element_blank(),
        legend.direction = ("horizontal"),
        axis.text=element_text(size=12),
        axis.text.x=element_blank(),
        legend.key = element_blank()) +
  theme(legend.key.width = unit(2, 'lines'),
        legend.spacing.y = unit(0.0000000005, "cm"))+
  theme(panel.grid.major = element_blank())


## Soil moisture 80 cm and swvl3 (between 0.28 and 100 cm)
swvl3_80cm <- df %>% 
  select(date, swvl3, UmidSol_80cm) %>% 
  ggplot(data=., aes(date, swvl3, col='SM ERA5 layer3'))+geom_line()+
  geom_line(aes(date, UmidSol_80cm, col ='SM tower 80cm'))+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('SM ERA5 layer3'="black", 'SM tower 80cm'= 'blue'))+
  ylab(expression(paste("Soil moisture (SM) (",' ', m^3*m^-3,")")))+
  theme_bw()+
  scale_x_datetime(breaks = '1 year', date_labels = "%Y")+
  xlab(' ')+
  theme(legend.justification=c(1,0.2),
        legend.position=c(0.29, 0.9),
        legend.background = element_blank(),
        legend.direction = ("horizontal"),
        axis.text=element_text(size=12),
        axis.text.x=element_blank(),
        legend.key = element_blank()) +
  theme(legend.key.width = unit(2, 'lines'),
        legend.spacing.y = unit(0.0000000005, "cm"))+
  theme(panel.grid.major = element_blank())

df %>% 
  select(date,swvl3, UmidSol_80cm) %>% 
  ggplot(data=.,aes(x=swvl3, y=UmidSol_80cm)) + geom_point()+
  geom_abline(aes(intercept=0, slope=1),col="red")
  ylab("Soil moisture")+xlab("VPD ERA5")
geom_smooth(method=lm, color="blue")+
  facet_wrap(~month)

## Soil moisture 150 cm and swvl4 (between 100 and 189 cm) - 
# ERA5 data represent clayey layers - up to 150 cm

df %>% 
  select(date, swvl4, UmidSol_150cm) %>% 
  ggplot(data=., aes(date, swvl4, col='swvl4'))+geom_line()+
  geom_line(aes(date, UmidSol_150cm, col='UmidSol_150cm'))+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('swvl4'="black", 'UmidSol_150cm'= 'blue'))


df %>% 
  select(date, swvl4, UmidSol_220cm) %>% 
  ggplot(data=., aes(date, swvl4, col='swvl4'))+geom_line()+
  geom_line(aes(date, UmidSol_220cm, col='UmidSol_220cm'))+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('swvl4'="black", 'UmidSol_220cm'= 'blue'))


swvl4_293cm <- df %>% 
  select(date, swvl4, UmidSol_293cm, UmidSol_220cm) %>% 
  ggplot(data=., aes(date, swvl4, col='SM ERA5 layer 4'))+geom_line()+
  geom_line(aes(date, UmidSol_293cm, col='SM tower 293 cm'))+
  geom_line(aes(date, UmidSol_220cm, col = 'SM tower 220 cm'))+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('SM ERA5 layer 4'="black", 'SM tower 293 cm'= 'darkgoldenrod', 'SM tower 220 cm' = 'purple'))+
  theme_bw()+
  scale_x_datetime(breaks = '1 year', date_labels = "%Y")+
  xlab(' ')+
  ylab(expression(paste("Soil moisture (SM) (",' ', m^3*m^-3,")")))+
  theme(legend.justification=c(1,0.2),
        legend.position=c(0.2, 0.3),
        legend.background = element_blank(),
        legend.direction = ("vertical"),
        axis.text=element_text(size=12),
        legend.key = element_blank()) +
  theme(legend.key.width = unit(2, 'lines'),
        legend.spacing.y = unit(0.0000000005, "cm"))+
  theme(panel.grid.major = element_blank())

library(ggpubr)
ggarrange(swvl2_20cm, swvl3_80cm, swvl4_293cm, nrow = 3)


tiff("fig\\sm_era5_tower.tiff", units="in", width=11, height=9, res=300)

dev.off()

df %>% 
  select(date, swvl4, lwe_thickness_csr) %>% 
  ggplot(data=., aes(date, swvl4, col='SM ERA5 layer 4'))+geom_point()+
  geom_line(aes(date, lwe_thickness_csr/70, col='GRACE'))+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('SM ERA5 layer 4'="black", 'GRACE'= 'darkgoldenrod'))+
  theme_bw()+
  theme(legend.justification=c(1,0.2),
        legend.position=c(1, 0.5),
        legend.background = element_blank(),
        axis.text=element_text(size=12),
        legend.key = element_blank()) +
  theme(legend.key.width = unit(1, 'lines'),
        legend.spacing.y = unit(0.0000000005, "cm"))+
  theme(panel.grid.major = element_blank())


# check correlation of soil data ERA5 predictors
df %>% 
  sample_n(10000) %>% 
  rename(GRACE = lwe_thickness_csr) %>% 
  select(UmidSol_20cm,UmidSol_40cm,UmidSol_80cm,UmidSol_150cm,UmidSol_220cm,UmidSol_293cm, swvl1, swvl2, swvl3, swvl4, GRACE) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(., method='number', type = 'upper')


# comparing ws_era5 data with ws from the flux tower - low correlation
df %>% ggplot(data=.,aes(date, Vh))+geom_line()+
  geom_line(aes(date, ws_era5), col = 'blue')

df %>% 
  sample_n(10000) %>% 
  select(Vh, ws_era5) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(., method='number')


##checking Le and H terms seasonal
# H is not good from 2008
# Le has some outliers 

df <- df %>% mutate(H = ifelse(H <= 0, 0, H), # can I do this?!
                    Le = ifelse(Le<=0,0, Le))

df %>% 
  select(date, H, Le, Tar) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year, month) %>% 
  summarize(H = mean(H, na.rm=T),
            Le = mean(Le, na.rm=T),
            Tar = mean(Tar, na.rm=T),
            nobs = n(),
            nobs_na = sum(is.na(Le)),
            nobs_naH = sum(is.na(H))) %>% 
  filter(year>=2004 & year<=2017) %>% 
  mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
  ggplot(data=., aes(date, H, col='H'))+geom_line()+geom_point()+
  geom_line(aes(date, Le, col='Le'))+ geom_line()+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('H'="black", 'Le'= 'blue'))

df %>% 
  select(date, slhf, sshf, H, Le) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year, month) %>% 
  summarize(slhf = mean(slhf, na.rm=T)/3600, # transform J/m2 em w/m2
            sshf = mean(sshf, na.rm=T)/3600, # transform J/m2 em w/m2
            H = mean(H, na.rm=T),
            Le = mean(Le, na.rm=T)) %>% 
  filter(year>=2004 & year<=2017) %>% 
  mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
  ggplot(data=., aes(date, sshf, col='sshf'))+geom_line()+geom_point()+
  #geom_line(aes(date, sshf, col='sshf'))+ geom_line()+
  geom_line(aes(date, H, col='H'))+ geom_line()+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('sshf'="black", 'H'= 'blue'))

## latent heat super correlated, but why era5 is negative?
## surface sensible heat not so well correlated
df %>% 
  sample_n(10000) %>% 
  select(slhf,sshf, Le, H) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(., method='number')

## comparison between temperature - good!
df1 <- df %>% 
  select(date, H, LE, Tar, t2m_cel, vpd, vpd_tow) %>% 
  filter((date >= as.POSIXct('2004-01-01', tz='UTC') & 
            date <= as.POSIXct('2007-01-31', tz='UTC')) | 
           (date >= as.POSIXct('2007-08-01', tz='UTC') & 
              date <= as.POSIXct('2008-05-31', tz='UTC')) | 
           (date >= as.POSIXct('2008-11-01', tz='UTC') &  
              date <= as.POSIXct('2011-10-30', tz='UTC'))| 
           (date >= as.POSIXct('2012-01-01', tz='UTC') &  
              date <= as.POSIXct('2012-05-31', tz='UTC'))) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year, month) %>% 
  summarize(H = mean(H, na.rm=T),
            Le = mean(LE, na.rm=T),
            Tar = mean(Tar, na.rm=T),
            t2m_cel = mean(t2m_cel, na.rm=T),
            vpd = mean(vpd, na.rm=T),
            vpd_tow = mean(vpd_tow, na.rm = T)) %>% 
  #filter(year>=2004 & year<=2017) %>% 
  mutate(date=parse_date_time(paste(year,month,15),'ymd'))
  ggplot(data=., aes(date, Tar))+geom_line()+geom_point()+
  geom_line(aes(date, t2m_cel, col='red'))

  
lm = lm(df1$t2m_cel~df1$Tar, na.action=na.exclude)
summary(lm)

ggplot(data=df1,aes(x=t2m_cel, y=Tar)) + geom_point()+
  #geom_abline(aes(intercept=1, slope=1),col="red")+
  geom_smooth(method=lm,se=FALSE, col='red')+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

df %>% 
  sample_n(10000) %>% 
  select(Tar, t2m_cel) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(., method='number')

## Bowen ratio - **filter outliers

df <- df %>% mutate(bow= H/Le, bow_era5 = sshf/slhf)
df %>% 
  select(date, bow, bow_era5, ET, Le) %>% 
  mutate(year=year(date), month=month(date), day=day(date)) %>% 
  filter(year==2004) %>% 
  ggplot(data=., aes(date, bow))+geom_point()
  geom_line(aes(date, Le),col='red')

df %>% 
  sample_n(100) %>% 
  select(bow, bow_era5) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(., method='number')

##checking calculated relative humidity
df %>% 
  select(date, rh,rh_tow) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year, month) %>% 
  summarize(rh = mean(rh, na.rm=T),
            rh_tow = mean(rh_tow, na.rm=T)) %>% 
  #filter(year>=2004 & year<=2006) %>% 
  mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
  ggplot(data=., aes(date, rh, col='rh'))+geom_line()+
  geom_line(aes(date, rh_tow,col='rh_tow'))+
  theme(panel.grid.major = element_blank())+
  scale_color_manual(name = ' ',values=c('rh'="black", 'rh_tow'= 'red'))

##checking VPD era5 and flux tower

df %>% 
  select(date, vpd_tow,vpd) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year, month) %>% 
  summarize(vpd_tow = mean(vpd_tow, na.rm=T),
            vpd = mean(vpd, na.rm=T)) %>% 
  #filter(year>=2004 & year<=2006) %>% 
  mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
  ggplot(data=., aes(date, vpd,col='VPD ERA5'))+geom_line()+
  geom_line(aes(date, vpd_tow, col='VPD Flux Tower'))+
  scale_color_manual(name = ' ',values=c('VPD Flux Tower'="black", 'VPD ERA5'= 'red'))+
  theme_bw()

# correlation VPD - good correlation!

df %>% 
  sample_n(10000) %>% 
  select(vpd, vpd_tow) %>% 
  na.omit() %>% 
  cor() %>% 
  corrplot::corrplot(., method='number')


# correlation plots -------------------------------------------------------

library(ggpubr);library(ggpmisc)
fig_vpd_daily <- df %>% 
  select(date,vpd, vpd_tow) %>% 
  #sample_n(10000) %>% 
  mutate(year=year(date), doy = yday(date)) %>% 
  group_by(year, doy) %>% 
  summarize(vpd=mean(vpd, na.rm=T),
           vpd_tow= mean(vpd_tow, na.rm=T))%>% 
  #mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
  ggplot(data=.,aes(x=vpd, y=vpd_tow)) + geom_point()+
  geom_hex()+
  geom_abline(aes(intercept=0, slope=1),col="red")+
  ylab("Vapour pressure deficit")+xlab("VPD")+
  theme_bw()+
  theme(text = element_text(size=16))+
  stat_fit_glance(method = "lm",
                             method.args = list(formula = y ~ x),
                             aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                                 stat(r.squared), stat(p.value))),
                             parse = TRUE)

  

fig_net_daily <-  df %>%
  filter(is.na(Net)==F & Net>0) %>% 
  # mutate(ifelse(UmidSol_20cm<=0.22, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_20cm>=0.6, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm<=0.3, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm>=0.58, NA, UmidSol_20cm)) %>% 
  sample_n(10000) %>% 
  filter(hour>=6 & hour <= 18) %>% 
  group_by(year, doy) %>%
  summarize(Net=mean(Net, na.rm=T),
            ssr= mean(ssr, na.rm=T))%>%
  ggplot(data=.,aes(x=ssr, y=Net)) + geom_point()+
  geom_hex()+
  geom_abline(aes(intercept=0, slope=1),col="red")+
  theme_bw()+
  theme(text = element_text(size=16))+
  ylab("Net radiation")+xlab("SSR")+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                      stat(r.squared), stat(p.value))),
                  parse = TRUE)

fig_Ki_daily <-  df %>%
  filter(is.na(Ki)==F & Ki>0) %>% 
  # mutate(ifelse(UmidSol_20cm<=0.22, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_20cm>=0.6, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm<=0.3, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm>=0.58, NA, UmidSol_20cm)) %>% 
  #sample_n(10000) %>% 
  #filter(hour>=6 & hour <= 18) %>% 
  group_by(year, doy) %>%
  summarize(Ki=mean(Ki, na.rm=T),
            ssrd= mean(ssrd, na.rm=T))%>%
  ggplot(data=.,aes(x=ssrd, y=Ki)) + geom_point()+
  geom_hex()+
  geom_abline(aes(intercept=0, slope=1),col="red")+
  ylab("Global radiation")+xlab("SSRD")+
  theme_bw()+
  theme(text = element_text(size=16))+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                      stat(r.squared), stat(p.value))),
                  parse = TRUE)

fig_Ta_daily <- df %>% 
  select(date,Tar, t2m_cel) %>% 
  #sample_n(10000) %>% 
  filter(is.na(Tar)==F & Tar <= 40) %>% 
  mutate(year=year(date), month=month(date), doy = yday(date), hour=hour(date)) %>% 
  #filter(hour>=6 & hour <= 18) %>% 
  group_by(year,doy) %>% 
  summarize(Tar = mean(Tar, na.rm=T),
            t2m_cel = mean(t2m_cel, na.rm=T)) %>% 
  ggplot(data=.,aes(x=t2m_cel, y=Tar)) + geom_point()+
  geom_hex()+
  geom_abline(aes(intercept=0, slope=1),col="red")+
  ylab("Temperature")+xlab("T2m")+
  theme_bw()+
  theme(text = element_text(size=16))+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                      stat(r.squared), stat(p.value))),
                  parse = TRUE)

fig_tp_prec <- df %>%
  select(date, Prec.new, tp2, trmm1) %>% 
  mutate(year=year(date), doy = yday(date), month=month(date)) %>% 
  filter(is.na(Prec.new) ==F & is.na(tp2)==F) %>% 
  # mutate(ifelse(UmidSol_20cm<=0.22, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_20cm>=0.6, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm<=0.3, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm>=0.58, NA, UmidSol_20cm)) %>% 
  #sample_n(10000) %>% 
  #filter(hour>=6 & hour <= 18) %>% 
  group_by(year, doy) %>%
  summarize(Prec.new=sum(Prec.new, na.rm=T),
            trmm1= sum(trmm1, na.rm=T),
            tp = sum(tp2))%>%
  filter(tp<50) %>% 
  ggplot(data=.,aes(x=tp, y=Prec.new)) + geom_point()+
  geom_hex()+
  geom_abline(aes(intercept=0, slope=1),col="red")+
  ylab("Rainfall Tower")+xlab("TP")+
  theme_bw()+
  theme(text = element_text(size=16))+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                      stat(r.squared), stat(p.value))),
                  parse = TRUE)

fig_trmm_precip <-df %>%
  select(date, Prec.new, tp2, trmm1) %>% 
  mutate(year=year(date), doy = yday(date), month=month(date)) %>% 
  filter(is.na(Prec.new) ==F & is.na(tp2)==F & is.na(trmm1)==F) %>% 
  # mutate(ifelse(UmidSol_20cm<=0.22, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_20cm>=0.6, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm<=0.3, NA, UmidSol_20cm)) %>% 
  # mutate(ifelse(UmidSol_80cm>=0.58, NA, UmidSol_20cm)) %>% 
  #sample_n(10000) %>% 
  #filter(hour>=6 & hour <= 18) %>% 
  group_by(year, doy) %>%
  summarize(Prec.new=sum(Prec.new, na.rm=T),
            trmm1= sum(trmm1, na.rm=T),
            tp2 = sum(tp2))%>%
  #filter(trmm1>0 & Prec.new>0) %>% 
  filter(trmm1 <50 & Prec.new<50) %>% 
  ggplot(data=.,aes(x=trmm1, y=Prec.new)) + geom_point()+
  geom_hex()+
  geom_abline(aes(intercept=0, slope=1),col="red")+
  ylab("Rainfall Tower")+xlab("TRMMM")+
  theme_bw()+
  theme(text = element_text(size=16))+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                      stat(r.squared), stat(p.value))),
                  parse = TRUE)


ggarrange(fig_Ta_daily, fig_vpd_daily, fig_Ki_daily, fig_net_daily, fig_tp_prec, fig_trmm_precip, nrow = 3, ncol = 2, legend = 'none')


tiff("fig/correlation_plot_daily_tow_era.tiff", units="in", width=11, height=9, res=300)

dev.off()


# checking precipitation
  df %>% 
    select(date, Prec.new, tp2, trmm1) %>% 
    mutate(year=year(date), month=month(date)) %>% 
    group_by(year, month) %>% 
    summarize(Prec.new = sum(Prec.new, na.rm=T),
              tp2 = sum(tp2, na.rm=T),
              trmm1 = sum(trmm1, na.rm=T)) %>% 
    #filter(year>=2004 & year<=2006) %>% 
    mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
    ggplot(data=., aes(date, Prec.new,col='Prec.new'))+geom_line()+
    geom_line(aes(date, tp2, col='tp2'))+
    geom_line(aes(date, trmm1, col = 'trmm'))+
    scale_color_manual(name = ' ',values=c('Prec.new'="black", 'tp2'= 'red', 'trmm' = 'blue'))+
    theme_bw()
  
  ev.off()
  # checking Net radiation
  df %>% 
    select(date, ssr, Net) %>% 
    mutate(year=year(date), month=month(date),hour = hour(date)) %>%
    filter(hour>=6 & hour <= 18) %>% 
    group_by(year, month) %>% 
    summarize(ssr = mean(ssr2, na.rm=T),
              Net = mean(Net, na.rm=T)) %>% 
    filter(year>=2004 & year<=2006) %>% 
    mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
    ggplot(data=., aes(date, ssr,col='ssr2'))+geom_line()+
    geom_line(aes(date, Net, col='Net'))+
    scale_color_manual(name = ' ',values=c('ssr2'="black", 'Net'= 'red'))+
    theme_bw()
  
  df %>% mutate(year=year(date), month=month(date),hour = hour(date)) %>%
    #filter(hour>=6 & hour <= 18) %>% 
    ggplot(aes(date,ssr))+geom_point()
  # correlation VPD - good correlation!
  
  df %>% 
    sample_n(10000) %>% 
    select(ssr, Net) %>% 
    na.omit() %>% 
    cor() %>% 
    corrplot::corrplot(., method='number')
  

# scaterplot tese --------------------------------------------------------

  
  df %>% 
    select(date,Tar, t2m_cel) %>% 
    filter(is.na(Tar)==F) %>% 
    mutate(year=year(date), month=month(date)) %>% 
    group_by(year,month) %>% 
    summarize(Tar = mean(Tar, na.rm=T),
              t2m_cel = mean(t2m_cel, na.rm=T)) %>% 
    #filter(year>=2004 & year<=2017) %>% 
    mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
    ggplot(data=.,aes(x=Tar, y=t2m_cel)) + geom_point()+
    geom_smooth(method='lm', color="red",se=F)+
    #geom_abline(aes(intercept=0, slope=1),col="red")+
    ylab("ERA5 Temperature")+ xlab("Tower temperature")+
    theme_bw()+
    theme(text = element_text(size=22))+
    stat_fit_glance(method = "lm",
                    method.args = list(formula = y ~ x), size = 9,
                    aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                        stat(r.squared), stat(p.value))),
                    parse = TRUE)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  
  
  tiff("fig/correlation_temperature.tiff", units="in", width=11, height=9, res=300)
  
  dev.off()
  
  
  
  df %>% 
    select(date,vpd, vpd_tow) %>% 
    filter(is.na(vpd_tow)==F) %>% 
    mutate(year=year(date), month=month(date)) %>% 
    group_by(year,month) %>% 
    summarize(vpd = mean(vpd, na.rm=T),
             vpd_tow = mean(vpd_tow, na.rm=T)) %>% 
    #filter(year>=2004 & year<=2017) %>% 
    mutate(date=parse_date_time(paste(year,month,15),'ymd')) %>% 
    ggplot(data=.,aes(x=vpd_tow, y=vpd)) + geom_point()+
    geom_smooth(method='lm', color="red",se=F)+
    #geom_abline(aes(intercept=0, slope=1),col="red")+
    ylab("ERA5 Vapour pressure deficit")+xlab("Tower Vapour pressure deficit")+
    theme_bw()+
    theme(text = element_text(size=22))+
    stat_fit_glance(method = "lm",
                    method.args = list(formula = y ~ x), size = 9,
                    aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                        stat(r.squared), stat(p.value))),
                    parse = TRUE)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  
  tiff("fig/correlation_vpd.tiff", units="in", width=11, height=9, res=300)
  
  dev.off()
  