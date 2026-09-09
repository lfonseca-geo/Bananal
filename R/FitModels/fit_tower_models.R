library(mgcv); 
library(mgcViz); 
library(tidyverse); 
library(lubridate); 
source('R/Pre_proc_data.R')

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
nobsgodET <- count(tmp_df)

## Percentual de dados preenchidos

tot_df <- df %>%  
  dplyr::filter ((date >= as.POSIXct('2004-01-01', tz='UTC') & 
                  date <= as.POSIXct('2010-09-30', tz='UTC'))) %>%
  #filter(is.na(ET)==F & ET>0) %>% 
  #mutate(ET=ET+0.01) %>% 
  filter(hour>=7 & hour<=17)
nobstot <- count(tot_df)

nobsgodET/ nobstot * 100
# filter data from the tower --------------------------------------

tmp_df<- tmp_df %>% 
  filter(Ki > 0) %>%
  filter(Net > 0) %>%
  mutate(UmidSol_20cm = ifelse(UmidSol_20cm<=0.22, NA, UmidSol_20cm)) %>%
  mutate(UmidSol_20cm = ifelse(UmidSol_20cm>=0.6, NA, UmidSol_20cm)) %>%
  mutate(UmidSol_80cm = ifelse(UmidSol_80cm<=0.3, NA, UmidSol_80cm)) %>%
  mutate(UmidSol_80cm = ifelse(UmidSol_80cm>=0.58, NA, UmidSol_80cm)) %>%
  mutate(UmidSol_220cm = ifelse(UmidSol_220cm>= 0.65, NA, UmidSol_220cm)) %>% 
  mutate(UmidSol_293cm = ifelse(UmidSol_293cm>=0.45, NA, UmidSol_293cm))
# temporary filtering because it siminish a lot the number of observations
tmp_df <- tmp_df %>% filter(is.na(UmidSol_293cm)==F &
                    is.na(UmidSol_220cm)==F&
                    is.na(UmidSol_20cm)==F)

tmp_df <- tmp_df %>% rename(TWS=lwe_thickness_csr)

# split into test and train subsets
tmp_df %>% select(vpd_tow, Net, Ki, Vh, Tar, UmidSol_20cm, UmidSol_80cm, UmidSol_150cm, UmidSol_220cm, UmidSol_293cm, lwe_thickness_csr) %>% 
  cor %>% corrplot::corrplot(method='number', type = 'upper')

tmp_df %>% select(UmidSol_20cm, UmidSol_80cm, UmidSol_150cm, UmidSol_220cm, UmidSol_293cm, swvl1, swvl2, swvl3, swvl4, TWS) %>% 
  cor %>% corrplot::corrplot(method='number', type = 'upper')

png("fig\\corrplot_soil.png", units="in", width=10, height=9, res=200, pointsize = 16)

dev.off()


tmp_train <- tmp_df %>% sample_n(10000)
tmp_test <- anti_join(tmp_df, tmp_train, by="date") %>% # filtering out all rows in the training data
  sample_n(10000)


tET_1 <- bam(ET ~ 
               s(vpd_tow,hour) +
               s(Net,UmidSol_80cm, UmidSol_20cm),
               # scale(d2m) + 
               # scale(strd) + 
               # scale(swvl3) + # removed because it's too correlated with swvl3 & 4
               # scale(swvl4) + # remove swvl4 because it's correlated with grace
             # s(lwe_thickness_csr),
             data=tmp_train,
             method = 'REML',
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_1)
AIC(tET_1)
res_tET_1 <- residuals(tET_1)
sqrt(mean(res_tET_1^2))
nobs(tET_1)

tET_2 <- bam(ET ~ 
               te(vpd_tow,Net,hour) +
               te(Net,UmidSol_20cm, UmidSol_80cm),
             data=tmp_train,
             method='REML',
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_2)
AIC(tET_2)
res_tET_2 <- residuals(tET_2)
sqrt(mean(res_tET_2^2))
nobs(tET_2)

tET_3 <- bam(ET ~ 
               te(vpd_tow,Ki,hour) +
               te(Ki,UmidSol_20cm, UmidSol_80cm),
             data=tmp_train,
             method='REML',
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_3)
AIC(tET_3)
res_tET_3 <- residuals(tET_3)
sqrt(mean(res_tET_3^2))
gam.check(tET_3); abline(0,1,col='red')
cor(tmp_test$ET, predict(tET_3, newdata=tmp_test, type='response'))**2
nobs(tET_3)

tET_4 <- bam(ET ~ 
               te(vpd_tow,Ki,hour) +
               te(Tar,UmidSol_20cm, UmidSol_80cm),
             data=tmp_train,
             method='REML',
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_4)
AIC(tET_4)
res_tET_4 <- residuals(tET_4)
sqrt(mean(res_tET_4^2))
gam.check(tET_4); abline(0,1,col='red')
nobs(tET_4)

cor(tmp_test$ET, predict(tET_6, newdata=tmp_test, type='response'))**2


tET_5 <- bam(ET ~ 
               te(vpd_tow,Ki,hour) +
               te(Ki,UmidSol_20cm, UmidSol_80cm)+
               te(Tar),
             data=tmp_train,
             method='REML',
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_5)
AIC(tET_5)
res_tET_5 <- residuals(tET_5)
sqrt(mean(res_tET_5^2))
gam.check(tET_5); abline(0,1,col='red')
nobs(tET_5)

tET_6 <- bam(ET ~ 
               te(vpd_tow,Ki,hour) +
               te(Ki,UmidSol_20cm, UmidSol_293cm),
             data=tmp_train,
             method='REML',
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_6)
AIC(tET_6)
res_tET_6 <- residuals(tET_6)
sqrt(mean(res_tET_6^2))
gam.check(tET_6); abline(0,1,col='red')
nobs(tET_6)

df %>% sample_n(10) %>% select(vpd, vpd_tow)
df %>% sample_n(10) %>% select(Ki, ssrd) %>% mutate(ssrd=ssrd/3600)

### we are not using the 220 cm because there are few observations
tET_7 <- bam(ET ~ 
               te(vpd_tow,Ki,hour) +
               te(Ki,UmidSol_20cm, UmidSol_220cm),
             data=tmp_train,
             method='REML',
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(tET_7)
AIC(tET_7)
res_tET_7 <- residuals(tET_7)
sqrt(mean(res_tET_7^2))
gam.check(tET_7); abline(0,1,col='red')
nobs(tET_7)

df %>% 
  #sample_n(10) %>% 
  # select(vpd_tow, Ki, UmidSol_20cm, UmidSol_293cm, Prec.new)
  select(date, hour,
         vpd, ssrd, swvl2, swvl4, tp) %>% 
  rename(vpd_tow = vpd, 
         Ki = ssrd, 
         UmidSol_20cm = swvl2, 
         UmidSol_293cm = swvl4, 
         Prec.new = tp) %>% 
  mutate(ET_pred = predict(tET_6, newdata = ., type='response'))


pred_df_tET_6 <- df %>% 
  mutate(hour = hour(date), month=month(date)) %>% 
  filter(date>= as.POSIXct("2004-01-01") & 
          date < as.POSIXct("2010-01-01")) %>% 
  mutate(et_pred = predict(tET_6, newdata=., type='response'))

# Plot Predicetd ET  --------------------------------------------
# Annual variation
pred_df_tET_6 %>% 
  #filter(date>= as.POSIXct("2016-01-01") &
  #        date < as.POSIXct("2016-12-31")) %>%
  #filter(year==2004) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T),
            ET_m = sum(ET, na.rm =T)) %>%
  ungroup() %>% 
  mutate(date = parse_date_time(paste(year,month,15), 'ymd')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+
  geom_line()+
  geom_line(aes(date, ET_m,col='ET'))+
  xlab(" ") + ylab(expression(paste("Predicted ET (mm",., month^-1,")")))+
  ylim(50,170)+
  scale_color_manual(name = ' ',values=c('ET'="blue"))+
  #scale_x_datetime(breaks = '3 months')+
  theme_bw()

# Hourly variation
pred_df_tET_6 %>% 
  mutate(year=year(date), month=month(date),hour=hour(date)) %>% 
  group_by(hour) %>% 
  summarize(tot_pred_et = mean(et_pred, na.rm=T),
            ET_m = mean(ET, na.rm =T)) %>%
  ungroup() %>% 
  ggplot(data=., aes(hour, tot_pred_et, col='Predicted ET'))+
  geom_line()+
  geom_line(aes(hour, ET_m,col='ET'))+
  theme_bw()+
  xlab("Time of day") + ylab(expression(paste("ET (mm",., hour^-1,")")))+
  scale_color_manual(name = ' ',values=c('Predicted ET'='black','ET'="red"))+
  theme(text = element_text(size=14))

# Hour variation per month

pred_df_tET_6 %>% 
  filter(hour>=6 & hour<=18) %>% 
  group_by(month,hour) %>% summarize(u=mean(et_pred,na.rm=T)) %>% 
  ggplot(data=., aes(month, u, color=as.factor(hour)))+geom_line()

# Annual ET
pred_df_tET_6 %>% 
  group_by(year) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T), 
            nobs = sum(is.na(et_pred)==F)) %>% 
  ungroup() %>% 
  #filter(year>=2004 & year<=2018) %>% 
  # mutate(date=ymd(paste(year,month,15), tz='UTC')) %>% 
  ggplot(data=., aes(year, tot_pred_et))+geom_line()



library(mgcViz)
plotSlice(sm(getViz(tET_7),1), 
          fix=list('hour'=c(10:12)))+
  l_fitRaster()+
  l_fitContour()+
  l_points()+
  scale_fill_gradient2()

plotSlice(sm(getViz(tET_7),2), 
          fix=list('Ki'=c(250,700,1000)))+
  l_fitRaster()+
  l_fitContour()+
  l_points()+
  scale_fill_gradient2()

plot(sm(getViz(tET_7),3))+
  l_points()+
  l_rug()

df %>% 
  select(ET, Net, Ki,Tar, vpd_tow,Vh,
         Prec.new,
         UmidSol_20cm,
         UmidSol_150cm
         # UmidSol_293cm
         # UmidSol_80cm
         ) %>% 
  na.omit()

mvnfast::rmvn(100)
d <- 5
mu <- 1:5
junk <- matrix(rnorm(d^2), d, d)
mcov <- tcrossprod(junk, junk)
rmvn(4, 1:d, mcov)


df %>% 
  filter(year(date) == 2004) %>% 
  summarise(across(c(vpd_tow, Ki, UmidSol_20cm, UmidSol_293cm), ~sum(!is.na(.))))

summary(pred_df_tET_6$et_pred[year(pred_df_tET_6$date) == 2005])
