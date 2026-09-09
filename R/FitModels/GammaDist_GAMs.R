library(mgcv); library(tidyverse); library(lubridate); library(visreg);library(RcppRoll);library(dplyr)
library(mgcViz)
# Run data
source('R/utils/metutils.R')
source('R/Pre_proc_data.R')
# preprocessing 
tmp_df <- df 
#tmp_df %>% filter(periods=='wet') %>% select(month, date)
# creating flooded period - grace defined better the flooded period
tmp_df <- tmp_df %>%
  arrange(date) %>%
  mutate(x_csr = roll_sumr(lwe_thickness_csr, n = 1)) %>%
  mutate(flood_mon = ifelse(x_csr >=0,1,0))

#tmp_df %>% ggplot(aes(date, flood_mon))+geom_line()

# # Creating derivative variables -------------------------------------------
# 
# tmp <- tmp_df %>%
#   #filter(year==2005 & month==1) %>% 
#   mutate(ssr_deriv = signal::sgolayfilt(ssr, p=3, n=13, m=1)) %>% 
#   mutate(lwe_gfz_deriv_5 = signal::sgolayfilt(lwe_thickness_gfz, p=3, n=5, m = 1)) %>%
#   select(date,ssr_deriv,lwe_gfz_deriv_5)
#   #ggplot(data=., aes(hour, ssrd_deriv))+geom_point()

# Join derivative from solar radiation on data.frame

# tmp_df <- left_join(tmp_df, tmp, by=c('date'))

# filtering data to days of good observation density
#first approach
# tmp_df <- tmp_df %>%  
#   dplyr::filter ((date >= as.POSIXct('2004-01-01', tz='UTC') & 
#                     date <= as.POSIXct('2006-06-30', tz='UTC')) |
#                    (date >= as.POSIXct('2006-09-30', tz='UTC') &
#                       date <= as.POSIXct('2007-01-31', tz='UTC')) | 
#                    (date >= as.POSIXct('2007-09-01', tz='UTC') & 
#                       date <= as.POSIXct('2008-05-31', tz='UTC')) | 
#                    (date >= as.POSIXct('2008-12-01', tz='UTC') &  
#                       date <= as.POSIXct('2010-09-30', tz='UTC'))) %>% 
#   mutate(ET=ET+0.01) %>% 
#   filter(is.na(ET)==F) %>% 
#   filter(hour>=7 & hour<=17) 
# 
# 
# 
# tmp_df <- tmp_df %>%  
#   dplyr::filter ((date >= as.POSIXct('2004-01-01', tz='UTC') & 
#                     date <= as.POSIXct('2006-06-30', tz='UTC')) |
#                    (date >= as.POSIXct('2006-09-30', tz='UTC') &
#                       date <= as.POSIXct('2007-01-31', tz='UTC')) | 
#                    (date >= as.POSIXct('2007-09-01', tz='UTC') & 
#                       date <= as.POSIXct('2008-05-31', tz='UTC')) | 
#                    (date >= as.POSIXct('2008-12-01', tz='UTC') &  
#                       date <= as.POSIXct('2010-09-30', tz='UTC'))) %>% 
#   #mutate(ET=ET+0.01) %>% 
#   mutate(ET= ifelse(ET<=0.01, NA, ET)) %>% # maybe it is more realistic than sum 0.01
#   filter(is.na(ET)==F) %>% 
#   filter(hour>=7 & hour<=17) # It is important to filter before doing the model?, the r² decreased

#including more days and using ET+ 0.01 
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
                   # (date >= as.POSIXct('2011-01-01', tz='UTC') &
                   #    date <= as.POSIXct('2011-05-31', tz='UTC'))|
                   # (date >= as.POSIXct('2011-08-01', tz='UTC') &
                   #    date <= as.POSIXct('2011-10-31', tz='UTC'))|
                   # (date >= as.POSIXct('2012-03-01', tz='UTC') &
                   #    date <= as.POSIXct('2012-05-31', tz='UTC'))) %>% 
                   # (date >= as.POSIXct('2015-01-01', tz='UTC') &  
                   #    date <= as.POSIXct('2015-01-31', tz='UTC'))| 
                   # (date >= as.POSIXct('2015-06-01', tz='UTC') &  
                   #    date <= as.POSIXct('2015-06-30', tz='UTC'))) %>% 
  #filter(ET>= 0.01) %>% # maybe it is more realistic than sum 0.01
  filter(is.na(ET)==F & ET>0) %>% 
  #mutate(ET=ET+0.01) %>% 
  filter(hour>=7 & hour<=17)
  # It is important to filter before doing the model?, the r² decreased


#df %>% ggplot(aes(date, swvl2))+geom_line()
# split into test and train subsets
tmp_train <- tmp_df %>% sample_n(10000)
tmp_test <- anti_join(tmp_df, tmp_train, by="date") %>% # filtering out all rows in the training data
  sample_n(10000)
  

#--- Fit models
tmp_train %>% select(pev,d2m,ssr,ssrd,tp,vpd, swvl1, swvl2,swvl3,swvl4,ws_era5,lwe_thickness_csr) %>% 
  cor %>% corrplot::corrplot(method='number', type = 'upper')
mET_1 <- bam(ET ~ 
               s(pev,hour) +
               s(ssr,swvl4)+
               # scale(d2m) + 
               # scale(strd) + 
               s(tp) +  
               s(swvl2) + 
               # scale(swvl3) + # removed because it's too correlated with swvl3 & 4
               # scale(swvl4) + # remove swvl4 because it's correlated with grace
               s(ws_era5),
               # s(lwe_thickness_csr),
              data=tmp_train,
              # rho = 0.7,
              family=Gamma(link='log')) # fit Gamma using a log link function
summary(mET_1)
visreg::visreg2d(mET_1, yvar="pev",xvar="ssr", scale='response')
visreg::visreg(mET_1, xvar="lwe_thickness_csr")
visreg::visreg(mET_1, xvar="pev", by='hour')
gam.check(mET_1); abline(0,1,col='red')

cor(tmp_test$ET, predict(mET_1, newdata=tmp_test, type='response'))**2

AIC(mET_1)
res_mET_1 <- residuals(mET_1)
sqrt(mean(res_mET_1^2))

### trying other combinations
#... radiation with soil moisture - I think its is reasonable

mET_2 <- bam(ET ~ 
               s(vpd, hour, by=periods) +
               s(swvl2, swvl4)+
               # scale(d2m) + 
               # scale(strd) + 
               s(tp),
               #s(ssrd) + 
               # scale(swvl3) + # removed because it's too correlated with swvl3 & 4
               # scale(swvl4) + # remove swvl4 because it's correlated with grace
               #s(ws_era5),
             # s(lwe_thickness_csr),
             data=tmp_train,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(mET_2)
AIC(mET_2)
res_mET_2 <- residuals(mET_2)
sqrt(mean(res_mET_2^2))

visreg::visreg2d(mET_2, yvar="swvl1",xvar="ssr", scale='response') # ET shows flat pattern, always high when soil moisture is high
visreg::visreg(mET_2, xvar="swvl4")
visreg::visreg(mET_2, xvar="pev", by='hour')
gam.check(mET_2); abline(0,1,col='red')

cor(tmp_test$ET, predict(mET_2, newdata=tmp_test, type='response'))**2


pred_df_mET_2 <- df %>% 
  mutate(hour = hour(date), month=month(date)) %>% 
  # filter(date>= as.POSIXct("2005-01-01") & 
  #        date < as.POSIXct("2008-01-01")) %>% 
  mutate(et_pred = predict(mET_2, newdata=., type='response'))

#...pev and radiation - not good

mET_3 <- bam(ET ~ 
               s(pev, ssr, by=flood_mon) +
               s(swvl2)+
               # scale(d2m) + 
               # scale(strd) + 
               s(tp) +  
               #s(ssrd) + 
               # scale(swvl3) + # removed because it's too correlated with swvl3 & 4
               # scale(swvl4) + # remove swvl4 because it's correlated with grace
              s(swvl4),
             data=tmp_train,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(mET_3)
AIC(mET_3)
res_mET_3 <- residuals(mET_3)
sqrt(mean(res_mET_3^2))
visreg::visreg2d(mET_3, xvar="d2m",yvar="swvl1", scale='response')
visreg::visreg(mET_3, xvar="swvl2")
visreg::visreg(mET_3, xvar="pev", by='hour')
gam.check(mET_3); abline(0,1,col='red')

cor(tmp_test$ET, predict(mET_3, newdata=tmp_test, type='response'))**2

# pev and hour 

mET_5 <- bam(ET ~ 
               s(pev,hour) +
               s(ssr,lwe_thickness_csr)+
               # scale(d2m) + 
               # scale(strd) + 
               s(tp) +  
               s(swvl2), 
               # scale(swvl3) + # removed because it's too correlated with swvl3 & 4
               # scale(swvl4) + # remove swvl4 because it's correlated with grace
               #s(ws_era5),
             # s(lwe_thickness_csr),
             data=tmp_train,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function
summary(mET_5)
AIC(mET_5)
res_mET_5 <- residuals(mET_5)
sqrt(mean(res_mET_5^2))


visreg::visreg2d(mET_5, yvar="pev",xvar="ssrd", scale='response')
visreg::visreg(mET_5, xvar="lwe_thickness_csr")
visreg::visreg(mET_5, xvar="pev", by='hour')
gam.check(mET_5); abline(0,1,col='red')

cor(tmp_train$ET, predict(mET_5, newdata=tmp_train, type='response'))**2

pred_df_mET_5 <- df %>% 
  mutate(hour = hour(date), month=month(date)) %>% 
  # filter(date>= as.POSIXct("2005-01-01") & 
  #        date < as.POSIXct("2008-01-01")) %>% 
  mutate(et_pred = predict(mET_5, newdata=., type='response'))
# Scale model across time -------------------------------------------------
# test 3
pred_df <- df %>% select(ET, pev, date, ssr, lwe_thickness_csr, tp, 
                         swvl2, ws_era5, vpd, swvl4) %>%
  mutate(hour=hour(date)) %>% 
  mutate(ET_mET2 = predict(mET_2, newdata=., type='response'))
pred_df$ET %>% summary
pred_df$ET_mET2 %>% summary

pred_df %>% 
  filter(is.na(lwe_thickness_csr)==F) %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  mutate(coarse_date = ymd(paste(year,month,15), tz="UTC")) %>% 
  group_by(coarse_date) %>% 
  summarize(tot_ET = sum(ET_mET2, na.rm=T), 
            tot_ET_obs = sum(ET, na.rm=T), 
            bad_ET_obs = sum(is.na(ET))) %>% 
  ungroup() %>% 
  ggplot(data=., aes(coarse_date, tot_ET))+geom_line()+
  geom_point(aes(coarse_date,tot_ET_obs, color=bad_ET_obs))+
  scale_color_viridis_c("B")+
  theme_bw()


# prediction test 4 - there is a peak in 2016, I saw this peak in the EVI data too
pred_df <- df %>% select(ET, date, ssr, tp, pev, 
                         swvl1,swvl4, ws_era5,lwe_thickness_csr,vpd) %>%
  mutate(hour=hour(date)) %>%
  mutate(x_csr = roll_sumr(lwe_thickness_csr, n = 1)) %>% 
  mutate(flood_mon = ifelse(x_csr >=0,1,0)) %>% 
  mutate(ET_mET4 = predict(mET_4, newdata=., type='response'))
pred_df$ET %>% summary
pred_df$ET_mET4 %>% summary # need to set the maximum value

pred_df %>% 
  filter(is.na(lwe_thickness_csr)==F) %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  mutate(coarse_date = ymd(paste(year,month,15), tz="UTC")) %>% 
  group_by(coarse_date) %>% 
  summarize(tot_ET = sum(ET_mET4, na.rm=T), 
            tot_ET_obs = sum(ET, na.rm=T), 
            bad_ET_obs = sum(is.na(ET))) %>% 
  ungroup() %>% 
  ggplot(data=., aes(coarse_date, tot_ET))+geom_line()+
  geom_point(aes(coarse_date,tot_ET_obs, color=bad_ET_obs))+
  scale_color_viridis_c("B")+
  theme_bw()

# test 5
pred_df <- df %>% select(ET, date, ssrd, tp, pev, 
                         swvl1,swvl4, ws_era5,lwe_thickness_csr) %>%
  mutate(hour=hour(date)) %>%
  mutate(x_csr = roll_sumr(lwe_thickness_csr, n = 1)) %>% 
  mutate(flood_mon = ifelse(x_csr >=0,1,0)) %>% 
  mutate(ET_mET5 = predict(mET_5, newdata=., type='response'))
pred_df$ET %>% summary
pred_df$ET_mET5 %>% summary # need to set the maximum value

pred_df %>% 
  filter(is.na(lwe_thickness_csr)==F) %>% 
  mutate(year=year(date), 
         month=month(date)) %>% 
  mutate(coarse_date = ymd(paste(year,month,15), tz="UTC")) %>% 
  group_by(coarse_date) %>% 
  summarize(tot_ET = sum(ET_mET5, na.rm=T), 
            tot_ET_obs = sum(ET, na.rm=T), 
            bad_ET_obs = sum(is.na(ET))) %>% 
  ungroup() %>% 
  ggplot(data=., aes(coarse_date, tot_ET))+geom_line()+
  geom_point(aes(coarse_date,tot_ET_obs, color=bad_ET_obs))+
  scale_color_viridis_c("B")+
  theme_bw()


# Fit models with te smooth term  ------------------------------------------------

tmp_train %>% select(pev,d2m,strd,ssr,tp,trmm1,vpd,swvl1, swvl2,swvl3,swvl4,ws_era5,lwe_thickness_csr) %>% 
  cor %>% corrplot::corrplot(method='number')

mET_6 <- bam(ET ~ 
               te(vpd,ssrd,hour) +
               te(ssrd,swvl2,swvl4),
              # s(lwe_thickness_csr),
               data=tmp_train,method='REML',
               select=T,
              # rho = 0.7,
               family=Gamma(link='log')) # fit Gamma using a log link function
summary(mET_6)

AIC(mET_6)
res_mET_6 <- residuals(mET_6)
sqrt(mean(res_mET_6^2))


visreg(mET_6, 'vpd', by='hour', scale = 'response')
visreg(mET_6, 'ssr', by='hour', scale = 'response')
visreg(mET_6, 'ssr', by='swvl2', scale = 'response')


gam.check(mET_6); abline(0,1,col='red',lwd=3)
cor(tmp_test$ET, predict(mET_6, newdata=tmp_test, type='response'))**2

pred_df_mET_6 <- df %>% 
  mutate(hour = hour(date), month=month(date)) %>% 
  # filter(date>= as.POSIXct("2005-01-01") & 
  #        date < as.POSIXct("2008-01-01")) %>% 
  mutate(et_pred = predict(mET_6, newdata=., type='response'))


# plot MgcVIZ gam ---------------------------------------------------------
plotSlice(sm(getViz(mET_6), 1), fix=list('hour'=c(8,12,15,17)))+
  l_fitRaster()+
  l_fitContour()+
  l_points()+
  l_rug()+
  scale_fill_gradient2(high='blue', low='red')+
  labs(x=expression(paste(VPD~(kPa))), 
       y=expression(paste(Shortwave~Radiation~(W~m**2~s**-1))), 
       # title='GAM mET_6', 
       title=NULL,
       subtitle = 'ERA5 met. covariates')

plotSlice(sm(getViz(mET_6), 2), 
          fix=list('swvl4'=tmp_train$swvl4 %>% quantile(c(0.1,0.5,0.9))), 
          a.facet=list(nrow=1))+
  l_fitRaster()+
  l_fitContour()+
  l_points()+
  l_rug()+
  scale_fill_gradient2(high='blue', low='red')+
  labs(y=expression(paste(Theta[0-20~cm])), 
       x=expression(paste(Shortwave~Radiation~(W~m**2~s**-1))), 
       # title='GAM mET_6', 
       title=NULL,
       subtitle = 'ERA5 met. covariates')

test <- getViz(mET_6)

pl <- plotSlice(x = sm(test, 1),
                fix = list("ET" = seq(-2, 2, length.out = 9)))
pl + l_fitRaster() + l_fitContour() + l_points() + l_rug()+
  scale_fill_gradient2(low='red', high='blue', mid='white', midpoint = 0, limits=c(-50,50))



# Plot Predicetd ET  --------------------------------------------
# Annual variation
pred_df_mET_6 %>% 
  #filter(et_pred >0) %>% 
   #filter(date>= as.POSIXct("2016-01-01") &
  #        date < as.POSIXct("2016-12-31")) %>%
  filter(year==2016) %>% 
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T),
            ET_m = sum(ET, na.rm =T)) %>%
  ungroup() %>% 
  mutate(date = parse_date_time(paste(year,month,15), 'ymd')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+
  geom_point()+
  geom_line(aes(date, ET_m,col='ET'))+
  xlab(" ") + ylab(expression(paste("Predicted ET (mm",., month^-1,")")))+
  #ylim(50,170)+
  scale_color_manual(name = ' ',values=c('ET'="blue"))+
  #scale_x_datetime(breaks = '3 months')+
  theme_bw()

pred_df_mET_6 %>% 
  #filter(date>= as.POSIXct("2016-01-01") &
  #        date < as.POSIXct("2016-12-31")) %>%
  mutate(year=year(date), month=month(date)) %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T)) %>% 
  ungroup() %>%
  mutate(date = parse_date_time(paste(year,month,15), 'ymd')) %>% 
  filter(year>=2016) %>% 
  ggplot(data=., aes(date, tot_pred_et))+
  geom_line()+
  xlab(" ") + ylab(expression(paste("Predicted ET (mm",., month^-1,")")))+
  #ylim(50,170)+
  scale_color_manual(name = ' ',values=c('ET'="blue"))+
  #scale_x_datetime(breaks = '3 months')+
  theme_bw()

 # Hourly variation
pred_df_mET_6 %>% 
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

pred_df_mET_6 %>% 
    filter(hour>=6 & hour<=18) %>% 
    group_by(month,hour) %>% summarize(u=mean(et_pred,na.rm=T)) %>% 
    ggplot(data=., aes(month, u, color=as.factor(hour)))+geom_line()
   
# Annual ET
pred_df_mET_6 %>% 
  group_by(year) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T), 
            nobs = sum(is.na(et_pred)==F)) %>% 
  ungroup() %>% 
  filter(tot_pred_et<= 2000) %>% 
  filter(year>=2004 & year<=2018) %>% 
  # mutate(date=ymd(paste(year,month,15), tz='UTC')) %>% 
  ggplot(data=., aes(year, tot_pred_et))+geom_line()


# Anomaly plot ------------------------------------------------------------


dat_agg_era <- pred_df_mET_6 %>% 
  mutate(year=year(date), 
         month = month(date)) %>% 
  filter(et_pred>0) %>% 
  #filter(year>= 2015) %>% 
  group_by(year,month) %>% 
  summarize(ET = sum(ET, na.rm=T),
            ET_pred = sum(et_pred, na.rm=T)) %>% 
  ungroup() %>%
  mutate(date = ymd(paste(year,month,15))) %>% 
  filter(year<=2017)

dat_agg_era %>% 
  #filter(year==2015) %>% 
  ggplot(data=., aes(date, ET_pred))+
  geom_line()+
  scale_x_date(breaks = "1 year", date_labels = ("%Y"))

dat_norms_era <- dat_agg_era %>% 
  group_by(month) %>% 
  summarize(u_et = mean(ET_pred),
            ET_se=stderr(ET_pred),
            E_ET= qnorm(.975)*ET_se) %>% 
  ungroup()


dat_agg_era %>% 
  left_join(., dat_norms_era, by='month') %>% 
  mutate(ET_anom = ET_pred-u_et) %>% 
  ggplot(data=., aes(date, ET_anom))+
  geom_line()+
  scale_x_date(breaks = "1 year", date_labels = ("%Y"))

mycolors <- (c("grey90","grey90","#000099","grey90","grey90","#000099","grey90","grey90","grey90","grey90","grey90","grey90","#CC0000","#CC0000", "black"))


dat_agg_era %>% 
  left_join(., dat_norms_era, by='month') %>% 
  mutate(year=year(date), 
         doy = yday(date), 
         month = month(date)) %>% 
  group_by(year,month) %>% 
  summarize(tot = sum(ET_pred),
            u_et = mean(u_et),
            E_ET = mean(E_ET)) %>% 
  ungroup() %>% 
  filter(year>=2004) %>% 
  mutate(month2=parse_date_time(paste(month),'m')) %>%
  #filter(year %in% c(2014,2015,2016)) %>% 
  ggplot(data=., aes(month2, tot, color=as.factor(year)))+
  geom_line()+
  geom_point()+
  geom_line(aes(month2, u_et, col = 'mean'))+
  geom_point(aes(month2, u_et, col = 'mean'))+
  geom_ribbon(aes(month2, ymax=u_et+E_ET, ymin=u_et-E_ET),lty=0,alpha=0.02)+
  scale_x_datetime(breaks = as.POSIXct(c('0-01-01 00:00:00','0-02-01 00:00:00','0-03-01 00:00:00','0-04-01 00:00:00','0-05-01 00:00:00','0-06-01 00:00:00','0-07-01 00:00:00','0-08-01 00:00:00','0-09-01 00:00:00','0-10-01 00:00:00','0-11-01 00:00:00','0-12-01 00:00:00')), date_labels = ("%b"))+
  scale_color_manual(name= 'years', values = mycolors)+
  theme_bw()+
  xlab(" ") + ylab(expression(paste("ET (mm",' ', month^-1,")")))+
  theme(text = element_text(size=16))

tiff("fig\\interannual_ET.tiff", units="in", width=15, height=9, res=300)

dev.off()

# Checking model and residual inspection ---------------------------------------------------------------

mET_6_scale <- bam(ET ~ 
                      te(scale(vpd),scale(ssr),scale(hour)) +
                      te(scale(ssr), scale(swvl2),scale(swvl4))+
                      s(scale(tp)),  
                    # s(lwe_thickness_csr),
                    data=tmp_train,
                    select=T,
                    # rho = 0.7,
                    family=Gamma(link='log')) # fit Gamma using a log link function
summary(mET_6_scale)

# turn off first and second smooths
cor(tmp_test$ET, 
    predict(mET_6, 
            newdata=tmp_test %>% mutate(vpd=0, ssr=0, hour=0), 
            type='response'))**2

# turn off second smooth - to see which variables are influencing more 
cor(tmp_test$ET, 
    predict(mET_6, 
            newdata=tmp_test %>% mutate(ssr=0, swvl2=0, swvl4=0), 
            type='response'))**2

# turn off third smooth
cor(tmp_test$ET, 
    predict(mET_6, 
            newdata=tmp_test %>% mutate(tp=0), 
            type='response'))**2

tmp_test %>% select(vpd, ssr, hour, swvl2, swvl4, tp) %>% 
  cor %>% 
  corrplot::corrplot(method='number')

# More GAM's with derivative terms ----------------------------------------


##using Grace in predictions. Just to check the depletion of water would be better represented,
# it is less smooth when ploting per month
mET_7 <- bam(ET ~ 
               te(vpd,ssr,hour) +
               te(swvl2,lwe_thickness_csr)+
               s(tp),  
             # s(lwe_thickness_csr),
             data=tmp_train,method='REML',
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function



summary(mET_7)
AIC(mET_7)
res_mET_7 <- residuals(mET_7)
sqrt(mean(res_mET_7^2))


visreg(mET_7, 'vpd', by='hour', scale = 'response')
visreg(mET_7, 'ssr', by='swvl2', scale = 'response')
visreg(mET_7, 'ssr', by='lwe_thickness_csr', scale = 'response')


gam.check(mET_7); abline(0,1,col='red',lwd=3)
cor(tmp_test$ET, predict(mET_7, newdata=tmp_test, type='response'))**2

pred_df_Met7 <- df %>% 
  mutate(hour = hour(date), month=month(date)) %>% 
  # filter(date>= as.POSIXct("2005-01-01") & 
  #        date < as.POSIXct("2008-01-01")) %>% 
  mutate(et_pred = predict(mET_7, newdata=., type='response'))

pred_df_Met7 %>% 
  mutate(year=year(date), month=month(date)) %>% 
  filter(year>=2004 & year<= 2016) %>% 
  #filter(year==2009) %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T),
            ET_m = sum(ET, na.rm =T)) %>%
  ungroup() %>% 
  mutate(date = parse_date_time(paste(year,month,15), 'ymd')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+
  geom_line()+
  geom_point(aes(date, ET_m,col='ET'))+
  scale_color_manual(name = ' ',values=c('ET'="blue"))+
  theme_bw()+
  ylim(90,170)

pred_df_Met7 %>% 
  mutate(year=year(date), month=month(date)) %>% 
  filter(year>=2004 & year<= 2016) %>% 
  #filter(year==2005) %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T),
            ET_m = sum(ET, na.rm =T)) %>%
  ungroup() %>% 
  mutate(date = parse_date_time(paste(year,month,15), 'ymd')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+
  geom_line()+
  geom_line(aes(date, ET_m,col='ET'))+
  scale_color_manual(name = ' ',values=c('ET'="blue"))+
  scale_x_datetime(breaks = '9 months')+
  theme_bw()+
  ylim(50,170)


pred_df_Met7 %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T), 
            nobs = sum(is.na(et_pred)==F),
            ET= sum(ET)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,15), tz='UTC')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+geom_line()+
  geom_line(aes(date, ET))+
  ylim(90,170)

# Grace influenced a lot when summarizing per year...
pred_df_Met7 %>% 
  group_by(year) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T), 
            nobs = sum(is.na(et_pred)==F)) %>% 
  ungroup() %>% 
  filter(year>=2004 & year<=2016) %>% 
  # mutate(date=ymd(paste(year,month,15), tz='UTC')) %>% 
  ggplot(data=., aes(year, tot_pred_et))+geom_line()+
  ylim(1500,1700)


# Hourly variation
pred_df_Met7 %>% 
  mutate(year=year(date), month=month(date),hour=hour(date)) %>% 
  #filter(year>=2004) %>% 
  group_by(hour) %>% 
  summarize(tot_pred_et = mean(et_pred, na.rm=T),
            ET_m = mean(ET, na.rm =T)) %>%
  ungroup() %>% 
  ggplot(data=., aes(hour, tot_pred_et, col='Predicted ET'))+
  geom_line()+
  geom_line(aes(hour, ET_m,col='ET'))+
  theme_bw()+
  xlab("hour") + ylab(expression(paste("mm",., hour^-1,"")))+
  scale_color_manual(name = ' ',values=c('Predicted ET'='black','ET'="red"))
# Hour variation per month

pred_df_Met7 %>% 
  filter(hour>=6 & hour<=18) %>% 
  group_by(month,hour) %>% summarize(u=mean(et_pred,na.rm=T)) %>% 
  ggplot(data=., aes(month, u, color=as.factor(hour)))+geom_line()

# changing hour per Solar derivative - R decrease 0.61 and inter-annual plots are
# strange

mET_8 <- bam(ET ~ 
               te(vpd,ssr_deriv) +
               te(ssr,swvl2,swvl4),
             # s(lwe_thickness_csr),
             data=tmp_train,
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function



summary(mET_8)
AIC(mET_8)
res_mET_8 <- residuals(mET_8)
sqrt(mean(res_mET_8^2))



visreg(mET_8, 'vpd', by='hour', scale = 'response')
visreg(mET_8, 'ssr_deriv', by='vpd', scale = 'response')
visreg(mET_8, 'ssr', by='swvl4', scale = 'response')


gam.check(mET_8); abline(0,1,col='red',lwd=3)
cor(tmp_test$ET, predict(mET_8, newdata=tmp_test, type='response'))**2

pred_df_Met8 <- df %>% 
  mutate(hour = hour(date), month=month(date)) %>% 
  # filter(date>= as.POSIXct("2005-01-01") & 
  #        date < as.POSIXct("2008-01-01")) %>% 
  mutate(et_pred = predict(mET_8, newdata=., type='response'))

pred_df_Met8 %>% 
  mutate(year=year(date), month=month(date)) %>% 
  filter(year>=2004 & year<= 2016) %>% 
  #filter(year==2009) %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T),
            ET_m = sum(ET, na.rm =T)) %>%
  ungroup() %>% 
  mutate(date = parse_date_time(paste(year,month,15), 'ymd')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+
  geom_line()+
  geom_point(aes(date, ET_m,col='ET'))+
  scale_color_manual(name = ' ',values=c('ET'="blue"))+
  theme_bw()+
  ylim(90,170)


pred_df_Met8 %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T), 
            nobs = sum(is.na(et_pred)==F),
            ET= sum(ET)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,15), tz='UTC')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+geom_line()+
  geom_line(aes(date, ET))+
  ylim(90,170)

# Grace influenced a lot when summarizing per year...
pred_df_Met8 %>% 
  group_by(year) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T), 
            nobs = sum(is.na(et_pred)==F)) %>% 
  ungroup() %>% 
  filter(year>=2004 & year<=2016) %>% 
  # mutate(date=ymd(paste(year,month,15), tz='UTC')) %>% 
  ggplot(data=., aes(year, tot_pred_et))+geom_line()


# Hourly variation
pred_df_Met8 %>% 
  mutate(year=year(date), month=month(date),hour=hour(date)) %>% 
  #filter(year>=2004) %>% 
  group_by(hour) %>% 
  summarize(tot_pred_et = mean(et_pred, na.rm=T),
            ET_m = mean(ET, na.rm =T)) %>%
  ungroup() %>% 
  ggplot(data=., aes(hour, tot_pred_et, col='Predicted ET'))+
  geom_line()+
  geom_line(aes(hour, ET_m,col='ET'))+
  theme_bw()+
  xlab("hour") + ylab(expression(paste("mm",., hour^-1,"")))+
  scale_color_manual(name = ' ',values=c('Predicted ET'='black','ET'="red"))
# Hour variation per month

pred_df_Met8 %>% 
  filter(hour>=6 & hour<=18) %>% 
  group_by(month,hour) %>% summarize(u=mean(et_pred,na.rm=T)) %>% 
  ggplot(data=., aes(month, u, color=as.factor(hour)))+geom_line()

## Using GRACE derivative - predictions stoped in 2015

mET_9 <- bam(ET ~ 
               te(vpd,ssr,hour) +
               te(ssr,swvl2,lwe_gfz_deriv_5)+
               s(trmm),  
             # s(lwe_thickness_csr),
             data=tmp_train,
             select=T,
             # rho = 0.7,
             family=Gamma(link='log')) # fit Gamma using a log link function



summary(mET_9)
visreg(mET_9, 'vpd', by='hour', scale = 'response')
visreg(mET_9, 'swvl2', by='lwe_gfz_deriv_5', scale = 'response')
visreg(mET_9, 'ssr', by='lwe_gfz_deriv_5', scale = 'response')


gam.check(mET_9); abline(0,1,col='red',lwd=3)
cor(tmp_test$ET, predict(mET_9, newdata=tmp_test, type='response'))**2

pred_df_Met9 <- df %>% 
  mutate(hour = hour(date), month=month(date)) %>% 
  # filter(date>= as.POSIXct("2005-01-01") & 
  #        date < as.POSIXct("2008-01-01")) %>% 
  mutate(et_pred = predict(mET_9, newdata=., type='response'))

pred_df_Met9 %>% 
  mutate(year=year(date), month=month(date)) %>% 
  filter(year>=2004 & year<= 2016) %>% 
  #filter(year==2009) %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T),
            ET_m = sum(ET, na.rm =T)) %>%
  ungroup() %>% 
  mutate(date = parse_date_time(paste(year,month,15), 'ymd')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+
  geom_line()+
  geom_point(aes(date, ET_m,col='ET'))+
  scale_color_manual(name = ' ',values=c('ET'="blue"))+
  theme_bw()+
  ylim(90,170)


pred_df_Met9 %>% 
  group_by(year,month) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T), 
            nobs = sum(is.na(et_pred)==F),
            ET= sum(ET)) %>% 
  ungroup() %>% 
  mutate(date=ymd(paste(year,month,15), tz='UTC')) %>% 
  ggplot(data=., aes(date, tot_pred_et))+geom_line()+
  geom_line(aes(date, ET))+
  ylim(90,170)

# Grace influenced a lot when summarizing per year...
pred_df_Met9 %>% 
  group_by(year) %>% 
  summarize(tot_pred_et = sum(et_pred, na.rm=T), 
            nobs = sum(is.na(et_pred)==F)) %>% 
  ungroup() %>% 
  filter(year>=2004 & year<=2016) %>% 
  # mutate(date=ymd(paste(year,month,15), tz='UTC')) %>% 
  ggplot(data=., aes(year, tot_pred_et))+geom_line()


# Hourly variation
pred_df_Met9 %>% 
  mutate(year=year(date), month=month(date),hour=hour(date)) %>% 
  #filter(year>=2004) %>% 
  group_by(hour) %>% 
  summarize(tot_pred_et = mean(et_pred, na.rm=T),
            ET_m = mean(ET, na.rm =T)) %>%
  ungroup() %>% 
  ggplot(data=., aes(hour, tot_pred_et, col='Predicted ET'))+
  geom_line()+
  geom_line(aes(hour, ET_m,col='ET'))+
  theme_bw()+
  xlab("hour") + ylab(expression(paste("mm",., hour^-1,"")))+
  scale_color_manual(name = ' ',values=c('Predicted ET'='black','ET'="red"))
# Hour variation per month

pred_df_Met9 %>% 
  filter(hour>=6 & hour<=18) %>% 
  group_by(month,hour) %>% summarize(u=mean(et_pred,na.rm=T)) %>% 
  ggplot(data=., aes(month, u, color=as.factor(hour)))+geom_line()

