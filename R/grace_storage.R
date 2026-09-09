# Calculating storage water from GRACE
grace <- read_csv('data/GRACE_ban_20000101_20180101.csv') %>% 
  mutate(date1 =ymd(substr(`system:index`,1,8), tz='UTC')) %>% 
  mutate(date2 = ymd(substr(`system:index`,10,17), tz='UTC')) %>% 
  mutate(year=year(date1), month=month(date1)) %>% 
  select(-.geo)

dados <- read.table("C:\\Users\\Leticia\\OneDrive\\OneDrive - inpe.br\\BANANAL\\Artigo\\Figuras_R\\chuva_TRMM3.txt", header=TRUE, sep = "\t", dec = ",")
dados <- dados %>% mutate(date1= as.Date(dados$Ano_Mês, "%m/%d/%y")) %>% 
  mutate(date1)

ds = grace$lwe_thickness_csr[5]-grace$lwe_thickness_csr[3]


# calculating storage through dplyr ---------------------------------------
#ver como manter o valor negativo, ou multiplicar por -1

teste <- grace %>%
  arrange(date1) %>%
  mutate(diff = lag(lwe_thickness_csr, default = first(lwe_thickness_csr), order_by = date1) - lead(lwe_thickness_csr, default = first(lwe_thickness_csr),order_by = date1)) %>% 
  mutate(day_dif = date2 - date1)


teste %>% filter(year==2007) %>% ggplot(aes(date1,diff))+geom_line()+
  geom_line(aes(date1,lwe_thickness_csr, col='red'))

teste$diff
teste$lwe_thickness_csr
teste$day_dif


# figure river level and GRACE --------------------------------------------


raw1 <- readxl::read_xlsx("data/jav_level.xlsx", sheet = 2)

raw1 <- raw1 %>% mutate(year=year(Data), month=month(Data)) %>% 
  filter(year>=2002) %>% 
  mutate(date1=parse_date_time(paste(year,month,01),'ymd'))

grc_month <- df %>% mutate(year=year(date), month=month(date)) %>% 
  filter(year>=2002) %>% 
  group_by(month, year) %>% 
  summarise(lwe_thickness_csr = mean(lwe_thickness_csr)) %>% 
  mutate(date1=parse_date_time(paste(year,month,01),'ymd')) %>% 
  arrange(date1)

riv <- left_join(raw1,grc_month, by=('date1'))

riv %>%  ggplot(aes(date1,media_calculada/100, col='Javaés level'))+geom_line()+
  geom_line(aes(date1, lwe_thickness_csr/10, col= 'TWS anomaly'))+
  geom_line(aes(date1, torre, col='Flood level'))+geom_line()+
  scale_color_manual(name = ' ',values=c('TWS anomaly'="black", 'Javaés level'= 'blue4', 'Flood level' = 'red'))+
  theme_bw()+
  theme(legend.justification=c(1,0.2),
        legend.position=c(0.2, 0.9),
        legend.background = element_blank(),
        axis.text=element_text(size=18),
        text = element_text(size=18),
        legend.key = element_blank()) +
  theme(legend.key.width = unit(2, 'lines'),
        legend.spacing.y = unit(0.0000000005, "cm"))+
  theme(panel.grid.major = element_blank())+
  scale_x_datetime(breaks = '1 year', date_labels = "%Y")+
  scale_y_continuous(sec.axis = sec_axis(~. *10, name = "Total water storage (TWS) anomaly (cm)"))+
  ylab('Javaés/flood height (m)')+
  xlab(' ')
  
library(ggpubr);library(ggpmisc)
# 
riv %>%  ggplot(data=.,aes(x=lwe_thickness_csr, y=media_calculada/100)) + geom_point()+
  ylab("River level (m)")+
  xlab("TWS (cm)")+
  theme_bw()+
  #facet_wrap(~periods, ncol=2)+
  geom_smooth(method=lm, col="red")+
  theme(text = element_text(size=14))+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.5f',
                                      stat(r.squared), stat(p.value))),
                  parse = TRUE)
# 
raw1 %>%  filter(is.na(torre)==F & year>=2010) %>% 
  ggplot(data=.,aes(x=media_calculada/100, y=torre)) + geom_point()+
  ylab("Flood height (m)")+
  xlab("River level (m)")+
  theme_bw()+
  #facet_wrap(~periods, ncol=2)+
  geom_smooth(method=loess, col="red", se=F)+
  theme(text = element_text(size=22),
        axis.text=element_text(size=22))+
  stat_fit_glance(method = "lm",
                  method.args = list(formula = y ~ x),size = 9,
                  aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"<0.001"',
                                      stat(r.squared), stat(p.value))),
                  parse = TRUE)

tiff("fig\\flood_river_scatter2.tiff", units="in", width=11, height=10, res=300)

dev.off()


raw1_mod = lm(raw1$media_calculada~raw1$torre)

tiff("fig\\flood_river_grace2.tiff", units="in", width=14, height=9, res=300)

dev.off()

cor.test(riv$media_calculada,riv$lwe_thickness_csr)
  
model = lm(riv$media_calculada~riv$lwe_thickness_csr)
summary(model)
