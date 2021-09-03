
# Script cleaning up and re-exporting daily fence data

#Babine 2010-2017

library(readxl)
library(tidyverse)
library(ggplot2)


babine201017 <- read_excel("BabineAdultWeirCount.xls", sheet="2010-17tab")

str(babine201017)

babine <- babine201017 %>% 
  mutate(Date = ymd(paste0(Year,substr(as.character(Date), 
                                  start=6, stop = 10)))) %>% 
  mutate(yday = yday(Date), fake.date = as_date(yday,origin="2021-01-01")) %>% 
  filter(yday<=278 & yday >= 213)

yday(ymd("2021-10-04"))

tmp <- babine %>% 
  mutate(calc.tot.co = adi.CO+adi.max.CO+unclip.CO+uncheck.CO) %>% 
  mutate(discrep = calc.tot.co - tot.CO) %>% 
  select(Date,calc.tot.co,tot.CO, discrep) 



# Plot hydro station 

# Babine R at outlet of Nilkitkwa L: 08EC013
# Babine R at Fort Babine: 08EC001

hydro.babine <- hy_daily_levels(station_number = c("08EC013")) %>% 
  mutate(Year = year(Date), julian = yday(Date)) %>% 
  filter(Year %in% c(2010:2017)) %>% 
  filter(julian<=278 & julian >= 213) %>% 
  mutate(fake.date = as_date(julian,origin="2021-01-01")) %>% 
  mutate(fyear = factor(Year, order = T))  
  #filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
  #filter(!is.na(year))

ggplot(data=hydro.babine)+
  geom_line(aes(x=fake.date, y=Value), size=1) + 
  facet_wrap(~Year)


#plot together

ggplot()+
  geom_point(data = babine,aes(x=as_date(fake.date),y=tot.CO, col=as.factor(Year)), 
             size=2)+
  geom_smooth(data = babine, aes(x=as_date(fake.date),y=tot.CO, col=as.factor(Year)),
              method = "loess", se=F)+
  geom_line(data=hydro.babine, aes(x=fake.date, y=Value*200), size=1) +
  facet_wrap(~Year, scales = "free_y")+
  scale_x_date(limits = c(as_date("2021-aug-01"),as_date("2021-oct-10")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="Babine Coho",x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=6),title = element_text(size=6),
        legend.box = "vertical")
