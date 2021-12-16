
# This script is used to clean, QA and analyse fish fence data
# Author: Kristen P., DFO 
# Created 2 Sept 2021



library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)



##### Babine fence ####

#newer coho data #Babine 1946-2020:
babine194620 <- read_excel("Babine Coho Daily 1946-2020.xlsx", sheet="Coho",
                          col_names = T ) %>% 
  gather("Year","Count",-Date) %>% 
  mutate(day=day(Date),month=month(Date),julian=yday(Date),
         Date=ymd(paste(Year,month,day)),fake.date=as_date(julian,origin="2021-01-01")) %>% 
  filter(!is.na(Count))

#extension years based on date of operation
extension.yrs <- as.vector(unique(babine194620[which(babine194620$julian >= 288),"Year"]))
#filter df
babine.extensions1 <- babine194620 %>% 
  filter(Year %in% extension.yrs$Year)

#"base years" based on Holtby
base.yrs = c(1950,1952,1957,1976,1977,1979,1985,1989,1995,1996,1998)

#filter df for base years
babine.extensions2 <- babine194620 %>% 
  filter(Year %in% base.yrs)

#current year:
babine2021 <- read_csv("Daily.counts.all.Babine-copy.csv") %>% 
  select(date,CO)
          
#plot years together:              
ggplot()+
  geom_line(data=babine.extensions1,
            aes(x=fake.date,y=Count,col=Year),size=1.5)+
  geom_line(data=babine2021,aes(x=date,y=CO),col="black",size=2)+
  scale_x_date(limits = c(as_date("2021-Sep-01"),as_date("2021-dec-05")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="2021 compared to late years")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),title = element_text(size=6),
        legend.box = "vertical",
        plot.title=element_text(size=10))

#plot years together - holtby:              
ggplot()+
  geom_line(data=babine.extensions2,
            aes(x=fake.date,y=Count,col=Year),size=1.5)+
  geom_line(data=babine2021,aes(x=date,y=CO),col="black",size=2)+
  scale_x_date(limits = c(as_date("2021-Sep-01"),as_date("2021-nov-22")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="2021 compared to Base Years (Holtby)")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),title = element_text(size=6),
        legend.box = "vertical",
        plot.title=element_text(size=10))
  

# yday(ymd("2021-10-15"))
# as_date(280,origin="2021-01-01")




#### Coho expansion - Holtby's method ####

# calculate ave daily proportion of run for base years

total.base.byyr <- babine.extensions2 %>% 
  group_by(Year) %>% 
  filter(fake.date < ymd("2021-10-15")) %>% #in Holtby, appear to cut the timing to Oct 14th for the total run
  summarize(total = sum(Count))

base.yrs.df <- babine.extensions2 %>% 
  filter(fake.date < ymd("2021-10-15")) %>% 
  left_join(total.base.byyr, by="Year") %>% 
  mutate(daily.prop = Count/total)

ave.daily.base.yrs <- base.yrs.df %>% 
  group_by(fake.date) %>% 
  summarize(ave.daily.prop = mean(daily.prop)) %>% 
  mutate(cumul.prop = cumsum(ave.daily.prop))

#visualize
ggplot(ave.daily.base.yrs)+
  geom_line(data=base.yrs.df, aes(x=fake.date,y=daily.prop,col=Year))+
  geom_line(aes(x=fake.date,y=ave.daily.prop))

#expand non-base years using base year proportions
    #base years from Holtby report
base.yrs = c(1950,1952,1957,1976,1977,1979,1985,1989,1995,1996,1998)
non.base.yrs <- setdiff(as.numeric(unique(babine194620$Year)),base.yrs)
non.base.yrs <- setdiff(non.base.yrs,1965) #removed 1965 since they estimate differently

#non base year df
babine.nonbase <- babine194620 %>% 
  filter(Year %in% non.base.yrs) 

total.nonbase.byyr <- babine.nonbase%>% 
  arrange(Year, julian) %>% 
  group_by(Year) %>% 
  summarize(total = sum(Count, na.rm=T), last.julian = tail(julian,1),
            fake.date = tail(fake.date,1)) 

#only expand counts on years when counts did not go to Oct 15th
expanded.yrs <- total.nonbase.byyr %>% 
  filter(fake.date < ymd("2021-10-15")) %>% 
  left_join(ave.daily.base.yrs, by= "fake.date") %>% 
  select(-ave.daily.prop) %>% 
  mutate(est.total = total/cumul.prop)

non.expanded.yrs <- total.nonbase.byyr %>% 
  filter(fake.date >= ymd("2021-10-15")) %>% 
  mutate(cumul.prop = 1, est.total = total/cumul.prop)

base.yrs.noexpansion <- babine.extensions2 %>% 
  group_by(Year) %>% 
  #filter(fake.date < ymd("2021-10-15")) %>% #in Holtby, appear to cut the timing to Oct 14th for the total run
  summarize(total = sum(Count), last.julian = tail(julian,1),
            fake.date = tail(fake.date,1)) %>% 
  mutate(cumul.prop = 1, est.total = total/cumul.prop)

#put back together:
total.byyr <- rbind(expanded.yrs,non.expanded.yrs,base.yrs.noexpansion) %>% 
  mutate(est.total = round(est.total,0),
         additional = est.total-total, Year=as.numeric(Year)) %>% 
  arrange(Year)

second2021 <- total.byyr %>% 
  filter(Year > 2021.1) %>% 
  mutate(Year = 2022, Year.lab = "2021 Actual")
  
therest <- total.byyr %>% 
  filter(Year <= 2021.1) %>% 
  mutate(Year = ifelse(Year %in% 2021.1, 2021, Year))

holtby20002021 <- ggplot(therest)+
  geom_col(aes(x=Year, y=est.total), fill="grey10", col="black") + 
  geom_col(aes(x=Year, y=total), fill="grey50", col="black") +
  geom_col(data=second2021,aes(x=Year, y=total), fill="purple", col="black") +
  geom_text(aes(x=2021,y=7900,label="est'd"),size=2)+
  geom_text(aes(x=2022,y=8200,label="actual"),size=2)+
  geom_text(aes(x=2021.5,y=9000,label="2021"),size=3)+
  geom_hline(aes(yintercept=1200))+
  geom_hline(aes(yintercept=11500),linetype="dashed")+
  scale_x_continuous(limits = c(1999,2023),breaks = seq(2000,2020,1))+
  labs(y="Coho Count (grey) and Expansion (black)",x="")+
  theme_classic()+theme(axis.text.x = element_text(hjust=1,angle=45))
  
        
# axis.text.y = element_text(size = 14),
        # axis.title.y = element_text(size = 16))
holtby20002021

ggsave(holtby20002021,filename = "holtby20002021.png",width = 6.5,height=4,device = "png")












# babine <- babine201017 %>%
#   mutate(Date = ymd(paste0(Year,substr(as.character(Date),
#                                   start=6, stop = 10)))) %>%
#   mutate(yday = yday(Date), fake.date = as_date(yday,origin="2021-01-01")) %>%
#   filter(yday<=278 & yday >= 213)


# tmp <- babine %>% 
#   mutate(calc.tot.co = adi.CO+adi.max.CO+unclip.CO+uncheck.CO) %>% 
#   mutate(discrep = calc.tot.co - tot.CO) %>% 
#   select(Date,calc.tot.co,tot.CO, discrep) 



# Plot hydro station 

# Babine R at outlet of Nilkitkwa L: 08EC013
# Babine R at Fort Babine: 08EC001

hydro.babine <- hy_daily_levels(station_number = c("08EC013")) %>% 
  mutate(Year = year(Date), julian = yday(Date)) %>% 
  filter(Year %in% c(2010:2017)) %>% 
  #filter(julian<=278 & julian >= 213) %>% 
  mutate(fake.date = as_date(julian,origin="2021-01-01")) %>% 
  mutate(fyear = factor(Year, order = T))  
  #filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
  #filter(!is.na(year))


hydro.skeena <- hy_daily_levels(station_number = c("08EB005")) %>% 
  mutate(Year = year(Date), julian = yday(Date)) %>% 
  filter(Year %in% c(2010:2017)) %>% 
  #filter(julian<=278 & julian >= 213) %>% 
  mutate(fake.date = as_date(julian,origin="2021-01-01")) %>% 
  mutate(fyear = factor(Year, order = T))  
#filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
#filter(!is.na(year))

yday(as_date("2021=oct-31"))

ggplot()+
  geom_line(data=hydro.babine,aes(x=fake.date, y=Value), col="green", size=1) + 
  geom_line(data=hydro.skeena,aes(x=fake.date, y=Value), col="blue", size=1) + 
  facet_wrap(~Year)


#plot together - don't really correlate at all, so take out hydro

ggplot()+
  geom_point(data = babine,aes(x=as_date(fake.date),y=tot.CO, col=as.factor(Year)), 
             size=2)+
  geom_smooth(data = babine, aes(x=as_date(fake.date),y=tot.CO, col=as.factor(Year)),
              method = "loess", se=F)+
  #geom_line(data=hydro.skeena, aes(x=fake.date, y=Value*50), size=1) +
  facet_wrap(~Year)+
  scale_x_date(limits = c(as_date("2021-aug-01"),as_date("2021-oct-31")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="Babine Fence Coho",x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=6),title = element_text(size=6),
        legend.box = "vertical")


#Meziadin fence

mez <- read_excel("Meziadin Daily Update 2021.xlsx", 
                  sheet="CohoAdults")
str(mez)

mez.daily <- mez %>% 
  gather("Year","cohoAL",-Date) %>% 
  mutate(Date= ymd(paste0(Year,"-",substr(Date,6,10)))) %>% 
  mutate(julian = yday(Date)) %>% 
  filter(Year %in% c(2015:2020)) %>% 
  mutate(fake.date = as_date(julian,origin="2021-01-01")) 

ggplot(data=mez.daily)+
  geom_point(aes(x=as_date(fake.date),y=cohoAL, col=as.factor(Year)), 
             size=2)+
  geom_smooth(aes(x=as_date(fake.date),y=cohoAL, col=as.factor(Year)),
              method = "loess", se=F)+
  #facet_wrap(~Year)+
  scale_x_date(limits = c(as_date("2021-aug-01"),as_date("2021-oct-31")),
               date_breaks= "1 week", date_labels = "%d-%b")+
  labs(title="Mez Fence Coho",x="appr.date", y="adult Coho")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),title = element_text(size=10),
        legend.box = "vertical")




