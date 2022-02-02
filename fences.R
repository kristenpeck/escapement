
# This script is used to clean, QA and analyse fish fence data
# Fences included: Babine, Toboggan
# Author: Kristen P., DFO 
# Created 2 Sept 2021


library(plyr)
library(dplyr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)

#### ggplot theme ####
theme_babine4 <- function(base_size = 14) {
  theme_bw(base_size = base_size) %+replace%
    theme(
      # L'ensemble de la figure
      plot.title = element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
      # Zone où se situe le graphique
      #panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      # Les axes
      axis.title = element_text(size = rel(0.85), face = "bold"),
      axis.text.x = element_text(size = rel(0.80), face = "bold", 
                                 angle=60, hjust = 1, vjust=1),
      axis.text.y = element_text(size = rel(0.80), face = "bold"),
      axis.line = element_line(color = "black", arrow = arrow(length = unit(0.3, "lines"), type = "closed")),
      # La légende
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.85), face = "bold"),
      legend.text = element_text(size = rel(0.70), face = "bold"),
      legend.key = element_rect(fill = "transparent", colour = NA),
      legend.key.size = unit(1.5, "lines"),
      legend.background = element_rect(fill = "transparent", colour = NA),
      # Les étiquettes dans le cas d'un facetting
      strip.background = element_rect(fill = "#17252D", color = "#17252D"),
      strip.text = element_text(size = rel(0.75), face = "bold", color = "white", margin = margin(5,0,5,0))
    )
}



##### Babine fence ####

#newer coho data #Babine 1946-2021:
babine194620 <- read_excel("Babine Coho Daily 1946-2021.xlsx", sheet="Coho",
                          col_names = T ) %>% 
  gather("Year","Count",-Date) %>% 
  mutate(Year= as.numeric(Year),day=day(Date),month=month(Date),julian=yday(Date),
         Date=ymd(paste(Year,month,day)),fake.date=as_date(julian,origin="2021-01-01")) %>% 
  filter(!is.na(Count))

#get total counts by year
(total.counts.CO <- babine194620 %>% 
  dplyr::group_by(Year) %>% 
  dplyr::summarize(total.CO = sum(Count)))

#calc cumulative proportion of the daily run by year, add columns
babine.all <- ddply(babine194620, "Year", summarize, Date=Date, Count=Count, 
      cumulsum = cumsum(Count)) %>% 
  left_join(total.counts.CO) %>% 
  mutate(daily.prop = Count/total.CO, 
         cumulprop = round(cumulsum/total.CO,2),
         day=day(Date),month=month(Date),julian=yday(Date),
        fake.date=as_date(julian,origin="2021-01-01"),
        decade = ifelse(Year %in% c(1940:1949), "1940s",
                        ifelse(Year %in% c(1950:1959), "1950s",
                               ifelse(Year %in% c(1960:1969), "1960s",
                                      ifelse(Year %in% c(1970:1979), "1970s",
                                             ifelse(Year %in% c(1980:1989), "1980s",
                                                    ifelse(Year %in% c(1990:1999), "1990s",
                                                           ifelse(Year %in% c(2000:2009), "2000s",
                                                                  ifelse(Year %in% c(2010:2019), "2010s", 
                                                                         ifelse(Year %in% c(2020:2029), "2020s", NA)))))))))) %>% 
  filter(!is.na(Count))
unique(babine.all[,c("Year","decade")])
  


#pick extension years based on date of operation - Oct15 cutoff
extension.yrs <- as.vector(unique(babine.all[which(babine.all$julian >= yday(ymd("2021-10-15"))),"Year"]))
length(extension.yrs)
yday(ymd("2021-10-15"))

#filter df for yrs running past mid-Oct
babine.extensions1 <- babine.all %>% 
  filter(Year %in% extension.yrs)

(extension.by.decade <- babine.extensions1 %>% 
  filter(!(decade %in% "2020s")) %>% 
  group_by(fake.date,decade) %>% 
  summarize(mn=mean(Count, na.rm=T),sd=sd(Count, na.rm=T), 
            n=length(Count),
            se=sd(Count, na.rm=T)/sqrt(length(Count))))

ggplot(extension.by.decade)+
  geom_ribbon(aes(x=fake.date, ymin=mn-se, ymax=mn+se, fill=decade))+
  geom_line(aes(x=fake.date, y=mn, col=decade))


#"base years" based on Holtby
base.yrs = c(1950,1952,1957,1976,1977,1979,1985,1989,1995,1996,1998)

#filter df for base years
babine.extensions2 <- babine.all %>% 
  filter(Year %in% base.yrs)

# #current year:
 babine2021 <- read_csv("Daily.counts.all.Babine-copy.csv") %>% 
   select(date,CO)
          
 
 
#plot years together:              
plot.decadal.coho <- ggplot()+
  geom_ribbon(data=extension.by.decade,
              aes(x=fake.date, ymin=mn-se, ymax=mn+se, fill=decade),
              alpha = 0.5)+
  geom_line(data=babine2021,aes(x=date,y=CO),col="black",size=1)+
  scale_x_date(limits = c(as_date("2021-Aug-10"),as_date("2021-dec-05")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(x="Date",y="Daily Coho Count", fill="")+
  theme_babine4()
plot.decadal.coho

# ggsave(plot = plot.decadal.coho,filename = "plotdecadalcoho.png", device="png",
#        width = 6, height = 5)

#plot years together - holtby:              
ggplot()+
  geom_line(data=babine.extensions2,
            aes(x=fake.date,y=Count,col=as.factor(Year)),size=1)+
  geom_line(data=babine2021,aes(x=date,y=CO),col="black",size=1.5)+
  scale_x_date(limits = c(as_date("2021-Aug-10"),as_date("2021-nov-22")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="2021 compared to Base Years (Holtby)")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),title = element_text(size=6),
        legend.box = "vertical",
        plot.title=element_text(size=10))
  

#### Timing ####

#box plots of coho timing by year ####

#proportion of the run 
props <- c(0.1,0.25,0.5,0.75,0.9)

# closest<-function(data,value){
#   x <- data[which(abs(data$cumulprop-value)==min(abs(data$cumulprop-value))),]
#   x$date}
# 


x <- data.frame(Year=extension.yrs,start=as_date(NA),first.quart = as_date(NA), 
                median = as_date(NA),third.quart = as_date(NA), end = as_date(NA))

for (i in 1:length(extension.yrs)){
  data <- babine.extensions1 %>% 
    filter(Year %in% extension.yrs[i])
  x[i,"start"] <- data[first(which(abs(data$cumulprop-props[1])==min(abs(data$cumulprop-props[1])))),"Date"]
  x[i,"first.quart"] <- data[first(which(abs(data$cumulprop-props[2])==min(abs(data$cumulprop-props[2])))),"Date"]
  x[i,"median"] <- data[first(which(abs(data$cumulprop-props[3])==min(abs(data$cumulprop-props[3])))),"Date"]
  x[i,"third.quart"] <- data[first(which(abs(data$cumulprop-props[4])==min(abs(data$cumulprop-props[4])))),"Date"]
  x[i,"end"] <- data[first(which(abs(data$cumulprop-props[5])==min(abs(data$cumulprop-props[5])))),"Date"]
}

date.of.operation <- babine.extensions1 %>% 
  filter(Year %in% extension.yrs) %>% 
  group_by(Year) %>% 
  arrange(Date) %>% 
  summarize(first.day = Date[1], last.day = tail(Date,1)) %>% 
  mutate(last.julian = yday(last.day))


timing.extensionyrs <- x %>%
  pivot_longer(!Year, names_to="phase", values_to = "Date") %>% 
  mutate(julian = yday(Date)) 

tmp <- babine.extensions1 %>% 
  left_join(x, by="Year")  %>% 
  # mutate(period = ifelse(Date < start, "start",
  #                        ifelse(Date >= start & Date <= first.quart, "first.quart",
  #                               ifelse(Date >= first.quart & Date <= median, "median",
  #                                      ifelse(Date >= median & Date <= third.quart, "third.quart",
  #                                             ifelse(Date >= third.quart & Date <= end, "end",
  #                                                    ifelse(Date >= end, "final", NA))))))) %>%
  mutate(period = ifelse(Date < first.quart, "first quarter",
                         ifelse(Date >= first.quart & Date < third.quart, "middle half",
                             ifelse(Date >= third.quart, "last quarter", NA)))) %>%
  mutate(periodf = factor(period, levels=c("first quarter","middle half","last quarter"),
                          ordered=T))
tmp1 <- tmp %>% 
  filter(Year %in% c(1950:1989))
tmp2 <- tmp %>% 
  filter(Year %in% c(1990:1999))
tmp3 <- tmp %>% 
  filter(Year %in% c(2000:2021))

plot.dist.timing1 <- ggplot(data=tmp1)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  facet_wrap(~Year,scales = "free_y")+
  labs(x="Date",y="Daily Coho",fill="")+
  theme_babine4()
plot.dist.timing1
ggsave(plot=plot.dist.timing1, filename = "plot.dist.timing1.png",
       device="png",width=6, height=6)

plot.dist.timing2 <- ggplot(data=tmp2)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  facet_wrap(~Year,scales = "free_y")+
  labs(x="Date",y="Daily Coho",fill="")+
  theme_babine4()
plot.dist.timing2
ggsave(plot=plot.dist.timing2, filename = "plot.dist.timing2.png",
       device="png",width=6, height=6)

plot.dist.timing3 <- ggplot(data=tmp3)+
  geom_ribbon(aes(x=fake.date, ymin=0, ymax=Count, fill=periodf))+
  geom_line(aes(x=fake.date, y=Count), col="black")+
  facet_wrap(~Year,scales = "free_y")+
  labs(x="Date",y="Daily Coho",fill="")+
  theme_babine4()
plot.dist.timing3
ggsave(plot=plot.dist.timing3, filename = "plot.dist.timing3.png",
       device="png",width=6, height=6)

plot.timing.ext <- ggplot()+
  geom_boxplot(data=timing.extensionyrs,aes(x=Year, y=julian, group=Year),
               fill="gray50")+
  geom_point(data=date.of.operation, 
             aes(x=Year, y=last.julian), shape="-", size=2)+
  geom_text(aes(x=base.yrs,y=min(timing.extensionyrs$julian),label="*"))+
  scale_x_continuous(breaks=seq(1950,2021,2), labels = seq(1950,2021,2))+
  labs(x="Year", y="Julian Day")+
  theme_babine4()
  # theme(axis.text.x = element_text(angle=45, hjust=1),
  #          legend.position = "none")
plot.timing.ext

# ggsave(plot = plot.timing.ext, filename = "plot.timing.ext.png",
#        device="png", width=6, height=4)

yday(ymd("2021-10-15"))

# regressions testing changes over time: ####

#date when first 10% past the fence
start <- timing.extensionyrs %>% 
  filter(phase %in% "start")

(start.sum <- summary(lm(data = start, julian~as.numeric(Year))))

plot.start.timing <- ggplot(start)+
  geom_point(aes(x=as.integer(Year), y=julian))+
  geom_text(aes(x=1960,y=max(julian-5), label = 
                  paste("R-squ =",round(start.sum$r.squared,2),", p =",
                        round(start.sum$coeff[2,4], 2))))+
  geom_smooth(aes(x=as.integer(Year), y=julian), method="lm")+
  labs(title = "Start of Run", x="Year", y="Julian Day")+
  scale_x_continuous(breaks=seq(1950,2021,2), labels = seq(1950,2021,2))+
  #theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme_babine4()
plot.start.timing
# ggsave(plot=plot.start.timing, filename = "plot.start.timing.png",
#        device="png", width=6, height=4)


#date when half past the fence
median <- timing.extensionyrs %>% 
  filter(phase %in% "median")

(median.sum <- summary(lm(data = median, julian~as.numeric(Year))))


plot.med.timing <- ggplot(median)+
  geom_point(aes(x=as.integer(Year), y=julian))+
  geom_text(aes(x=1960,y=max(julian-5), label = 
                  paste("R-squ =",round(median.sum$r.squared,2),", p =",
                        round(median.sum$coeff[2,4], 2))))+
  geom_smooth(aes(x=as.integer(Year), y=julian), method="lm")+
  labs(title = "Median of Run", x="Year", y="Julian Day")+
  scale_x_continuous(breaks=seq(1950,2021,2), labels = seq(1950,2021,2))+
  #theme(axis.text.x = element_text(angle=45, hjust=1))+
  theme_babine4()
plot.med.timing
# ggsave(plot=plot.med.timing, filename="plot.med.timing.png", device="png",
#        width=6, height = 4)

# date when 90% of run has passed
end <- timing.extensionyrs %>% 
  filter(phase %in% "end")

(end.sum <- summary(lm(data = end, julian~as.numeric(Year))))

plot.end.timing <- ggplot(end)+
  geom_point(aes(x=as.integer(Year), y=julian))+
  geom_text(aes(x=1960,y=max(julian-5), label = 
                  paste("R-squ =",round(end.sum$r.squared,2),", p =",
                        round(end.sum$coeff[2,4], 2))))+
  geom_smooth(aes(x=as.integer(Year), y=julian), method="lm")+
  labs(title = "End of Run",x="Year", y="Julian Day")+
  scale_x_continuous(breaks=seq(1950,2021,2), labels = seq(1950,2021,2))+
  theme_babine4()
  #theme(axis.text.x = element_text(angle=45, hjust=1))
plot.end.timing
# ggsave(plot=plot.end.timing, filename = "plot.end.timing.png",
#        device="png", width=6, height=4)


#followup question - is end later because the fence ran later?
# may not be all that informative if so. Median more important.
# But since they both show this trend, is more believable. And the
# fence possibly ran late BECAUSE there were still coho





#### Coho expansion - Holtby's method ####

# calculate ave daily proportion of run for base years

#cut.off.holtby <- ymd("2021-10-13")

str(babine.extensions2) #remember that this includes only the base years from Holtby

#filter the base years to the julian day corresponding to Oct 13 and total run
total.base.byyr <- babine.extensions2 %>% 
  group_by(Year) %>% 
  filter(julian <= yday(ymd(paste0(Year,"-Oct-13")))) %>% #in Holtby, appear to cut the timing to Oct 13th for the total run
  summarize(total = sum(Count), last.julian = tail(julian,1))

base.yrs.df <- babine.extensions2 %>% 
  left_join(total.base.byyr) %>% 
  filter(julian <= last.julian) %>% 
  mutate(holtby.daily.prop = Count/total)

#hm... number of ways you could do this. 
  #could get ave count per day, then get daily proportion of total?
# below averages daily proportion in each year

ave.daily.base.yrs <- base.yrs.df %>% 
  group_by(fake.date) %>% 
  summarize(ave.daily.prop = mean(daily.prop)) %>% 
  mutate(cumul.prop = round(cumsum(ave.daily.prop),2))

#visualize
ggplot(ave.daily.base.yrs)+
  geom_line(data=base.yrs.df, aes(x=fake.date,y=daily.prop,col=as.character(Year)))+
  geom_line(aes(x=fake.date,y=ave.daily.prop))

#expand non-base years using base year proportions
    #base years from Holtby report
base.yrs = c(1950,1952,1957,1976,1977,1979,1985,1989,1995,1996,1998)
non.base.yrs <- setdiff(as.numeric(unique(babine194620$Year)),base.yrs)
non.base.yrs <- setdiff(non.base.yrs,1965) #removed 1965 since they estimate differently

#non base year df
babine.nonbase <- babine.all %>% 
  filter(Year %in% non.base.yrs) 

total.nonbase.byyr <- babine.nonbase%>% 
  arrange(Year, julian) %>% 
  group_by(Year) %>% 
  summarize(total = total.CO, last.julian = tail(julian,1),
            fake.date = tail(fake.date,1)) 

#only expand counts on years when counts did not go to Oct 13th (286)
expanded.yrs <- total.nonbase.byyr %>% 
  filter(last.julian <= 286) %>% 
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

# second2021 <- total.byyr %>% 
#   filter(Year > 2021.1) %>% 
#   mutate(Year = 2022, Year.lab = "2021 Actual")
  
therest <- total.byyr %>% 
  filter(Year <= 2021.1) %>% 
  mutate(Year = ifelse(Year %in% 2021.1, 2021, Year))

holtby20002021 <- ggplot(therest)+
  geom_col(aes(x=Year, y=est.total), fill="grey10") + 
  geom_col(aes(x=Year, y=total), fill="grey50") +
  #geom_col(data=second2021,aes(x=Year, y=total), fill="purple", col="black") +
  #geom_text(aes(x=2021,y=7900,label="est'd"),size=2)+
  #geom_text(aes(x=2022,y=8200,label="actual"),size=2)+
  #geom_text(aes(x=2021.5,y=9000,label="2021"),size=3)+
  # geom_hline(aes(yintercept=1200))+
  # geom_hline(aes(yintercept=11500),linetype="dashed")+
  scale_x_continuous(limits = c(1999,2023),breaks = seq(2000,2021,1))+
  scale_y_continuous(limits = c(0,max(therest$est.total)+5000))+
  labs(y="Coho Count (grey) and Expansion (black)",x="")+
  theme_classic()+theme(axis.text.x = element_text(hjust=1,angle=45))
holtby20002021  
        
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



#### Toboggan Fence ####

#extract Toboggan data

#2018
dates.rng.2018 <- excel_sheets("Toboggan Creek Fence Data 2018.xls")
(dates.rng.2018 <- dates.rng.2018[5:15])

tmp <- vector(mode="list", length=length(dates.rng.2018))

for (i in 1:length(dates.rng.2018)){
tmp[[i]] <- read_excel("Toboggan Creek Fence Data 2018.xls", 
                  sheet = dates.rng.2018[i],range = c("A3:AE14")) %>% 
  filter(!is.na(Date)) %>% 
  mutate(Date = as_date(Date),year = 2018, month = month(Date), 
         day=day(Date)) %>% 
  mutate(date = ymd(paste(year, month, day))) %>% 
  select(Date=date, wild.m = `Wild Male (WM)`,wild.f=`Wild Female (WF)`,
         hatch.m = `Hatchery Male (AM)`, hatch.f = `Hatchery Female (AF)`,
         witset.fishway.num =`Moricetown Fishway Tags`,
         witset.seine.num = `Moricetown Seign Net Tags`,
         ST = Steelhead, CH = Chinook, PK = Pink, trout = Trout,
         tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8,tag9,tag10,tag11,
         tag12,tag13,tag14,tag15)
  #mutate(year = year(Date), month = month(Date), day = day(Date))
}
head(tmp[[i]])
daily.count.2018 <- do.call(rbind, tmp)


#2019
dates.rng.2019 <- excel_sheets("Toboggan Creek Fence Data 2019.xlsx")
(dates.rng.2019 <- dates.rng.2019[5:16])

tmp <- vector(mode="list", length=length(dates.rng.2019))

for (i in 1:length(dates.rng.2019)){
  tmp[[i]] <- read_excel("Toboggan Creek Fence Data 2019.xlsx", 
                         sheet = dates.rng.2019[i],range = c("A3:AE14")) %>% 
    filter(!is.na(Date)) %>% 
    mutate(Date = as_date(Date),year = 2019, month = month(Date), 
           day=day(Date)) %>% 
    mutate(date = ymd(paste(year, month, day))) %>% 
    select(Date=date, wild.m = `Wild Male (WM)`,wild.f=`Wild Female (WF)`,
           hatch.m = `Hatchery Male (AM)`, hatch.f = `Hatchery Female (AF)`,
           witset.fishway.num =`Moricetown Fishway Tags`,
           witset.seine.num = `Moricetown Seign Net Tags`,
           ST = Steelhead, CH = Chinook, PK = Pink, trout = Trout,
           tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8,tag9,tag10,tag11,
           tag12,tag13,tag14,tag15)
}
head(tmp[[i]])
daily.count.2019 <- do.call(rbind, tmp)

#2020
dates.rng.2020 <- excel_sheets("Toboggan Creek Fence Data 2020.xlsx")
(dates.rng.2020 <- dates.rng.2020[5:17])

tmp <- vector(mode="list", length=length(dates.rng.2020))

for (i in 1:length(dates.rng.2020)){
  tmp[[i]] <- read_excel("Toboggan Creek Fence Data 2020.xlsx", 
                         sheet = dates.rng.2020[i],range = c("A3:AG14") ) %>% 
    filter(!is.na(Date)) %>% 
    mutate(Date = as_date(Date),year = 2020, month = month(Date), 
           day=day(Date)) %>% 
    mutate(date = ymd(paste(year, month, day)))%>% 
    select(Date=date, wild.m = `Wild Male (WM)`,wild.f=`Wild Female (WF)`,
           hatch.m = `Hatchery Male (AM)`, hatch.f = `Hatchery Female (AF)`,
           witset.fishway.num =`Moricetown Fishway Tags`,
           witset.seine.num = `Moricetown Seign Net Tags`,
           ST = Steelhead, CH = Chinook, PK = Pink, trout = Trout,
           tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8,tag9,tag10,tag11,
           tag12,tag13,tag14,tag15)
}
head(tmp[[i]])
daily.count.2020 <- do.call(rbind, tmp)




#2021
dates.rng.2021 <- excel_sheets("Toboggan Creek Fence Data 2021.xlsx")
(dates.rng.2021 <- dates.rng.2021[5:17])

tmp <- vector(mode="list", length=length(dates.rng.2021))

for (i in 1:length(dates.rng.2021)){
  tmp[[i]] <- read_excel("Toboggan Creek Fence Data 2021.xlsx", 
                         sheet = dates.rng.2021[i],range = c("A3:AH10") ) %>% 
    filter(!is.na(Date)) %>% 
    mutate(Date = as_date(Date),year = 2021, month = month(Date), 
           day=day(Date)) %>% 
    mutate(date = ymd(paste(year, month, day))) %>% 
    select(Date=date, wild.m = `Wild Male (WM)`,wild.f=`Wild Female (WF)`,
           hatch.m = `Hatchery Male (AM)`, hatch.f = `Hatchery Female (AF)`,
           witset.fishway.num =`Moricetown Fishway Tags`,
           witset.seine.num = `Moricetown Seign Net Tags`,
           ST = Steelhead, CH = Chinook, PK = Pink, trout = Trout,
           tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8,tag9,tag10,tag11,
           tag12,tag13,tag14,tag15)
}
head(tmp[[i]])
daily.count.2021 <- do.call(rbind, tmp)

#2018 to 2021 daily totals

#total caught versus marked

daily.count <- rbind(daily.count.2018, daily.count.2019, daily.count.2020, daily.count.2021)
str(daily.count)


total.marked.coho <- daily.count %>% 
  mutate(total.coho = wild.m+wild.f+hatch.m+hatch.f) %>% 
  mutate(marked1 = ifelse(is.na(witset.fishway.num),0,witset.fishway.num)) %>% 
  mutate(marked2 = ifelse(is.na(witset.seine.num),0,witset.seine.num)) %>% 
  mutate(total.marked = marked1+marked2) %>% 
  select(Date, total.coho, total.marked)

total.marked.coho

write_csv(total.marked.coho, "Toboggan.total-marked.coho.2018-2021.csv")



tags <- daily.count %>% 
  select(Date,tag1,tag2,tag3,tag4,tag5,tag6,tag7,tag8,tag9,tag10,tag11,
         tag12,tag13,tag14,tag15) %>% 
  pivot_longer(!Date, values_to = "tag",values_drop_na = TRUE) %>% 
  mutate(tag = ifelse(tag %in% "notread",NA,substr(tag,3,20)))


#export for use in witset MR script (witset.R):
write_csv(tags, "Toboggan.tagrecoveries.2018-2021.csv")
