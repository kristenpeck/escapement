
#This script is to analyse the BC-16 data 
#Components include data clean up (mainly data/times),
# coho peak run timing estimators, and extracting info for 
# heli logistics

#Author: KPeck, started 24-Aug-2021

library(readxl)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(sf)
library(mapview)
library(leaflet)
library(tidyhydat)

#supress warnings for compiling
#oldw <- getOption("warn"); options(warn = -1)

#load data
bc16.BFraw <- read_excel("StreamInspection_BF2020.xlsx",
                         sheet = "StreamInspection")
bc16.0419raw <- read_excel("Stream_Inspection2004_2019.xlsx",
                           sheet = "StreamInspection")
streams<- read_excel("Streams.xlsx", sheet = "Streams") %>% 
  select(StreamId,StatArea,StreamName,REG_NAME)



#Fix date problems in Barry's 2020 data

bc16.BFbaddate <- bc16.BFraw %>% 
  filter(year(SilDate) != `Inspection Year`) %>% 
  mutate(Date = dmy(paste0(substr(as.character(SilDate), 
                                  start=3, stop = 10),"20"))) %>% 
  mutate(starttime = substr(StartTime,12,19), 
         startdatetime = ymd_hms(paste(Date, starttime))) %>%
  mutate(endtime = substr(StopTime,12,19), 
         enddatetime = ymd_hms(paste(Date, endtime))) 

bc16.BFgooddate <- bc16.BFraw %>% 
  filter(year(SilDate) == `Inspection Year`) %>% 
  mutate(Date = SilDate) %>% 
  mutate(starttime = substr(StartTime,12,19), 
         startdatetime = ymd_hms(paste(Date, starttime))) %>%
  mutate(endtime = substr(StopTime,12,19), 
         enddatetime = ymd_hms(paste(Date, endtime)))

bc16.BF <- rbind(bc16.BFbaddate,bc16.BFgooddate)




#remove data for other years with problem dates (may go through the bad ones later)
# Only using 2004-2019 because data format changed prior to 2004

bc16.gooddate <- bc16.0419raw %>% 
  filter(year(SilDate) == `Inspection Year`) %>% 
  mutate(Date = SilDate) %>% 
  mutate(starttime = substr(StartTime,12,19), 
         startdatetime = ymd_hms(paste(Date, starttime))) %>%
  mutate(endtime = substr(StopTime,12,19), 
         enddatetime = ymd_hms(paste(Date, endtime)))


# join BF 2020 data to the rest of the good data

bc16 <- bc16.gooddate %>% 
  rbind(bc16.BF) %>%
  mutate(duration = round(difftime(enddatetime,startdatetime, units="mins"),1)) %>% 
  mutate(yday = yday(Date)) %>% 
  left_join(streams,by=c("StreamID"="StreamId"))

#bc16[which(bc16$duration < 0),] #date time issues where duration calculates out as negative

BFstreams <- unique(bc16$StreamName[grep("finnega", bc16$Observer, ignore.case = T)])


#subset columns - exclude chum

bc16short <- bc16 %>% 
  select(SilID, StreamID, StatArea, StreamName, Year=`Inspection Year`,
         Date,yday,startdatetime,enddatetime,duration,Observer,
         Affiliation,PrimaryInspMode,StartBoundary,StopBoundary, 
         WaterColour,WaterBankfull,Brightness,Precipitation, StreamVisibility,
         
         Sock_Active_Spawning,Sock_AL_ObsTotal,
         Sock_AL_EstTotal,Sock_AL_EstReliability, Sock_AL_FishCountability,
         Coho_Active_Spawning,Coho_AL_Spawning,Coho_AL_ObsTotal,Coho_AL_EstTotal,Coho_AL_EstReliability, 
         Coho_AL_FishCountability,Pink_Active_Spawning,Pink_AL_ObsTotal,
         Pink_AL_EstTotal,Pink_AL_EstReliability, Pink_AL_FishCountability,
         Chum_Active_Spawning, Chum_AL_ObsTotal,Chum_AL_EstTotal,Chum_AL_EstReliability, 
         Chum_AL_FishCountability,Chinook_Active_Spawning, Chinook_AL_ObsTotal,Chinook_AL_EstTotal,
         Chinook_AL_EstReliability, Chinook_AL_FishCountability) %>% 
  mutate(fake.date = as_date(yday,origin="2021-01-01")) %>% 
  mutate(co.countability = recode(Coho_AL_FishCountability, F ="Fair", G="Good", 
                                  E="Excellent",High="Good", Nil ="Poor", 
                                  `500`="NA"))

#look through Barry's stream data

BFinspect <- bc16 %>% 
  filter(StreamName %in% BFstreams) %>% 
  select(-c(Month,Day,StartTime,StopTime,`Start Time`,`Stop Time`,REG_NAME,
            InpectDetailsAsPerNar))

write.csv(BFinspect, "BFinspect.csv", row.names = F, na = "")





#### BF Streams ####

# subset Area 4 and filter to only streams barry did

bc16.area4 <- bc16short %>% 
  filter(StreamName %in% BFstreams) %>% 
  filter(StatArea %in% "4") %>% 
  filter(Year >=2015) #%>% 
#filter(PrimaryInspMode %in% "Helicopter") %>% 

#number of SILs per year
table(bc16.area4$Year) 
as.data.frame(table(bc16.area4$StreamName))

# (date.stream <- bc16.area4 %>% 
#   group_by(Date) %>% 
#   summarize(yday=yday, stream = unique(StreamName), duration) %>% 
#     as.data.frame())

#write.csv(date.stream, "date.stream.csv", row.names = F)


# order stream visits per stream by year

bc16.ordered <- bc16.area4 %>% 
  arrange(yday) %>% 
  group_by(StreamID, Year) %>% 
  mutate(visit.num = c(1:length(StreamID)))


# bc16.ordered %>% 
#   group_by(StreamName) %>% 
#   summarize(unique(StreamID))


Babine.sock <- ggplot(data = bc16.ordered %>% filter(Year %in% c(2018,2019,2020), 
                                                     StreamID %in% c(463,464,465)))+
  geom_point(aes(x=as_date(fake.date),y=Sock_AL_ObsTotal, 
                 col=StreamName, shape=Sock_Active_Spawning), size=3)+
  geom_line(aes(x=as_date(fake.date),y=Sock_AL_ObsTotal, 
                col=StreamName, linetype=as.factor(Year)), size=1)+
  scale_x_date(limits = c(as_date("2021-aug-20"),as_date("2021-dec-15")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="Babine Sockeye",x="appr.date")+
  theme_dark()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=6),title = element_text(size=6),
        legend.box = "vertical")
Babine.sock

Babine.coho <- ggplot(data = bc16.ordered %>% filter(Year %in% c(2018,2019,2020), 
                                                     StreamID %in% c(463,464,465)))+
  geom_point(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal, 
                 col=StreamName, shape=Coho_Active_Spawning), size=3)+
  geom_line(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal, 
                col=StreamName, linetype=as.factor(Year)), size=1)+
  scale_x_date(limits = c(as_date("2021-aug-20"),as_date("2021-dec-15")),
               date_breaks= "1 week", date_labels = "%d%b")+
  labs(title="Babine Coho",x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=6),title = element_text(size=6),
        legend.box = "vertical")

Babine <- arrangeGrob(Babine.sock,Babine.coho,heights = c(6,10),nrow = 2, ncol=1)
plot(Babine)


# table of # counts per year by stream
bc16.ordered %>% 
  group_by(StreamName, Year) %>% 
  summarize(num.visits = max(visit.num, na.rm=T)) %>% 
  spread(Year, num.visits) %>% 
  as.data.frame()





#Barry stream groupings-kinda arbitrary based on location

group1 <- c("ZYMOETZ RIVER - UPPER","BULKLEY RIVER - UPPER",
            "MORICE RIVER","MORICE LAKE","TELKWA RIVER","GOSNELL CREEK",
            "NANIKA RIVER","ATNA RIVER AND LAKE","OWEN CREEK")
group2 <- c("BEAR RIVER","ASITKA LAKE","BEAR LAKE","SALIX CREEK",
            "AZUKLOTZ CREEK","MOTASE LAKE")
group3 <- c("MORRISON CREEK","TAHLO CREEK - (LOWER)",           
            "TAHLO CREEK - UPPER (SALMON CR.)","BABINE RIVER (SECTION 4)",        
            "BABINE RIVER (SECTION 5)","BABINE RIVER (SECTIONS 1 - 3)",
            "NILKITKWA RIVER","NICHYESKWA RIVER")
group4 <- c("SHASS CREEK","SUTHERLAND RIVER","PIERRE CREEK","TWAIN CREEK")
group5 <- c("KITSEGUECLA RIVER","TOUHY CREEK")

groups <- c(1:5)

str.group <- data.frame(StreamName=c(group1, group2, group3, group4, group5)) %>% 
  mutate(group=ifelse(StreamName %in% group1,1,
                      ifelse(StreamName %in% group2,2,
                             ifelse(StreamName %in% group3,3,
                                    ifelse(StreamName %in% group4,4,5)))))


# plot data based on group of streams

for(i in c(1:3,5)){
  BFstream.select <- bc16.area4 %>% 
    filter(StreamName %in% str.group$StreamName[which(str.group$group==i)],
           Coho_Active_Spawning %in% c("Start", "Peak","End")) 
  
  BFstream.select %>% 
    group_by(Year, StreamName) %>%
    summarize(peak = max(Coho_AL_ObsTotal, na.rm=T)) %>% 
    arrange(Year)
  
  plot <- ggplot(data=BFstream.select)+
    geom_point(aes(x=as.Date(fake.date),y=Coho_AL_Spawning,
                   col=StreamName, shape=Coho_Active_Spawning), size=2)+
    geom_line(aes(x=as.Date(fake.date),y=Coho_AL_Spawning,
                  col=StreamName))+
    facet_wrap(~Year, scales="free")+
    scale_x_date(limits = c(as_date("2021-aug-10"),as_date("2021-dec-15")),
                 date_labels = "%d-%b", date_breaks="10 days")+
    labs(x="appr. date", y="# Coho Observed", col="", shape="Est. Peak",
         title = paste("Streams =",paste(unique(BFstream.select$StreamName),
                                         collapse = ", ")))+
    theme_dark()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(size=8,angle =45, hjust=1),
          legend.text = element_text(size=6),title = element_text(size=6),
          legend.box = "vertical")+
    scale_shape_discrete(limits=c("Start","Peak", "End"))
  #not sure if this is working?                
  print(plot)  
  ggsave(plot = plot, filename = paste0("BFstreamgroup",groups[i],".png"), 
         height=6, width=10,device = "png", dpi=300)
}

BFstream.select


#Make interactive map of stream mouths - BF only streams

stream.loc <- read_excel("Streams.xlsx", sheet = "BFstreams")

stream.loc10 <- stream.loc %>% 
  filter(UTMZ %in% 10) %>% 
  st_as_sf(coords= c("UTME","UTMN"), crs = 3157) %>% 
  st_transform(crs=3005)

stream.loc9 <- stream.loc %>% 
  filter(UTMZ %in% 9) %>% 
  st_as_sf(coords= c("UTME","UTMN"), crs = 3156) %>% 
  st_transform(crs=3005)

stream.pts <- rbind(stream.loc9,stream.loc10)

mapview(stream.pts) 

















#### COHO peak timing ####

#summarize timing of peak counts per stream for coho

timing <- read_excel("Salmon Timing Info (Area 1-6).xlsx", 
                     sheet="Salmon_Timing_Info__Area_1_6_") %>% 
  filter(Species %in% "Coho")
coho.streams<- read_excel("Streams.xlsx", sheet = "CohoBaseline")


coho.timing <- coho.streams %>% 
  left_join(timing, by="StreamId") %>% 
  mutate(apprday = recode(StreamPeakSpawnDays,A="1",B="11",C="21")) %>% 
  mutate(apprdate = dmy(paste(apprday,StreamPeakSpawnMonth,"2021"))) %>% 
  mutate(apprdate.dm = format(apprdate,format="%d-%b")) %>% 
  select(StreamId, Stream, StreamName, PFMA,Species,
         StreamPeakSpawnMonth,StreamPeakSpawnDays,apprdate.dm) %>% 
  arrange(PFMA,apprdate.dm)

#this is the peak spawn timing according to table in BC16
#write.csv(coho.timing, "coho.timing_areas1-5.csv", row.names=F)

#the following is the peak spawning timing by year according to the SENs

coho.SEN<- read_excel("tblSEN.xlsx", sheet = "tblSEN") %>% 
  select(StreamID,Year,CohoArrivMonth:CohoEstType,
         AnnualEstRationale:UpdatedByDate)
names(coho.SEN)

SEN.timing <- coho.streams %>% 
  left_join(coho.SEN, by=c("StreamId"="StreamID")) %>% 
  mutate(apprday = recode(CohoPeakSpawnDay,A="1",B="11",C="21")) %>% 
  mutate(apprdate = dmy(paste(apprday,CohoPeakSpawnMonth,"2021"))) %>% 
  mutate(apprdate.dm = format(apprdate,format="%d-%b")) %>% 
  arrange(PFMA,Stream,apprdate.dm) %>% 
  filter(CohoAnnualEst != "N/I")

write.csv(SEN.timing, "SEN-based.timing.coho.csv", row.names = F, na="")




#plots of actual survey obs (all survey types) by statistical area

areas <- c("1","2E","2W","3","4","5")

for(i in 1:length(areas)){
  stream.select <- coho.timing %>% 
    filter(PFMA %in% areas[i]) 
  
  str.select <- bc16short %>%
    filter(StreamName %in% stream.select$StreamName, Year >= 2010) %>% 
    mutate(ydate = as_date(yday, origin = "2021-01-01")) %>% 
    mutate(ydate.dm = format(ydate,format="%d-%b"))
  
  str.select %>% 
    group_by(Year, StreamName) %>%
    summarize(peak = max(Coho_AL_ObsTotal, na.rm=T)) %>% 
    arrange(Year)
  
  plot <- ggplot(data=str.select)+
    geom_point(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                   col=StreamName, shape=Coho_Active_Spawning), size=2)+
    geom_line(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                  col=StreamName))+
    facet_wrap(~Year, scales="free_y")+
    scale_x_date(limits = c(as_date("2021-aug-10"),as_date("2021-dec-15")),
                 date_labels = "%d-%b", date_breaks="2 weeks")+
    labs(y="# Coho Observed", title = paste("StatArea=",str.select$StatArea),
         col="", x="appr.date")+
    theme_dark()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(size=8,angle =45, hjust=1),
          legend.text = element_text(size=8),legend.box = "vertical")
  print(plot)
}


# Area 1

bc16.area1 <- bc16short %>% 
  filter(StatArea %in% "1") 
str.area1 <- table(bc16.area1$StreamName) %>% 
  as.data.frame() %>% 
  filter(Freq>=40) %>% 
  select(StreamName = Var1) %>% 
  left_join(bc16short)


plot.area1 <- ggplot(data=bc16.area1)+
  geom_point(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                 col=StreamName, shape=Coho_Active_Spawning), size=2)+
  geom_line(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                col=StreamName))+
  facet_wrap(~Year, scales="free_y")+
  # scale_x_date(limits = c(as_date("2021-Jul-10"),as_date("2021-dec-15")),
  #              date_labels = "%d-%b", date_breaks="2 weeks")+
  labs(y="# Coho Observed", title = paste("StatArea=",str.area1$StatArea),
       col="", x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),legend.box = "vertical")
plot.area1
 ggsave(plot = plot.area1, filename = "area1coho.png", 
         height=6, width=10,device = "png", dpi=300)



# Area 2E

bc16.area2E <- bc16short %>% 
  filter(StatArea %in% "2E") 
str.area2E <- table(bc16.area2E$StreamName) %>% 
  as.data.frame() %>% 
  filter(Freq>=40) %>% 
  select(StreamName = Var1) %>% 
  left_join(bc16short)


plot.area2E <- ggplot(data=str.area2E)+
  geom_point(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                 col=StreamName, shape=Coho_Active_Spawning), size=2)+
  geom_line(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                col=StreamName))+
  facet_wrap(~Year, scales="free_y")+
  scale_x_date(limits = c(as_date("2021-Aug-10"),as_date("2021-dec-15")),
               date_labels = "%d-%b", date_breaks="2 weeks")+
  labs(y="# Coho Observed", title = paste("StatArea=",str.area2E$StatArea),
       col="", x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),legend.box = "vertical")
plot.area2E

 ggsave(plot = plot.area2E, filename = "area2Ecoho.png", 
        height=6, width=10,device = "png", dpi=300)


# Area 2W

bc16.area2W <- bc16short %>% 
  filter(StatArea %in% "2W") 
str.area2W <- table(bc16.area2W$StreamName) %>% 
  as.data.frame() %>% 
  filter(Freq>=25) %>% 
  select(StreamName = Var1) %>% 
  left_join(bc16short)


plot.area2W <- ggplot(data=str.area2W)+
  geom_point(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                 col=StreamName, shape=Coho_Active_Spawning), size=2)+
  geom_line(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                col=StreamName))+
  facet_wrap(~Year, scales="free_y")+
  # scale_x_date(limits = c(as_date("2021-Aug-10"),as_date("2021-dec-15")),
  #              date_labels = "%d-%b", date_breaks="2 weeks")+
  labs(y="# Coho Observed", title = paste("StatArea=",bc16.area2W$StatArea),
       col="", x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),
        legend.box = "vertical")
plot.area2W

 ggsave(plot = plot.area2W, filename = "area2Wcoho.png", 
        height=6, width=10,device = "png", dpi=300)


# Area 3

bc16.area3 <- bc16short %>% 
  filter(StatArea %in% "3") 
str.area3 <- table(bc16.area3$StreamName) %>% 
  as.data.frame() %>% 
  filter(Freq>=40) %>% 
  select(StreamName = Var1) %>% 
  left_join(bc16short) %>% 
  filter(Year >= 2010)


plot.area3 <- ggplot(data=str.area3)+
  geom_point(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                 col=StreamName, shape=Coho_Active_Spawning), size=2)+
  geom_line(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                col=StreamName))+
  facet_wrap(~Year, scales="free_y")+
  # scale_x_date(limits = c(as_date("2021-Aug-10"),as_date("2021-dec-15")),
  #              date_labels = "%d-%b", date_breaks="2 weeks")+
  labs(y="# Coho Observed", title = paste("StatArea=",bc16.area3$StatArea),
       col="", x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),
        legend.box = "vertical")
plot.area3

 ggsave(plot = plot.area3, filename = "area3coho.png", 
        height=6, width=10,device = "png", dpi=300)



# Area 5

bc16.area5 <- bc16short %>% 
  filter(StatArea %in% "5") 
str.area5 <- table(bc16.area5$StreamName) %>% 
  as.data.frame() %>% 
  filter(Freq>=40) %>% 
  select(StreamName = Var1) %>% 
  left_join(bc16short) %>% 
  filter(Year >= 2010)


plot.area5 <- ggplot(data=str.area5)+
  geom_point(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                 col=StreamName, shape=Coho_Active_Spawning), size=2)+
  geom_line(aes(x=as_date(fake.date),y=Coho_AL_ObsTotal,
                col=StreamName))+
  facet_wrap(~Year, scales="free_y")+
  scale_x_date(limits = c(as_date("2021-Aug-10"),as_date("2021-dec-15")),
               date_labels = "%d-%b", date_breaks="2 weeks")+
  labs(y="# Coho Observed", title = paste("StatArea=",bc16.area5$StatArea),
       col="", x="appr.date")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),
        legend.box = "vertical")
plot.area5

 ggsave(plot = plot.area5, filename = "area5coho.png", 
        height=6, width=10,device = "png", dpi=300)


#look at hydrology during counts

# this function will download a copy of HYDAT, but i will take some time, 
# and once done it doesn't need updating all the time 

#download_hydat()

# Skeena R above Babine: 08EB005
# Skeena R at Glen Vowell: 08EB003
# Driftwood Crk (Fraser drainage): 08JD006

hydro.skeena <- hy_daily_levels(station_number = c("08EB005","08EB003",
                                                   "08JD006")) %>% 
  mutate(year = year(Date), julian = yday(Date), 
         name=recode(STATION_NUMBER,`08EB005`="Skeena-Babine",
                     `08EB003`="Skeena-Gitxsan",`08JD006`="Driftwood")) %>% 
  dplyr::filter(year >= 2010) %>% 
  mutate(fyear = factor(year, order = T))


ggplot(data=hydro.skeena)+
  geom_line(aes(x=julian, y=Value, colour=name), size=1) + 
  facet_wrap(~year)




# Skeena R above Babine: 08EB005
# Skeena R at Glen Vowell: 08EB003

hydro.skeena <- hy_daily_levels(station_number = c("08EB005","08EB003")) %>% 
  mutate(year = year(Date), julian = yday(Date), 
         name=ifelse(STATION_NUMBER=="08EB005","Skeena-Babine",
                     "Skeena-Gitxsan")) %>% 
  dplyr::filter(year >= 2010) %>% 
  mutate(fyear = factor(year, order = T)) %>% 
  #filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
  #filter(!is.na(year))
  
  ggplot(data=hydro.skeena)+
  geom_line(aes(x=julian, y=Value, colour=name), size=1) + 
  facet_wrap(~year)



#re-activate warnings
#options(warn=oldw)

