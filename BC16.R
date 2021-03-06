
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
bc16.9803raw <- read_excel("Stream_Inspection1998_2003.xlsx", 
                           sheet= "Stream_Inspection1998_2003")


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
# only from 2004-2019

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
         Sock_AL_Hold,Sock_AL_Spawning,Sock_AL_ObsTotal,
         Sock_AL_EstTotal,Sock_AL_EstReliability, Sock_AL_FishCountability,
         Coho_AL_Hold,Coho_AL_Spawning,Coho_AL_ObsTotal,Coho_AL_EstTotal,Coho_AL_EstReliability, 
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

#write.csv(BFinspect, "BFinspect.csv", row.names = F, na = "")





#### BF Streams ####

# subset Area 4 and filter to only streams barry did

bc16.area4 <- bc16short %>% 
  filter(StreamName %in% BFstreams) #%>%
  #filter(StatArea %in% "4") #%>%
 # filter(Year >=2015) #%>% 
#filter(PrimaryInspMode %in% "Helicopter") %>% 

#number of SILs per year
table(bc16.area4$Year) 
as.data.frame(table(bc16.area4$StreamName))

# (date.stream <- bc16.area4 %>% 
#   group_by(Date) %>% 
#   summarize(yday=yday, stream = unique(StreamName), duration) %>% 
#     as.data.frame())

#write.csv(date.stream, "date.stream.csv", row.names = F)


# order stream visits per stream by day of the year

bc16.ordered <- bc16.area4 %>% 
  arrange(yday) %>% 
  group_by(StreamID, Year) %>% 
  mutate(visit.num = c(1:length(StreamID)))


# bc16.ordered %>% 
#   group_by(StreamName) %>% 
#   summarize(unique(StreamID))

#look at sockeye and coho timing in Babine sections 1-5
Babine.sock <- ggplot(data = bc16.ordered %>% filter(Year %in% c(2018,2019,2020), 
                                                     StreamID %in% c(463,464,465)))+
  geom_point(aes(x=as_date(fake.date),y=Sock_AL_ObsTotal, 
                 col=StreamName, shape=Sock_Active_Spawning), size=3)+
  geom_line(aes(x=as_date(fake.date),y=Sock_AL_ObsTotal, 
                col=StreamName, linetype=as.factor(Year)), size=1)+
  scale_x_date(limits = c(as_date("2021-aug-20"),as_date("2021-dec-15")),
               date_breaks= "1 week", date_labels = "%d%b")+
  scale_shape_discrete(limits=c("Before","Start","Peak", "End","NA"))+
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
               date_breaks= "1 week", date_labels = "%d-%b")+
  scale_shape_discrete(limits=c("Before","Start","Peak", "End","NA"))+
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

group1 <- c("BULKLEY RIVER - UPPER",
            "MORICE RIVER","MORICE LAKE","TELKWA RIVER","GOSNELL CREEK",
            "NANIKA RIVER","ATNA RIVER AND LAKE","OWEN CREEK")
group2 <- c("BEAR RIVER","ASITKA LAKE","BEAR LAKE","SALIX CREEK",
            "AZUKLOTZ CREEK","MOTASE LAKE")
group3 <- c("MORRISON CREEK","TAHLO CREEK - (LOWER)",           
            "TAHLO CREEK - UPPER (SALMON CR.)","BABINE RIVER (SECTION 4)",        
            "BABINE RIVER (SECTION 5)","BABINE RIVER (SECTIONS 1 - 3)",
            "NILKITKWA RIVER","NICHYESKWA RIVER")
group4 <- c("SHASS CREEK","SUTHERLAND RIVER","PIERRE CREEK","TWAIN CREEK")
group5 <- c("ZYMOETZ RIVER - UPPER","KITSEGUECLA RIVER","TOUHY CREEK")

groups <- data.frame(group = c(1:5),name=c("Bulkley-Morice","Bear",
                                            "Lower Babine","Upper Babine",
                                            "Zymoetz-Kitseg-Touhy"))

str.group <- tibble(StreamName=c(group1, group2, group3, group4, group5)) %>% 
  mutate(group=ifelse(StreamName %in% group1,1,
                      ifelse(StreamName %in% group2,2,
                             ifelse(StreamName %in% group3,3,
                                    ifelse(StreamName %in% group4,4,5))))) %>% 
  left_join(groups)
  
  


# plot data based on group of streams

for(i in c(1:3,5)){
  BFstream.select <- bc16.area4 %>% 
    filter(StreamName %in% str.group$StreamName[which(str.group$group==i)],
           Coho_Active_Spawning %in% c("Before","Start", "Peak","End"),
           Year >=2010) 
  
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
    scale_shape_discrete(limits=c("Before","Start","Peak", "End","NA"))               
  print(plot)  
  ggsave(plot = plot, filename = paste0("BFstreamgroup",groups[i],".png"), 
         height=6, width=10,device = "png", dpi=300)
}

BFstream.select


#### Maps ####

#Make interactive map of stream mouths - BF only streams

stream.loc <- read_excel("Streams.xlsx", sheet = "BFstreams") %>% 
  left_join(streams, by=c("stream" = "StreamName"))

stream.loc10 <- stream.loc %>% 
  filter(UTMZ %in% 10) %>% 
  st_as_sf(coords= c("UTME","UTMN"), crs = 3157) %>% 
  st_transform(crs=3005)

stream.loc9 <- stream.loc %>% 
  filter(UTMZ %in% 9) %>% 
  st_as_sf(coords= c("UTME","UTMN"), crs = 3156) %>% 
  st_transform(crs=3005)

stream.pts.bf <- rbind(stream.loc9,stream.loc10) %>% 
  mutate(cat="BFstreams") %>% 
  select(PFMA=StatArea,stream,geometry,cat)

#mapview(stream.pts.bf) 


#Make interactive map of stream mouths - Coho baseline streams

stream.loc <- read_excel("Streams.xlsx", sheet = "CohoBaseline")

stream.loc10 <- stream.loc %>% 
  filter(UTMZ %in% 10) %>% 
  st_as_sf(coords= c("UTME","UTMN"), crs = 3157) %>% 
  st_transform(crs=3005)

stream.loc9 <- stream.loc %>% 
  filter(UTMZ %in% 9) %>% 
  st_as_sf(coords= c("UTME","UTMN"), crs = 3156) %>% 
  st_transform(crs=3005)

stream.pts.coho <- rbind(stream.loc9,stream.loc10) %>% 
  mutate(cat = "baseline2021") %>% 
  select(PFMA,stream=Stream,geometry,cat)

#mapview(stream.pts.coho)

#join the two
stream.pts <- rbind(stream.pts.bf,stream.pts.coho)

mapviewOptions(vector.palette = colorRampPalette(c("red", "blue")))
mapview(list(stream.pts), zcol="cat")


#geo groupings:
upper <- c("Damdochax River","Upper Skeena (Above Kluatantan)",
           "Chipmunk Creek","Mosque River")















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

#write.csv(SEN.timing, "SEN-based.timing.coho.csv", row.names = F, na="")




#plots of actual survey obs (all survey types) by statistical area

areas <- c("1","2E","2W","3","4","5")

for(i in 1:length(areas)){
  stream.select <- coho.timing %>% 
    filter(PFMA %in% areas[i]) 
  
  str.select <- bc16short %>%
    filter(StreamName %in% stream.select$StreamName, Year >= 2010) %>% 
    mutate(fake.date = as_date(yday, origin = "2021-01-01")) 
  
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
    scale_shape_discrete(limits=c("Before","Start","Peak", "End","NA"))+
    labs(y="# Coho Observed", title = paste("StatArea=",str.select$StatArea),
         col="", x="appr.date")+
    theme_dark()+
    theme(legend.position = "bottom",
          axis.text.x = element_text(size=8,angle =45, hjust=1),
          legend.text = element_text(size=8),legend.box = "vertical")
  print(plot)
  ggsave(plot = plot, filename = paste0("area",areas[i],"coho.png"), 
         height=6, width=10,device = "png", dpi=300)
}





#### 2020 timing BF ####
str.group

BF2020 <- bc16.area4 %>% 
  filter(Year %in% 2020, PrimaryInspMode %in% c("Helicopter")) %>% 
  left_join(str.group)
str(BF2020)

ggplot(data = BF2020) +
  geom_point(aes(x=as_date(Date), y=Coho_AL_ObsTotal, col=name), shape=19, size=2)+
  geom_line(aes(x=as_date(Date), y=Coho_AL_ObsTotal, col=StreamName))+
  geom_point(aes(x=as_date(Date), y=Sock_AL_ObsTotal, col=name), shape=2,  size=2)+
  geom_line(aes(x=as_date(Date), y=Sock_AL_ObsTotal, col=StreamName))+
  scale_x_date(limits = c(as_date("2020-Sep-1"),as_date("2020-dec-15")),
               date_labels = "%d-%b", date_breaks="2 weeks")+
  facet_wrap(~name,scales = "free_y")+
  theme_dark()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),legend.box = "vertical")


#individually by group
for(i in 1:nrow(groups)){
  
group2020plot <- ggplot(data = BF2020[BF2020$group %in% groups[i,1],]) +
  geom_point(aes(x=as_date(Date), y=Coho_AL_ObsTotal, col=StreamName), shape=19, size=2)+
  geom_line(aes(x=as_date(Date), y=Coho_AL_ObsTotal, col=StreamName))+
  #geom_point(aes(x=as_date(Date), y=Sock_AL_ObsTotal, col=StreamName), shape=2,  size=2)+
  #geom_line(aes(x=as_date(Date), y=Sock_AL_ObsTotal, col=StreamName))+
  scale_x_date(limits = c(as_date("2020-Aug-1"),as_date("2020-dec-15")),
               date_labels = "%d-%b", date_breaks="2 weeks")+
  #facet_wrap(~name,scales = "free_y")+
  labs(title = paste("BF2020",groups$name[i]), y="Tot.Observed Coho (circles) and Sockeye (triangles)",
       x="Date",col="Stream")+
  theme_dark()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size=8,angle =45, hjust=1),
        legend.text = element_text(size=8),legend.box = "vertical")
  ggsave(plot = group2020plot, filename = paste0("BF2020",groups$name[i],".png"),
         height=6, width=10,device = "png", dpi=300)
}





#### Hydro stations ####
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
  mutate(fyear = factor(year, order = T))# %>%
  #filter(julian %in% c(yday(ymd("2021-aug-01")),yday(ymd("2021-dec-01"))) ) %>% 
  #filter(!is.na(year))
  
ggplot(data=hydro.skeena)+
  geom_line(aes(x=julian, y=Value, colour=name), size=1) + 
  facet_wrap(~year)




#re-activate warnings
#options(warn=oldw)

