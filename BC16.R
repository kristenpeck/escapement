
#this script is to analyse the BC-16 data from Barry Finnegan

library(readxl)
library(tidyverse)
library(lubridate)


bc16.raw <- read_excel("StreamInspection_BF2020.xlsx",sheet = "StreamInspection1")
streams<- read_excel("Streams.xlsx", sheet = "Streams") %>% 
  select(StreamId,StatArea,StreamName,REG_NAME)

str(bc16.raw)
names(bc16.raw)



bc16 <- bc16.raw %>% 
  mutate(Date = dmy(paste0(substr(as.character(SilDate), 
                                  start=3, stop = 10),"20"))) %>% 
  mutate(starttime = substr(StartTime,12,19), 
         startdatetime = ymd_hms(paste(Date, starttime))) %>%
  mutate(endtime = substr(StopTime,12,19), 
         enddatetime = ymd_hms(paste(Date, endtime))) %>%
  mutate(duration = enddatetime-startdatetime) %>% 
  left_join(streams,by=c("StreamID"="StreamId"))
names(bc16)
bc16$enddatetime


bc16short <- bc16 %>% 
  select(SilID,StreamID,StatArea,StreamName,Date,startdatetime,enddatetime,duration,
         PrimaryInspMode,StartBoundary, StopBoundary, WaterColour,WaterBankfull,
         Brightness,Precipitation, StreamVisibility,Sock_AL_ObsTotal,
         Sock_AL_EstTotal,Sock_AL_EstReliability, Sock_AL_FishCountability,
         Coho_AL_ObsTotal,Coho_AL_EstTotal,Coho_AL_EstReliability, 
         Coho_AL_FishCountability,Pink_AL_ObsTotal,
         Pink_AL_EstTotal,Pink_AL_EstReliability, Pink_AL_FishCountability,
         Chum_AL_ObsTotal,Chum_AL_EstTotal,Chum_AL_EstReliability, 
         Chum_AL_FishCountability,Chinook_AL_ObsTotal,Chinook_AL_EstTotal,
         Chinook_AL_EstReliability, Chinook_AL_FishCountability) 



