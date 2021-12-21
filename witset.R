# Witset MR

library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)

# # # # # # # # #
###### SOCK #####
# # # # # # # # #

# load exported query for SK

witsetSK.raw <- read_excel("QuerySKWitset.xlsx") 

unique(witsetSK.raw$`Recaptured Color`)

witsetSK <- witsetSK.raw %>% 
  mutate(tag.col=recode(AppliedColor,Orange="o",yellow="y",White="w",
                        Yellow="y",Green="g",`Light Green`="lt.g",
                        `Light Orange`="lt.o",Pink="p",`Lime Green`="lt.g",
                        Blue="b",Red="r")) %>% 
  mutate(recap.col=recode(`Recaptured Color`,Orange="o",yellow="y",
                          White="w",Yellow="y",Green="g",
                          `Light Green`="lt.g",`Light Orange`="lt.o",
                        Pink="p",`Lime Green`="lt.g",Blue="b",Red="r")) %>%
  mutate(year = year(Sample_Date),
         new.tag = ifelse(is.na(tag.col)&is.na(AppliedTagNumber),NA,
                          paste0(tag.col,"-",AppliedTagNumber))) %>% 
  mutate(recap.tag = ifelse(is.na(recap.col)&is.na(`Recaptured number`),NA,
                          paste0(recap.col,"-",`Recaptured number`)))
str(witsetSK)
unique(witsetSK$new.tag)

nanikaswim <- read_excel("NanikaSnorkel.xlsx") %>% 
  select(year=Year, nanika.counted=`total sockeye counted`,
         nanika.tags=`total tags observed`)
str(nanikaswim)

unique(witsetSK$TagStatus)


SKtotals <- witsetSK %>% 
    group_by(year) %>% 
    summarize(totalSK = length(year),
              harvested=length(which(Harvested %in% TRUE)),
              newtags = length(which(TagStatus %in% c("A","A2"))),
              recaps.witset = length(which(TagStatus %in% c("AR","R"))))%>% 
    left_join(nanikaswim)
SKtotals

SKbylocation <- witsetSK %>% 
  group_by(Location_Code, year) %>% 
  summarize(totalcaught = length(Location_Code), 
            harvested=length(which(Harvested %in% TRUE))) 
SKbylocation

#QA:

#note that the column Recaptured Color has 7 numbers in there, not just colors (mostly 2017)
#   To fix manually!- DONE

#how many fish recapped at campground?
witsetSK %>% 
  filter(year %in% 2021, Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("AR","R"))

#how many of those fish were marked at the canyon?
newtags.canyon <- witsetSK %>% 
  filter(year %in% 2021, Location_Code %in% "Canyon") %>% 
  summarize(uniq.tags=unique(AppliedTagNumber))
witsetSK %>% 
  filter(year %in% 2021, Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  filter(`Recaptured number` %in% newtags.canyon$uniq.tags)
#in 2021, 117 fish were recapped at campground, 39 of which were 
#   tagged at the canyon (33%)

#what is the longest time between tagging and recapture? TBD



#are there any duplicate new tag numbers within 2021? Yes
#   Does colour need to be attached? Yes - done with new.tag column

tmp <- witsetSK %>% 
  filter(!is.na(new.tag), year %in% 2021) 
tmp[duplicated(tmp$new.tag),] %>% 
  select(Sample_Date, Location_Code, Counter, new.tag)
#2021: y-4901 duplicate not fixable, the rest fixed


#how many recaptures without a new tag record? Filter out.
(new.recaps <- witsetSK %>% 
  filter(year %in% 2021) %>% 
  filter(!is.na(recap.tag), 
         !(recap.tag %in% new.tag) ) ) %>% 
  select(Sample_Date, Location_Code, Counter, recap.tag)
#there are 15 recaptures without a matching tag number in 2021
#canyon data not entered for SK on July 19th 2021- DONE
#need to fix database structure for A2 tags (so they don't get put in recaps)

#how many new tag numbers with no colour?
witsetSK %>% 
  filter(!is.na(AppliedTagNumber),is.na(AppliedColor), year %in% 2021)
#77 missing colour of tag, just 1 in 2021 - DONE

#how many tag colours with no tag number?
witsetSK %>% 
  filter(!is.na(AppliedColor),is.na(AppliedTagNumber), year %in% 2021)
#160 tags with no number, just a colour

#how many new tag numbers with no colour?
witsetSK %>% 
  filter(!is.na(AppliedTagNumber),is.na(AppliedColor), year %in% 2021)
#77 missing colour of tag, just 1 in 2021 - DONE

#how many recap tag colours with no tag number?
witsetSK %>% 
  filter(!is.na(`Recaptured Color`),is.na(`Recaptured number`), year %in% 2021)
#160 tags with no number, just a colour; 3 in 2021, all b/c recap tag number doesn't exist

#how many recap tag numbers with no colour?
witsetSK %>% 
  filter(!is.na(`Recaptured number`),is.na(`Recaptured Color`), year %in% 2021)
#none in 2021

#check if the tag status is recorded incorrectly
(no.tag.number <- witsetSK %>% 
  filter(TagStatus %in% c("A","A2"),
         is.na(AppliedTagNumber)&is.na(AppliedColor))) %>% 
  filter(year %in% 2021) %>% 
  select(Counter)
#49 cases where tag status was recorded as a new tag but no tag number 
# or colour. Just 1 in 2021- fixed


  
#### SK Witset MR ####

#get closed LP estimate from fish tagged at the campground, 
# then recaptured at the canyon. Assume no tags lost (i.e. no fallback)
#year.select <- 2017

markedcampground <- witsetSK %>% 
  #filter(year %in% year.select) %>% 
  filter(Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("A","A2")) 
#length(unique(markedcampground$new.tag))

#markedcampground[anyDuplicated(markedcampground$new.tag),"new.tag"]
#562 fish marked at campground in 2021 


# # of new tags put out in the canyon
newtagscanyon <- witsetSK %>% 
  #filter(year %in% year.select) %>% 
  filter(Location_Code %in% "Canyon") %>% 
  #filter(TagStatus %in% c("A","A2")) %>% 
  filter(!is.na(new.tag))
#length(unique(newtagscanyon$new.tag))

#newtagscanyon[anyDuplicated(newtagscanyon$new.tag),"new.tag"]
#1788 fish marked at canyon in 2021
# one duplicate new tag put out: yellow 4901 on 27 July 2021. Already noted in db


# filter out new canyon tags from recaps at canyon
canyontagrecaps <- witsetSK %>% 
  #filter(year %in% year.select) %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  filter(!(recap.tag %in% unique(newtagscanyon$new.tag)))
#49 fish recapped at canyon from campground in 2021

#total canyon catch excluding canyon tags
canyoncatch <- witsetSK %>% 
  #filter(year %in% year.select) %>% 
  filter(Location_Code %in% "Canyon") 
%>% 
  filter(!(recap.tag %in% unique(newtagscanyon$new.tag)))
#2452 SK captured at canyon. Should exclude other portions of catch?
  

#LP estimate for campground -> canyon

#marked in campground
(m <- markedcampground %>% 
  group_by(year) %>% 
  summarize(marked = length(Sample_Date)))

#recapped at canyon
(r <- canyontagrecaps %>% 
    group_by(year) %>% 
    summarize(recapped = length(Sample_Date)))

#total unique fish caught at canyon
(c <- canyoncatch %>% 
    group_by(year) %>% 
    summarize(total.catch = length(Sample_Date)))


LP <- m %>% 
  left_join(r) %>% 
  left_join(c) %>% 
  mutate(LP = marked*total.catch/recapped)
LP

ggplot(data=LP)+
  geom_line(aes(x=year, y=LP))+
  geom_point(aes(x=year, y=LP))+
  labs(y="LP estimated population size (SK)")










# # # # # # # # #
###### COHO #####
# # # # # # # # #

witsetCO <- read_excel("QueryCOWitset.xlsx") %>% 
  mutate(year = year(Sample_Date),
         tag.col = substr(AppliedColor,1,1),
         new.tag = ifelse(is.na(tag.col),NA,
                          paste0(tag.col,"-",AppliedTagNumber))) %>% 
  mutate(recap.col = substr(`Recaptured Color`,1,1),
         recap.tag = ifelse(is.na(recap.col),NA,
                            paste0(recap.col,"-",`Recaptured number`)))
str(witsetCO)

# look at total harvested, tagged, and recaptured by year
COtotals <- witsetCO %>% 
  group_by(year) %>% 
  summarize(totalcaught = length(year),
            harvested=length(which(Harvested %in% TRUE)),
            newtags = length(which(TagStatus %in% c("A","A2"))),
            recaps.witset = length(which(TagStatus %in% c("AR","R"))))
COtotals

CObylocation <- witsetCO %>% 
  group_by(Location_Code, year) %>% 
  summarize(totalcaught = length(Location_Code), 
            harvested=length(which(Harvested %in% TRUE))) 
CObylocation


#toboggan
# c(Year, `No. Wild in fence count`, `No. Hatchery in fence count`,
#   `Total estimated return`, `Total estimated wild return`,
#   `Total estimated hatchery return`)
# c(2021, 


