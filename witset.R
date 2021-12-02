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
#   To fix manually!

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
#   tagged at the canyon 

#what is the longest time between tagging and recapture?
#are there any duplicate new tag numbers within year? Yes
#   Does colour need to be attached? Yes - done
anyDuplicated(witsetSK$newtags)


#any tags misrecorded as a new tag that should be a recap? (will need to 
# order the tags by date, then check each line if self is found in
# any preceding line)

#how many recaptures without a new tag record? Filter out.
(new.recaps <- witsetSK %>% 
  filter(year %in% 2021) %>% 
  filter(!is.na(recap.tag), 
         !(recap.tag %in% new.tag) ) )
#there are 10 recaptures without a matching tag number in 2021

#how many new tag numbers with no colour?
witsetSK %>% 
  filter(!is.na(AppliedTagNumber),is.na(AppliedColor))
#77 missing colour of tag

#how many tag colours with no tag number?
witsetSK %>% 
  filter(!is.na(AppliedColor),is.na(AppliedTagNumber))
#160 tags with no number, just a colour

#check if the tag status is recorded incorrectly
(no.tag.number <- witsetSK %>% 
  filter(TagStatus %in% c("A","A2"),
         is.na(AppliedTagNumber)&is.na(AppliedColor)))
#49 cases where tag status was recorded as a new tag but no tag number 
# or colour


  
#### SK Witset MR ####

#get closed LP estimate from fish tagged at the campground, 
# then recaptured at the canyon. Assume no tags lost (i.e. no fallback)

markedcampground21 <- witsetSK %>% 
  filter(year %in% 2021) %>% 
  filter(Location_Code %in% "Campground") %>% 
  filter(TagStatus %in% c("A","A2")) 
length(unique(markedcampground21$new.tag))

markedcampground21[anyDuplicated(markedcampground21$new.tag),"new.tag"]
#561 fish marked at campground in 2021 
#one duplicate new tag put out: yellow 5648 on Sept 1st, 2021. One was a VHF

# # of new tags put out in the canyon
newtagscanyon <- witsetSK %>% 
  filter(year %in% 2021) %>% 
  filter(Location_Code %in% "Canyon") %>% 
  #filter(TagStatus %in% c("A","A2")) %>% 
  filter(!is.na(new.tag))
length(unique(newtagscanyon$new.tag))

newtagscanyon[anyDuplicated(newtagscanyon$new.tag),"new.tag"]
#1773 fish marked at canyon in 2021
# one duplicate new tag put out: yellow 4901 on 27 July 2021. Already noted


# filter out new canyon tags from recaps at canyon
canyontagrecaps <- witsetSK %>% 
  filter(year %in% 2021) %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(TagStatus %in% c("AR","R")) %>% 
  filter(!(recap.tag %in% unique(newtagscanyon$new.tag)))
#53 fish recapped at canyon from campground in 2021

canyoncatch <- witsetSK %>% 
  filter(year %in% 2021) %>% 
  filter(Location_Code %in% "Canyon") %>% 
  filter(!(recap.tag %in% unique(newtagscanyon$new.tag)))

  
length(unique(markedcampground21$AppliedTagNumber))



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


