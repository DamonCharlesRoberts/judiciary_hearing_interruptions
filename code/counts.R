#### Judiciary Hearing Interruptions: Cleaning - Count of interruptions ####

#### Notes: ####   
### Description: Clean to get count of interruptions on nominees. Run pre_processing_script.R first ###

#### Updates: ####
### By: dcr ###
### 2021/03/23 ###

#### Setup ####
box::use(
  dplyr = dplyr[...],
  magrittr = magrittr[...],
  stringr = stringr[str_count],
  tidyr = tidyr[separate]
)

transcripts <- read_csv("data/analyzable_transcript.csv")

#### Get Count of interruptions (em dashes) ####

transcripts$count <- NA #create an empty count column
# get count of em-dashes and assign it to count column
transcripts$count <- str_count(transcripts$text, pattern = "—{2,}")
interruptionCounts <- aggregate(count~names+male+poc+`Birth Year`+`Court Type (1)`+`Hearing Date (1)` + `Party of Appointing President (1)` + `ABA Rating (1)` + `Judiciary Committee Action (1)` + `Appointing President (1)`, transcripts, sum) #aggregate by unique name the number of interruptions
#Merge the demographic data to the count data
#demographics <- interruptionCounts %>%
#  select(poc, male, names, `Court Type (1)`, `Hearing Date (1)`, `Party of Appointing President (1)`, `ABA Rating (1)`, `Judiciary Committee Action (1)`, `Appointing President (1)`)
#demographics <- demographics %>%
#  distinct(names, .keep_all = TRUE)
#interruptionCounts <- right_join(interruptionCounts, demographics, by = "names")

#Merge confirmation date information for time data
#time <- read_csv("data/judge_demographic_data.csv")
#time <- time %>%
#  rename(names = `Last Name`) 
#time$names <- str_to_upper(time$names)
#
#interruptionCountsTime <- left_join(interruptionCounts, time, by = "names")
#analyzable data
#interruptionCounts <- interruptionCountsTime


#### Cleaning ####
interruptionCounts$`Birth Year` <- as.numeric(interruptionCounts$`Birth Year`)

interruptionCounts$`Hearing Date (1)` <-  gsub('/', '-', interruptionCounts$`Hearing Date (1)`)
interruptionCounts <- interruptionCounts %>%
  separate(`Hearing Date (1)`, c("hearingmm", "hearingdd", "hearingYear"), sep = "-")
interruptionCounts$hearingYear <- as.numeric(interruptionCounts$hearingYear)
interruptionCounts <- interruptionCounts %>%
  mutate(age = hearingYear - `Birth Year`)

interruptionCounts <- interruptionCounts %>%
  mutate(republican = ifelse(`Party of Appointing President (1)` == "Republican", 1, 0))


interruptionCounts <- interruptionCounts %>%
  mutate(abaQualified = ifelse(`ABA Rating (1)` == "Not Qualified", 0, 
                               ifelse(`ABA Rating (1)` == "Qualified", 1, 
                                      ifelse(`ABA Rating (1)` == "Well Qualified", 2, 
                                             ifelse(`ABA Rating (1)` == "Exceptionally Well Qualified", 3, NA)))))


interruptionCounts <- interruptionCounts %>%
  mutate(favorably = ifelse(`Judiciary Committee Action (1)` == "Reported (no recommendation recorded)", 0 , 
                            ifelse(`Judiciary Committee Action (1)` == "Reported (favorably)", 1, NA)))


interruptionCounts <- interruptionCounts %>%
  mutate(court = ifelse(`Court Type (1)` == "Other", 0,
                        
                        ifelse(`Court Type (1)` == "U.S. District Court", 1, 
                               ifelse(`Court Type (1)` == "U.S. Court of Appeals", 2, NA))))


interruptionCounts <- interruptionCounts %>%
  mutate(president = ifelse(`Appointing President (1)` == "George W. Bush", 1, 
                            ifelse(`Appointing President (1)` == "Barack Obama", 2, 
                                   ifelse(`Appointing President (1)` == "Donald J. Trump", 3, NA))))

interruptionCounts <- interruptionCounts %>%
  mutate(division = ifelse(hearingYear == 2019 | hearingYear == 2020, 2, 
                           ifelse(hearingYear == 2017 | hearingYear == 2018, 1, 
                           ifelse(hearingYear == 2015 | hearingYear == 2016, 2, 
                                  ifelse(hearingYear == 2013 | hearingYear == 2014, -2, 
                                         ifelse(hearingYear == 2011 | hearingYear == 2012, -2, 
                                                ifelse(hearingYear == 2009 | hearingYear == 2010, -6, 
                                                       ifelse(hearingYear == 2007 | hearingYear == 2008, -2,
                                                              ifelse(hearingYear == 2005 | hearingYear == 2006, 1, 
                                                                     ifelse(hearingYear == 2003 | hearingYear == 2004, 1,
                                                                            ifelse(hearingYear == 2001 | hearingYear == 2002, -1, NA)))))))))))

interruptionCounts <- interruptionCounts %>%
  mutate(govdivided = ifelse(division > 0 & republican == 1, 0,
                             ifelse(division > 0 & republican == 0, 1,
                                    ifelse(division < 0 & republican == 1, 1,
                                           ifelse(division < 0 & republican == 0, 0, NA)))))

write_csv(interruptionCounts, "data/counts.csv")
