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
  stringr = stringr[str_count]
)

transcripts <- read_csv("data/analyzable_transcript.csv")

#### Get Count of interruptions (em dashes) ####

transcripts$count <- NA #create an empty count column
# get count of em-dashes and assign it to count column
transcripts$count <- str_count(transcripts$text, pattern = "—{2,}")
interruptionCounts <- aggregate(count~names+male+poc+`Birth Year`+`Court Type (1)`+`Hearing Date (1)`, transcripts, sum) #aggregate by unique name the number of interruptions
#Merge the demographic data to the count data
#demographics <- transcripts %>%
#  select(poc, male, names)
#demographics <- demographics %>%
#  distinct(names, .keep_all = TRUE)
#interruptionCounts <- right_join(interruptionCounts, demographics, by = #"names") %>%
#  rename(counts = x)
#
#Merge confirmation date information for time data
#time <- read_csv("data/judge_demographic_data.csv")
#time <- time %>%
#  rename(names = `Last Name`) 
#time$names <- str_to_upper(time$names)
#
#interruptionCountsTime <- left_join(interruptionCounts, time, by = "names")
#analyzable data

#### Cleaning ####
interruptionCounts$`Birth Year` <- as.numeric(interruptionCounts$`Birth Year`)

interruptionCounts$`Hearing Date (1)` <-  gsub('/', '-', interruptionCounts$`Hearing Date (1)`)
interruptionCounts <- interruptionCounts %>%
  separate(`Hearing Date (1)`, c("hearingmm", "hearingdd", "hearingYear"), sep = "-")
interruptionCounts$hearingYear <- as.numeric(interruptionCounts$hearingYear)
interruptionCounts <- interruptionCounts %>%
  mutate(age = hearingYear - `Birth Year`)

write_csv(interruptionCounts, "data/counts.csv")
