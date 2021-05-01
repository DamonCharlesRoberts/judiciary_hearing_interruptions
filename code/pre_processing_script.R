#### Judiciary Hearing Interruptions: Cleaning - loading data ####

#### Notes: ####   
### Description: Script to turn pdf's to be readable. Goal is to get count of times nominees were interrupted (represented by the em dash). ###

#### Updates: ####
### By: dcr ###
### 2021/03/23 ###

#### Setup ####
source("code/pre_processing_fxns.R")

box::use(
  plyr = plyr[ldply],
  readr = readr[read_csv],
  stringr = stringr[str_to_upper, str_remove, str_detect],
  tidyr = tidyr[separate]
)

demographics <- read_csv("data/judge_demographic_data.csv") %>%
  rename(names = `Last Name`)
demographics$names <- str_to_upper(demographics$names)
demographics <- demographics %>%
  filter(`Appointing President (1)` == "George W. Bush" | `Appointing President (1)` == "Barack Obama" | `Appointing President (1)` == "Donald J. Trump")

#### White Males ####
male_white <- transcriptsRead("male_white") # execute function to convert pdfs to csvs with line of transcript per row and column for speaker

filePath <- list.files(path = "data/male_white", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

male_white <- ldply(filePath, read_csv) #combine all csv's in file path defined above into one dataframe

# Code the male column as 1 for all in dataset and 0 for the poc column
male_white <- male_white %>% 
  mutate(male = 1) %>%
  mutate(poc = 0)


male_white$names <- str_remove(male_white$names, "\\.") # Remove periods from names column for filtering

male_white <- male_white %>%
  separate(names, c("title", "names"), sep = " ") #Create title and name column

male_whiteFiltered <- male_white %>%
  filter(title != "Senator") #filter out all senators

male_whiteFiltered <- male_whiteFiltered %>%
  filter(names %in% demographics$names) #Filter out committee member's conversations. Only include text for nominee - can use this to only look at instances of nominees being interrupted
  

#### White Females ####
female_white <- transcriptsRead("female_white") # execute function to convert pdfs to csvs with line of transcript per row and column for speaker

filePath2 <- list.files(path = "data/female_white", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

female_white <- ldply(filePath2, read_csv) #combine all csv's in file path defined above into one dataframe

# Code the male column as 1 for all in dataset and 0 for the poc column
female_white <- female_white %>% 
  mutate(male = 0) %>%
  mutate(poc = 0)


female_white$names <- str_remove(female_white$names, "\\.") # Remove periods from names column for filtering

female_white <- female_white %>%
  separate(names, c("title", "names"), sep = " ") #Create title and name column

female_whiteFiltered <- female_white %>%
  filter(title != "Senator") #filter out all senators

female_whiteFiltered <- female_whiteFiltered %>%
  filter(names %in% demographics$names) #Filter out committee member's conversations. Only include text for nominee - can use this to only look at instances of nominees being interrupted


#### POC Males ####
male_poc <- transcriptsRead("male_poc") # execute function to convert pdfs to csvs with line of transcript per row and column for speaker

filePath3 <- list.files(path = "data/male_poc", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

male_poc <- ldply(filePath3, read_csv) #combine all csv's in file path defined above into one dataframe

# Code the male column as 1 for all in dataset and 0 for the poc column
male_poc <- male_poc %>% 
  mutate(male = 1) %>%
  mutate(poc = 1)


male_poc$names <- str_remove(male_poc$names, "\\.") # Remove periods from names column for filtering

male_poc <- male_poc %>%
  separate(names, c("title", "names"), sep = " ") #Create title and name column
male_pocFiltered <- male_poc %>%
  filter(title != "Senator") #filter out all senators

male_pocFiltered <- male_pocFiltered %>%
  filter(names %in% demographics$names) #Filter out committee member's conversations. Only include text for nominee - can use this to only look at instances of nominees being interrupted


#### POC Females ####
female_poc <- transcriptsRead("female_poc") # execute function to convert pdfs to csvs with line of transcript per row and column for speaker

filePath4 <- list.files(path = "data/female_poc", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

female_poc <- ldply(filePath4, read_csv) #combine all csv's in file path defined above into one dataframe

# Code the male column as 1 for all in dataset and 0 for the poc column
female_poc <- female_poc %>% 
  mutate(male = 0) %>%
  mutate(poc = 1)


female_poc$names <- str_remove(female_poc$names, "\\.") # Remove periods from names column for filtering

female_poc <- female_poc %>%
  separate(names, c("title", "names"), sep = " ") #Create title and name column
female_pocFiltered <- female_poc %>%
  filter(title != "Senator") #filter out all senators

female_pocFiltered <- female_pocFiltered %>%
  filter(names %in% demographics$names) #Filter out committee member's conversations. Only include text for nominee - can use this to only look at instances of nominees being interrupted


#### Merging ####

transcripts <- rbind(male_whiteFiltered, male_pocFiltered, female_whiteFiltered, female_pocFiltered)

transcripts <- right_join(transcripts, demographics, by = "names")
anti <- anti_join(transcripts, demographics, by = "names")

write_csv(transcripts, "data/analyzable_transcript.csv")

