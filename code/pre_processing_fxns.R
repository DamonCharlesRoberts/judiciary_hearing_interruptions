#### Judiciary Hearing Interruptions: Cleaning - Preprocessing and loading data ####

#### Notes: ####   
    ### Description: Script to turn pdf's to be readable. Goal is to get count of times nominees were interrupted (represented by the em dash). ###

#### Updates: ####
    ### By: dcr ###
    ### 2021/03/18 ###

#### Setup ####
box::use(
   here = here[here],
   pdftools = pdftools[pdf_text],
   tm = tm[...],
   dplyr = dplyr[...],
   quanteda = quanteda[...],
   tibble = tibble[...],
   tidyverse = tidyverse[...],
   readr = readr[...]
)
here::here()

##### transcriptsRead fxn ####
#folder <- "female_poc"

transcriptsRead <- function(folder){
filePath <- paste("data/", folder, sep = "")
fileVector <- list.files(filePath, full.names = TRUE)
#for (i in fileVector){
#  df <<- pdf_text(fileVector[i])
#}
df <- lapply(fileVector, pdf_text)
#new_list <- rlang::set_names(purrr::map2(df,names(df),  
#                    ~tibble(!!.y := .x)), stringr::str_c("df", 1:length(df)))

#df <- corpus(df[1])
for (i in 1:length(fileVector)){
df <- pdf_text(fileVector[i])
df <- data.frame(df)
df <- data.frame(df)
positions <- gregexpr ('(Senator|Justice|Judge|Mr.|Ms.|Mrs.) (\\b[A-Z]{2,}\\.)', df)[[1]]
#positions <- gregexpr('\\b[A-Z]+[A-ZA-Z]*(\\b\\s*\\b[A-ZA-Z]+[A-Za-z])*\\.', df)[[1]]
# regex detailed explanation (note \\ instead of \ because we need to escape "\" in R strings):
# \\b = word boundary
# [A-Z]+ any non-zero capitalized word or letter.
# =>   \\b[A-Z]+ -> any capitalizd word after a word boundary
# [A-Za-z]*\\b any sequence of letters (possibly length 0 because of *) ended by a word-boundary
# => \\b[A-Z]+[A-Za-z]*\\b  any word with one or more starting capitalized letters
# \\s* Any number of spaces between words.
# \\b[A-Za-z]*\\. any word ending with a dot.
# 
# => (\\b\\s*\\b[A-Za-z]*)* any number of potentially capitalized.
# 
# Translated:
# Match any sequence of words where the first word has a starting capital letter
# any all of the following words may be capitalized fully or partialyl
# End when you find a "."
match_lengths <- attr(positions, "match.length")
n_matches <- length(positions)
# Extract the matches
names <- substring(df, positions, positions + match_lengths - 1) # -1 because the "start + length = end + 1"
# Extract text in between matches.

text_starts <- positions + match_lengths
text_ends <- c(positions[-1] - 1, nchar(df))
text <- substring(df, 
                  text_starts, 
                  text_ends)

df <- data.frame(names = names, text = text) %>%
    mutate(male = NA) %>%
    mutate(poc = NA)

write_csv(df, paste("C:/Users/damon/Dropbox/judiciary_hearing_interruptions/data/", folder,"/", "cleaned_", i , ".csv", sep = ""))
}
}



