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
)

#### Working Directory ####
here::here()
write_csv(df[i], paste("C:/Users/damon/Dropbox/judiciary_hearing_interruptions/data/", "sorted_", 1:length(df), ".csv", sep = ""))
#### 
    ### Pull in Documents ###

fileVector <- list.files(path = "data")
# corpus_raw <- data.frame("folder" = c(),"text" = c())
# for (i in 1:length(pdf_list)){
# print(i)
#  pdf_text(paste("data/", pdf_list[i],sep = "")) %>% 
#  strsplit("\n")-> document_text
# data.frame("folder" = gsub(x =pdf_list[i],pattern = ".pdf", replacement = ""), 
#  "text" = document_text, stringsAsFactors = FALSE) -> document
# colnames(document) <- c("folder", "text")
# corpus_raw <- rbind(corpus_raw,document) 
# }
    ### Sample ###
#strsplitcustom <- function(x,
#                     split,
#                     type = "remove",
#                     perl = FALSE,
#                     ...) {
#  if (type == "remove") {
#    # use base::strsplit
#    out <- base::strsplit(x = x, split = split, perl = perl, ...)
#  } else if (type == "before") {
#    # split before the delimiter and keep it
#    out <- base::strsplit(x = x,
#                          split = paste0("(?<=.)(?=", split, ")"),
#                          perl = TRUE,
#                          ...)
#  } else if (type == "after") {
#    # split after the delimiter and keep it
#    out <- base::strsplit(x = x,
#                          split = paste0("(?<=", split, ")"),
#                          perl = TRUE,
#                          ...)
#  } else {
#    # wrong type input
#    stop("type must be remove, after or before!")
#  }
#  return(out)
#}

#pattern <- "[A-Z][a-zA-Z]+( [A-Z][a-zA-Z]+)*"

male_white <- pdf_text("data/male_white/c115_kavanaugh.pdf")
male_white <- corpus(male_white)
male_white <- data.frame(male_white)
male_white <- male_white[-c(1:22),]
male_white <- data.frame(male_white) %>%
  gsub("D.C.", "d.c.", .) %>%
  gsub("U.S.", "u.s.", .) %>%
  gsub("OPENING STATEMENT OF HON.", "opening statement of hon", .) %>%
  gsub("W. Bush", "w.bush", .) %>%
  gsub("ACLU.", "aclu", .) %>%
  gsub("IV.", "iv", .) %>%
  gsub("NRA.", "nra", .) %>%
  gsub("III.", "iii", .) %>%
  gsub("USDA.", "usda", .) %>%
  gsub("TV.", "tv", .) %>%
  gsub("W.", "w", .) %>%
  gsub("NRLB.", "nrlb", .) %>%
  gsub("NLRA.", "nrla", .) %>%
  gsub("\n", " ", .) %>%
  gsub("II,", "ii", .) 
male_white <- data.frame(male_white)
View(male_white)
#    ### Trying QDAP ###
#library(qdap)
#dat <- read.transcript("data/male_white/c115_kavanaugh.docx")
#    ### Trying textreadr ###
#library(textreadr)
#dat <- read_docx("data/male_white/c115_kavanaugh.docx")
#### Preprocessing

# male_white <- "Senator GRASSLEY. Blah blah blah. Senator CRUZ. blah blah blah."
# use gregexpr to find word positions
positions <- gregexpr('\\b[A-Z]+[A-ZA-Z]*(\\b\\s*\\b[A-ZA-Z])*\\.', male_white)[[1]]
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
names <- substring(male_white, positions, positions + match_lengths - 1) # -1 because the "start + length = end + 1"
# Extract text in between matches.

text_starts <- positions + match_lengths
text_ends <- c(positions[-1] - 1, nchar(male_white))
text <- substring(male_white, 
                  text_starts, 
                  text_ends)

male_white <- data.frame(names = names, text = text) %>%
    mutate(male = 1) %>%
    mutate(poc = 0)
