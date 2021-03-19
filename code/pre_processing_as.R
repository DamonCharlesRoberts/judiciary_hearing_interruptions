#Load Packages
library(tesseract)
library(magick)
library(pdftools)
library(tidyverse)

#Set Language to English
eng <- tesseract("eng")

pngfile <- pdftools::pdf_convert('data/male_white/c115_kavanaugh.pdf', dpi = 600)
text <- tesseract::ocr("data/male_white/c115_kavanaugh.png")
cat(text)

df<-as.data.frame(text)
df<-separate_rows(df, text, sep="\n\n")
df<-separate(df, text, into=c("speaker", "speech"), sep="\\.", extra="merge")
df<-separate(df, speaker, into=c("speaker", "continuing"), sep="\\[")
df_clean<-df[!is.na(df$speech)]