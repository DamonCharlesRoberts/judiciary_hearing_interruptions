#### Judiciary Hearing Interruptions: Analysis ####

#### Notes: ####   
### Description: Analysis of interruptions in federal judiciary confirmations. Run pre_processing_script.R and counts.R first ###

#### Updates: ####
### By: dcr ###
### 2021/03/23 ###

#### Setup ####
{
  library(ggplot2)
  library(dplyr)
  library(MASS)
  library(ggthemes)
  library(readr)
  library(stargazer)
  library(forcats)
}

counts <- read_csv("data/counts.csv")

counts$poc <- factor(counts$poc, labels = c("White", "Non-white"))
counts$male <- factor(counts$male, labels = c("Female", "Male"))

#### Descriptive Statistics ####
  ### Overdispersion present
median <- median(counts$count)
mean <- mean(counts$count)

median
mean
    ## Graphical Representation of the Overdispersion ##
plot <- ggplot() +
  geom_density(aes(x = count), data = counts, size = 0.8) +
  geom_vline(aes(xintercept = median), linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean), linetype = "dotted", size = 0.9) +
  labs(title = "Distribution of counts of Federal Judicial nominees interrupted", y = "Density", x = "Count of Interruptions", caption = "Source: U.S. Senate Judiciary Committee Transcripts; 2001-2018.\n Note: Dashed Line represents the median of counts which is equal to 0.\n Dotted line represents mean of counts which is equal to 2.5.") +
  theme_bw() +
  ggsave("tables_figures/interruptions_distribution.png", height = 10, width = 10, units = "in")

#### Modelling ####
nb <- glm.nb(count ~ poc + male + hearingYear, data = counts)
summary(nb)
nb2 <- glm.nb(count ~ poc*male + poc + male + hearingYear, data = counts)
summary(nb2)


table <- stargazer(nb2, type = 'text')
