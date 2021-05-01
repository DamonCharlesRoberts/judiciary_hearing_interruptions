#### Judiciary Hearing Interruptions: Analysis ####

#### Notes: ####   
### Description: Analysis of interruptions in federal judiciary confirmations. Run pre_processing_script.R and counts.R first ###

#### Updates: ####
### By: dcr ###
### 2021/04/29 ###

#### Setup ####
{
  library(ggplot2)
  library(dplyr)
  library(MASS)
  library(ggthemes)
  library(readr)
  library(stargazer)
  library(forcats)
  library(fixest)
  library(ggridges)
  library(GLMMadaptive)
  library(afex)
  library(plyr)
  library(stm)
  library(quanteda)
  library(furrr)
  library(stringr)
  library(tidyr)
  #library(tidystm)
}

#### Load in Data ####
counts <- read_csv("data/counts.csv")




#### Descriptive Statistics ####
countsdesc <- counts %>%
  select(count, republican, president, hearingYear, age, male, poc, abaQualified, favorably, govdivided)
cor(countsdesc, use="complete.obs")

  # NOTE: Overdispersion present
median <- median(counts$count)
mean <- mean(counts$count)

median

mean

    ## Graphical Representation of the Overdispersion ##
countF <- counts %>%
  filter(count <= 100) # Filter out Gorsuch and Kavanaugh Hearings 
 
#countF$poc <- factor(countF$poc, levels = c(0, 1), labels = c("White", "Non-white"))
#countF$male <- factor(countF$male, levels = c(0, 1), labels = c("Female", "Male"))
#countF$republican <- factor(countF$republican,
#                            levels = c(0, 1),
#                            labels = c("Democrat", "Republican"))
#countF$abaQualified <- factor(countF$abaQualified,
#                              levels= c(0, 1, 2, 3),
#                              labels = c("Not", "Qualified", "Well", "Exceptionally"))
#countF$favorably <- factor(countF$favorably,
#                           levels = c(0, 1),
#                           labels = c("No", "Yes"))
#countF$court <- factor(countF$court,
#                       levels = c(0,1,2),
#                       labels = c("Other", "District", "Appeals"))
#countF$president <- factor(countF$president, 
#                           levels = c(1,2,3),
#                           labels = c("Bush", "Obama", "Trump"))

    # Plot Distributions - Figure 1 #
plot <- ggplot() +
  geom_density(aes(x = count), data = countF, size = 0.8) +
  geom_vline(aes(xintercept = median), linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean), linetype = "dotted", size = 0.9) +
  labs(title = "Distribution of counts of Federal Judicial nominees interrupted", y = "Density", x = "Count of Interruptions", caption = "Source: U.S. Senate Judiciary Committee Transcripts; 2001-2018.\n Note: For ease of readibility two extreme cases where the count is higher than 100 is excluded from the figure.\n Dashed Line represents the median of counts which is equal to 0.\n Dotted line represents mean of counts which is equal to 3.79.") +
  theme_bw() +
  ggsave("tables_figures/interruptions_distribution.png", height = 10, width = 10, units = "in")

  # Plot Distributions - Figure 2 #
plot2 <- ggplot() + 
  geom_density_ridges(data = countF, aes(x = count, y= male, fill = male), alpha = 0.6) +
  theme_bw() + 
  theme(strip.background = element_rect(fill = "#d4d9de")) +
  scale_fill_manual(values = c("#0072B2","#CC79A7")) +
  facet_wrap(~ poc) +
  labs(title = "Density of Interruptions by Race and Gender", y = "", x = "Count of Interruptions", fill = "Male", caption = "Source: U.S. Senate Judiciary Committee Transcripts; 2001-2018.\n Note: Density of interruption counts by Gender and by Race.") +
  ggsave("tables_figures/interruptions_ridgeplot.png", height = 10, width = 10, units = "in")


#### Modelling ####
#counts$poc <- droplevels(counts$poc)
#counts$male <- droplevels(counts$male)
#counts$republican <- droplevels(counts$republican)


  ### T-tests ###
pocdf <- counts %>%
  filter(poc == 1) 
white <- counts %>%
  filter(poc == 0)
race <- t.test(white$count, pocdf$count)
race2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
race2$t <- race[["statistic"]][["t"]]
race2$conf.low <- race$conf.int[1]
race2$conf.high <- race$conf.int[2]
race2$variable <- "Race - Full"


maledf <- counts %>%
  filter(male == 1)
female <- counts %>%
  filter(male == 0)
male <- t.test(female$count, maledf$count)
male2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
male2$t <- male[["statistic"]][["t"]]
male2$conf.low <- male$conf.int[1]
male2$conf.high <- male$conf.int[2]
male2$variable <- "Male - Full"


divided <- counts %>%
  filter(govdivided == 1)
ndivided <- counts %>%
  filter(govdivided == 0)
division <- t.test(ndivided$count, divided$count)
division2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
division2$t <- division[["statistic"]][["t"]]
division2$conf.low <- division$conf.int[1]
division2$conf.high <- division$conf.int[2]
division2$variable <- "Division - Full"



pocdf2 <- countF %>%
  filter(poc == 1)
white2 <- countF %>%
  filter(poc == 0)
race2.1 <- t.test(white2$count, pocdf2$count)
race2.2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
race2.2$t <- race2.1[["statistic"]][["t"]]
race2.2$conf.low <- race2.1$conf.int[1]
race2.2$conf.high <- race2.1$conf.int[2]
race2.2$variable <- "Race"


maledf2 <- countF %>%
  filter(male == 1)
female2 <- countF %>%
  filter(male == 0)
male2.1 <- t.test(female2$count, maledf2$count)
male2.2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
male2.2$t <- male2.1[["statistic"]][["t"]]
male2.2$conf.low <- male2.1$conf.int[1]
male2.2$conf.high <- male2.1$conf.int[2]
male2.2$variable <- "Male"


divided2 <- countF %>%
  filter(govdivided == 1)
ndivided2 <- countF %>%
  filter(govdivided == 0)
division2.1 <- t.test(ndivided2$count, divided2$count)
division2.2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
division2.2$t <- division2.1[["statistic"]][["t"]]
division2.2$conf.low <- division2.1$conf.int[1]
division2.2$conf.high <- division2.1$conf.int[2]
division2.2$variable <- "Division"

  ## Bind all test statistics into two dataframes for plotting ##
ttest <- rbind(race2, male2,division2)
ttest2 <- rbind(race2.2, male2.2, division2.2)

    ## Figure 4 - T-test point estimates ##
ttestplot <- ggplot() +
  geom_point(aes(x=ttest$t, y = ttest$variable), color = "#77AB43", fill = "#77AB43", size = 3, shape = 22) +
  geom_point(aes(x=ttest2$t, y = ttest2$variable), size = 3) +
  geom_linerange(aes(y = ttest$variable, xmin = ttest$conf.low, xmax = ttest$conf.high), color = "#77AB43", size = .9) +
  geom_linerange(aes(y = ttest2$variable, xmin = ttest2$conf.low, xmax = ttest2$conf.high), size = .9) +
  ggthemes::theme_fivethirtyeight() +
  annotate(
    geom = "text",
    label= c("t = 1.40 [-1.62, 9.61]","t = 0.18 [-0.23, 0.28]", "t = -1.40 [-9.11, 1.47]","t = -0.39 [-0.53, 0.36]", "t = 1.32 [-2.28, 11.66]", "t = -1.45 [-0.56, 0.08]"), 
    x=c(-8, -8, 8, -8, -8, -8),
    y= c("Race - Full", "Race", "Male - Full", "Male", "Division - Full", "Division"),
    color = "black",
    size = 4
  ) +
  labs(title = "Point Estimates of Two-Sided T-Test on Counts", ylab = "Variable", xlab = "T", caption = "Data Source: 2001-2020 Senate Judiciary Hearing Transcripts.\n Note: T-Test comparing mean count of interruptions within variable categories at 95% Confidence Levels.") +
  theme(text = element_text(size = 16)) +
  ggsave("tables_figures/t-test.png", height = 10.1, width = 10.1, units = "in")


### Unpaired two-samples Wilcoxon test ###

race_nonpara <- wilcox.test(white$count, pocdf$count)
race_nonpara2 <- tibble(pvalue = NA, variable = NA)
race_nonpara2$pvalue <- race_nonpara[["p.value"]]
race_nonpara2$variable <- "Race - Full"


male_nonpara <- wilcox.test(female$count, maledf$count)
male_nonpara2 <- tibble(pvalue = NA, variable = NA)
male_nonpara2$pvalue <- male_nonpara[["p.value"]]
male_nonpara2$variable <- "Male - Full"



division_nonpara <- wilcox.test(ndivided$count, divided$count)
division_nonpara2 <- tibble(pvalue = NA, variable = NA)
division_nonpara2$pvalue <- division_nonpara[["p.value"]]
division_nonpara2$variable <- "Division - Full"



race_nonpara2.1 <- wilcox.test(white2$count, pocdf2$count)
race_nonpara2.2 <- tibble(pvalue = NA, variable = NA)
race_nonpara2.2$pvalue <- race_nonpara2.1[["p.value"]]
race_nonpara2.2$variable <- "Race"


male_nonpara2.1 <- wilcox.test(female2$count, maledf2$count)
male_nonpara2.2 <- tibble(pvalue = NA, variable = NA)
male_nonpara2.2$pvalue <- male_nonpara2.1[["p.value"]]
male_nonpara2.2$variable <- "Male"


division_nonpara2.1 <- wilcox.test(ndivided2$count, divided2$count)
division_nonpara2.2 <- tibble(pvalue = NA, variable = NA)
division_nonpara2.2$pvalue <- division_nonpara2.1[["p.value"]]
division_nonpara2.2$variable <- "Division"
  
  ## Make dataframes of Wilcoxon Test P-values ##
ttest_nonpara <- rbind(race_nonpara2, male_nonpara2, division_nonpara2)
ttest_nonpara2 <- rbind(race_nonpara2.2, male_nonpara2.2, division_nonpara2.2)

  ## Figure 3 - P-values of Wilcoxon Tests ##
ttest_nonparaplot <- ggplot() +
  geom_point(aes(x=ttest_nonpara$pvalue, y = ttest_nonpara$variable), color = "#77AB43", fill = "#77AB43", size = 3, shape = 22) +
  geom_point(aes(x=ttest_nonpara2$pvalue, y = ttest_nonpara2$variable), size = 3) +
  geom_vline(xintercept=0.05, linetype = "dashed") +
  geom_vline(xintercept=0.95, linetype = "dashed") +
  ggthemes::theme_fivethirtyeight() +
  labs(title = "P-values calculated from unpaired Wilcoxon Test", ylab = "Variable", xlab = "T", caption = "Data Source: 2001-2020 Senate Judiciary Hearing Transcripts.\n Note: Non-parametric Wilcoxon Test comparing mean count of interruptions within variable categories at\n 95% Confidence Levels.") +
  theme(text = element_text(size = 16)) +
  ggsave("tables_figures/wilcox-test.png", height = 10.1, width = 10.1, units = "in")

### Regression ###
  ## Negative Binomial Regression - Full sample ##

nb <- glm.nb(count ~ poc + male + hearingYear + abaQualified + govdivided, data = counts)
summary(nb)

#nb2 <- glm.nb(count ~  hearingYear + abaQualified + govdivided + poc:male, data = counts)
#coeftest(nb2)
#summary(nb2)

#nb3 <- glm.nb(count ~ hearingYear + abaQualified + govdivided + poc*male, data = counts)

#table <- stargazer(nb, nb2, type = 'text')
  
  ## Negative Binomial Regression - Excluding Gorsuch and Kavanaugh ##
nb4 <- glm.nb(count ~ poc + male + hearingYear + abaQualified + govdivided, data = countF)

#nb5 <- glm.nb(count ~  hearingYear + abaQualified + govdivided + poc:male, data = countF)
#nb6 <- glm.nb(count ~ hearingYear + abaQualified + govdivided + poc*male, data = countF)

### Fixed effects poisson
#
#fepo <- lmer(formula = count ~ poc + male + age + abaQualified + #govdivided + (1 |hearingYear), data = counts, family = "poisson")

#summary(fepo)
#
#fepo2 <- fepois(count ~ poc*male + age + abaQualified| hearingYear, data = countF) #shocker... multicolinnear poc:male coef
#
#summary(fepo2)

  ### TOPIC MODELS ###
source("code/pre_processing_fxns.R")

    ### Setup ###

    ## White Males ##
filePath <- list.files(path = "data/male_white", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

male_white <- ldply(filePath, read_csv) #combine all csv's in file path defined above into one dataframe

# Code the male column as 1 for all in dataset and 0 for the poc column
male_white <- male_white %>% 
  mutate(male = 1) %>%
  mutate(poc = 0)


male_white$names <- str_remove(male_white$names, "\\.") # Remove periods from names column for filtering

male_white <- male_white %>%
  separate(names, c("title", "names"), sep = " ") #Create title and name column


    ## White Females ##

filePath2 <- list.files(path = "data/female_white", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

female_white <- ldply(filePath2, read_csv) #combine all csv's in file path defined above into one dataframe

# Code the male column as 1 for all in dataset and 0 for the poc column
female_white <- female_white %>% 
  mutate(male = 0) %>%
  mutate(poc = 0)


female_white$names <- str_remove(female_white$names, "\\.") # Remove periods from names column for filtering

female_white <- female_white %>%
  separate(names, c("title", "names"), sep = " ") #Create title and name column


    ## POC Males ##

filePath3 <- list.files(path = "data/male_poc", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

male_poc <- ldply(filePath3, read_csv) #combine all csv's in file path defined above into one dataframe

# Code the male column as 1 for all in dataset and 0 for the poc column
male_poc <- male_poc %>% 
  mutate(male = 1) %>%
  mutate(poc = 1)


male_poc$names <- str_remove(male_poc$names, "\\.") # Remove periods from names column for filtering

male_poc <- male_poc %>%
  separate(names, c("title", "names"), sep = " ") #Create title and name column


    ## POC Females ##
filePath4 <- list.files(path = "data/female_poc", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

female_poc <- ldply(filePath4, read_csv) #combine all csv's in file path defined above into one dataframe

# Code the male column as 1 for all in dataset and 0 for the poc column
female_poc <- female_poc %>% 
  mutate(male = 0) %>%
  mutate(poc = 1)


female_poc$names <- str_remove(female_poc$names, "\\.") # Remove periods from names column for filtering

female_poc <- female_poc %>%
  separate(names, c("title", "names"), sep = " ") #Create title and name column

topicDF <- rbind(male_white, male_poc, female_white, female_poc)
topicDF$text <- gsub("[\\r\\n][\\r\\n]", "",topicDF$text)

  ### Modelling ###

set.seed(060906)

topicDFCLean <- textProcessor(documents = topicDF$text, lowercase = TRUE,
                              removestopwords = TRUE, removenumbers = TRUE,
                              removepunctuation = TRUE, stem = TRUE, metadata = topicDF)

topicDFClean <- prepDocuments(topicDFCLean$documents, topicDFCLean$vocab, topicDFCLean$meta, lower.thresh = 1)

#save.image()


stm_model_poc = stm(documents = topicDFClean$documents, vocab = topicDFClean$vocab, prevalence = ~ poc, data = topicDFClean$meta, K = 20, init.type = "LDA")

#saveRDS(stm_model_poc, "data/stm_model_poc.RDS")

# Figure 7
topics_poc <- labelTopics(stm_model_poc, n=7)
topics_poc$frex
jpeg('tables_figures/stm_poc_frex.jpg')
plot(stm_model_poc, type = "summary", xlim = c(0,1), n=4, labeltype = "frex")
dev.off()

stm_model_male = stm(documents = topicDFClean$documents, vocab = topicDFClean$vocab, prevalence = ~male, data = topicDFClean$meta, K=20, init.type = "LDA")

# saveRDS(stm_model_male, "data/stm_model_male.RDS")

# Figure 8
topics_male <- labelTopics(stm_model_male, n = 7)
topics_male$frex
jpeg('tables_figures/stm_male_frex.jpg')
plot(stm_model_male, type = "summary", xlim = c(0,1), n=4, labeltype = "frex")
dev.off()

predict_topic_poc = estimateEffect(formula = 1:20 ~ poc, stmobj = stm_model_poc, metadata = topicDFClean$meta, uncertainty = "Global")

predict_topic_male = estimateEffect(formula = 1:20 ~ male, stmobj = stm_model_male, metadata = topicDFClean$meta, uncertainty = "Global")

# Figure 5
jpeg('tables_figures/stm_poc_predict.jpg')
plot(predict_topic_poc, covariate = "poc", topics = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
     model = stm_model_poc, method = "difference",
     cov.value1 = "poc", cov.value2 = "true",
     main = "Topics of POC and non-poc nominee hearings", verbose.labels = FALSE, sub = "Data Source: Senate Judiciary Committee Federal Judiciary Hearing Transcripts.\n Note: Coefficient plot of nominee race predicting topic.")
dev.off()
summary(predict_topic_poc)
save.image()

# Figure 6
jpeg('tables_figures/stm_male_predict.jpg')
plot(predict_topic_male, covariate = "male", topics = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
     model = stm_model_poc, method = "difference",
     cov.value1 = "male", cov.value2 = "true",
     main = "Topics of Female and Male nominee hearings", verbose.labels = FALSE, sub = "Data Source: Senate Judiciary Committee Federal Judiciary Hearing Transcripts.\n Note: Coefficient plot of nominee gender predicting topic")
dev.off()
summary(predict_topic_poc)
save.image()

#### Tables ####


#fig1 <- stargazer::stargazer(nb, nb2, nb3, style = "apsr",
#                             notes = c("Source: 2001 - 2018 U.S. Senate Judiciary Hearing Transcripts", "Coefficients from Negative Binimial Regressions", "Standard errors in parentheses.", "* p < 0.05"),
#                             title = "Female and Nominees of Color interrupted more in confirmation hearings", 
#                             dep.var.labels = "Count of Interruptions",
#                             covariate.labels = c("Person of Color", "Male", "Hearing Year", "ABA Qualified", "Division", "POC X Male"),
#                             notes.append = FALSE, 
#                             star.cutoffs = c(0.05),
#                             column.labels = c("Additive", "Interaction - Same Intercepts", "Interaction - Different Intercepts"),
#                             model.numbers = FALSE,
#                             type = "latex", 
#                             out = "tables_figures/negative_binomial.tex")
#fig2 <- stargazer::stargazer(nb4, nb5, nb6, style = "apsr",
#                             notes = c("Source: 2001 - 2018 U.S. Senate Judiciary Hearing Transcripts - Excluding Gorsuch and Kavanaugh", "Coefficients from Negative Binimial Regressions", "Standard errors in parentheses.", "* p < 0.05"),
#                             title = "Female and Nominees of Color interrupted more in confirmation hearings - Cases Excluded", 
 #                            dep.var.labels = "Count of Interruptions",
#                             covariate.labels = c("Person of Color", "Male", "Hearing Year", "ABA Qualified", "Division", "POC X Male"),
#                             notes.append = FALSE, 
#                             star.cutoffs = c(0.05),
#                             column.labels = c("Additive", "Interaction - Same Intercepts", "Interaction - Different Intercepts"),
#                             model.numbers = FALSE,
#                             type = "latex", 
#                             out = "tables_figures/negative_binomial2.tex")

#fig3 <- etable(fepo, fepo2, tex = TRUE)
#fig3 <- stargazer::stargazer(fepo, style = "apsr",
#                             notes = c("Source: 2001 - 2018 U.S. Senate Judiciary Hearing Transcripts", "Coefficients from Poisson Count Model with Hearing Year Fixed Effects", "Standard errors in parentheses.", "* p < 0.05"),
#                                       title = "Female and Nominees of Color interrupted more in confirmation hearings",
#                                       covariate.labels = c("Person of Color", "Male", "Age", "ABA Qualified", "POC X Male"),
#                                       notes.append = FALSE,
#                                       star.cutoffs = c(0.05),
#                                       column.labels = c("Additive", "Interaction"),
#                                       model.numbers = FALSE,
#                                       type = "latex",
#                                       out = "tables_figures/poisson_fe.tex")

  ## Table 1 ## 
fig1 <- stargazer::stargazer(nb, nb4,
                             style = "apsr",
                             notes = c("Source: 2001 - 2020 U.S. Senate Judiciary Hearing Transcripts.", "Coefficients from Negative Binomial Regression Models.", "Standard errors in parenthases", "* p $<0.05$"),
                             title = "Female and Nominees of Color interrupted more in confirmation hearings",
                             dep.var.labels = c("Count of Interruptions"),
                             covariate.labels = c("Person of Color", "Male", "Hearing Year", "ABA Qualified", "Division"),
                             notes.append = FALSE,
                             star.cutoffs = c(0.05),
                             column.labels = c("Full Cases", "Excluded Extreme Cases"),
                             model.numbers = FALSE,
                             type = "latex",
                             out = "tables_figures/simple.tex"
                             )
