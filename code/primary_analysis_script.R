# Title: Judiciary Hearing Interruptions: Analysis ----

# Notes: ----
  #* Description: Analysis of interruptions in federal judiciary confirmations. Run pre_processing_script.R and counts.R first ----

# Updates: ----
  #* By: dcr ----
  #* Date: 2022-03-12 ----

# Setup ----
  #* Modularly load packages
#install.packages('box')
box::use(
  ggplot2 = ggplot2[geom_linerange, geom_vline, geom_density, geom_point, ggsave, labs, theme_bw],
  MASS = MASS[glm.nb],
  dplyr = dplyr[select, filter, mutate],
  tibble = tibble[tibble],
  plyr = plyr[ldply],
  stringr = stringr[str_remove],
  tidyr = tidyr[separate],
  quanteda = quanteda[corpus, tokens, tokens_remove, tokens_select, stopwords, dfm, dfm_wordstem, dfm_subset, ntoken, docvars],
  stm = stm[searchK, stm, asSTMCorpus, labelTopics, estimateEffect, findThoughts, plotQuote],
  modelsummary = modelsummary[modelsummary]
)

# STUDY 1 ----
  #* Load in Data ----
counts <- read_csv("data/counts.csv")




  #* Descriptive Statistics ----
countsdesc <- counts |>
  select(count, republican, president, hearingYear, age, male, poc, abaQualified, favorably, govdivided)
cor(countsdesc, use="complete.obs")

  #** NOTE: Overdispersion present ----
median <- median(counts$count)
mean <- mean(counts$count)


      #*** Graphical Representation of the Overdispersion -----
countF <- counts |>
  filter(count <= 100) # Filter out Gorsuch and Kavanaugh Hearings 
 
countF$poc <- factor(countF$poc, levels = c(0, 1), labels = c("White", "Non-white"))
countF$male <- factor(countF$male, levels = c(0, 1), labels = c("Female", "Male"))
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

        #*** Figure 1 ----
plot <- ggplot() +
  geom_density(aes(x = count), data = countF, size = 0.8) +
  geom_vline(aes(xintercept = median), linetype = "dashed", size = 0.8) +
  geom_vline(aes(xintercept = mean), linetype = "dotted", size = 0.9) +
  labs(title = "Distribution of counts of Federal Judicial nominees interrupted", y = "Density", x = "Count of Interruptions", caption = "Source: U.S. Senate Judiciary Committee Transcripts; 2001-2018.\n Note: For ease of readibility two extreme cases where the count is higher than 100 is excluded from the figure.\n Dashed Line represents the median of counts which is equal to 0.\n Dotted line represents mean of counts which is equal to 3.79.") +
  theme_bw()
ggsave("tables_figures/interruptions_distribution.png", height = 10, width = 10, units = "in")

        #*** Figure 2 ----
plot2 <- ggplot(data = countF, aes(x = count, y= male, fill = male)) + 
  geom_density_ridges(alpha = 0.6) +
  theme_bw() + 
  theme(strip.background = element_rect(fill = "#d4d9de")) +
  scale_fill_manual(values = c("#0072B2","#CC79A7")) +
  facet_wrap(~ poc) +
  labs(title = "Density of Interruptions by Race and Gender", y = "", x = "Count of Interruptions", fill = "Male", caption = "Source: U.S. Senate Judiciary Committee Transcripts; 2001-2018.\n Note: Density of interruption counts by Gender and by Race.")
ggsave("tables_figures/interruptions_ridgeplot.png", plot = plot2, height = 10, width = 10, units = "in")


  #* Modelling ----
#counts$poc <- droplevels(counts$poc)
#counts$male <- droplevels(counts$male)
#counts$republican <- droplevels(counts$republican)


    #** T-tests ----
      #*** POC ----
pocdf <- counts |>
  filter(poc == 1) 
white <- counts |>
  filter(poc == 0)
race <- t.test(white$count, pocdf$count)
race2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
race2$t <- race[["statistic"]][["t"]]
race2$conf.low <- race$conf.int[1]
race2$conf.high <- race$conf.int[2]
race2$variable <- "Race - Full"

      #*** Male ----
maledf <- counts |>
  filter(male == 1)
female <- counts |>
  filter(male == 0)
male <- t.test(female$count, maledf$count)
male2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
male2$t <- male[["statistic"]][["t"]]
male2$conf.low <- male$conf.int[1]
male2$conf.high <- male$conf.int[2]
male2$variable <- "Male - Full"

      #*** Divided Government -----
divided <- counts |>
  filter(govdivided == 1)
ndivided <- counts |>
  filter(govdivided == 0)
division <- t.test(ndivided$count, divided$count)
division2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
division2$t <- division[["statistic"]][["t"]]
division2$conf.low <- division$conf.int[1]
division2$conf.high <- division$conf.int[2]
division2$variable <- "Division - Full"


        #*** Without Gorsuch and Kavanaugh ----
          #**** POC ----
pocdf2 <- countF |>
  filter(poc == 1)
white2 <- countF |>
  filter(poc == 0)
race2.1 <- t.test(white2$count, pocdf2$count)
race2.2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
race2.2$t <- race2.1[["statistic"]][["t"]]
race2.2$conf.low <- race2.1$conf.int[1]
race2.2$conf.high <- race2.1$conf.int[2]
race2.2$variable <- "Race"

          #**** Male ----
maledf2 <- countF |>
  filter(male == 1)
female2 <- countF |>
  filter(male == 0)
male2.1 <- t.test(female2$count, maledf2$count)
male2.2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
male2.2$t <- male2.1[["statistic"]][["t"]]
male2.2$conf.low <- male2.1$conf.int[1]
male2.2$conf.high <- male2.1$conf.int[2]
male2.2$variable <- "Male"

          #**** Divided government -----
divided2 <- countF |>
  filter(govdivided == 1)
ndivided2 <- countF |>
  filter(govdivided == 0)
division2.1 <- t.test(ndivided2$count, divided2$count)
division2.2 <- tibble(t = NA, conf.low = NA, conf.high = NA)
division2.2$t <- division2.1[["statistic"]][["t"]]
division2.2$conf.low <- division2.1$conf.int[1]
division2.2$conf.high <- division2.1$conf.int[2]
division2.2$variable <- "Division"

    #** Bind all test statistics into two dataframes for plotting ----
ttest <- rbind(race2, male2,division2)
ttest2 <- rbind(race2.2, male2.2, division2.2)

    #** Figure 4 - T-test point estimates ----
ttestplot <- ggplot() +
  geom_point(aes(x=ttest$t, y = ttest$variable), color = "#77AB43", fill = "#77AB43", size = 3, shape = 22) +
  geom_point(aes(x=ttest2$t, y = ttest2$variable), size = 3) +
  geom_linerange(aes(y = ttest$variable, xmin = ttest$conf.low, xmax = ttest$conf.high), color = "#77AB43", size = .9) +
  geom_linerange(aes(y = ttest2$variable, xmin = ttest2$conf.low, xmax = ttest2$conf.high), size = .9) +
  theme_bw() +
  annotate(
    geom = "text",
    label= c("t = 1.40 [-1.62, 9.61]","t = 0.18 [-0.23, 0.28]", "t = -1.40 [-9.11, 1.47]","t = -0.39 [-0.53, 0.36]", "t = 1.32 [-2.28, 11.66]", "t = -1.45 [-0.56, 0.08]"), 
    x=c(-8, -8, 8, -8, -8, -8),
    y= c("Race - Full", "Race", "Male - Full", "Male", "Division - Full", "Division"),
    color = "black",
    size = 4
  ) +
  labs(title = "Point Estimates of Two-Sided T-Test on Counts", y = "Variable", x = "T", caption = "Data Source: 2001-2020 Senate Judiciary Hearing Transcripts.\n Note: T-Test comparing mean count of interruptions within variable categories at 95% Confidence Levels.") +
  theme(text = element_text(size = 16))
ggsave("tables_figures/t-test.png", plot = ttestplot, height = 10.1, width = 10.1, units = "in")


  #* Unpaired two-samples Wilcoxon test -----
    #** Race ----
race_nonpara <- wilcox.test(white$count, pocdf$count)
race_nonpara2 <- tibble(pvalue = NA, variable = NA)
race_nonpara2$pvalue <- race_nonpara[["p.value"]]
race_nonpara2$variable <- "Race - Full"

    #** Male ----
male_nonpara <- wilcox.test(female$count, maledf$count)
male_nonpara2 <- tibble(pvalue = NA, variable = NA)
male_nonpara2$pvalue <- male_nonpara[["p.value"]]
male_nonpara2$variable <- "Male - Full"

    #** Divided government ----
division_nonpara <- wilcox.test(ndivided$count, divided$count)
division_nonpara2 <- tibble(pvalue = NA, variable = NA)
division_nonpara2$pvalue <- division_nonpara[["p.value"]]
division_nonpara2$variable <- "Division - Full"

    #** Excluding Gorsuch and Kavanaugh ----
      #*** Race ----
race_nonpara2.1 <- wilcox.test(white2$count, pocdf2$count)
race_nonpara2.2 <- tibble(pvalue = NA, variable = NA)
race_nonpara2.2$pvalue <- race_nonpara2.1[["p.value"]]
race_nonpara2.2$variable <- "Race"
      
      #*** Male ----
male_nonpara2.1 <- wilcox.test(female2$count, maledf2$count)
male_nonpara2.2 <- tibble(pvalue = NA, variable = NA)
male_nonpara2.2$pvalue <- male_nonpara2.1[["p.value"]]
male_nonpara2.2$variable <- "Male"

      #*** Divided government ----
division_nonpara2.1 <- wilcox.test(ndivided2$count, divided2$count)
division_nonpara2.2 <- tibble(pvalue = NA, variable = NA)
division_nonpara2.2$pvalue <- division_nonpara2.1[["p.value"]]
division_nonpara2.2$variable <- "Division"
  
  #* Make dataframes of Wilcoxon Test P-values ----
ttest_nonpara <- rbind(race_nonpara2, male_nonpara2, division_nonpara2)
ttest_nonpara2 <- rbind(race_nonpara2.2, male_nonpara2.2, division_nonpara2.2)

  #* Figure 3 - P-values of Wilcoxon Tests ----
ttest_nonparaplot <- ggplot() +
  geom_point(aes(x=ttest_nonpara$pvalue, y = ttest_nonpara$variable), color = "#77AB43", fill = "#77AB43", size = 3, shape = 22) +
  geom_point(aes(x=ttest_nonpara2$pvalue, y = ttest_nonpara2$variable), size = 3) +
  geom_vline(xintercept=0.05, linetype = "dashed") +
  geom_vline(xintercept=0.95, linetype = "dashed") +
  theme_bw() +
  labs(title = "P-values calculated from unpaired Wilcoxon Test", y = "Variable", x = "", caption = "Data Source: 2001-2020 Senate Judiciary Hearing Transcripts.\n Note: Non-parametric Wilcoxon Test comparing mean count of interruptions within variable categories at\n 95% Confidence Levels.") +
  theme(text = element_text(size = 16))
ggsave("tables_figures/wilcox-test.png", plot = ttest_nonparaplot, height = 10.1, width = 10.1, units = "in")

  #* Regression -----
    #** Negative Binomial Regression - Full sample -----

nb <- glm.nb(count ~ poc + male + hearingYear + abaQualified + govdivided, data = counts)

    #** Negative Binomial Regression - Excluding Gorsuch and Kavanaugh ----
nb4 <- glm.nb(count ~ poc + male + hearingYear + abaQualified + govdivided, data = countF)

    #** Tables ----

models = list('Full Cases' = nb, 'Excluded Extreme Cases' = nb4)
fig1 = modelsummary(models, coef_map = c('poc' = 'Nominee of Color', 'male' = 'Male', 'hearingYear' = 'Hearing Year', 'abaQualified' = 'ABA Qualified', 'govdivided' = 'Divided Government', '(Intercept)' = 'Constant'), stars = c('*' = 0.05), out = 'tables_figures/simple.tex')

# STUDY 2 ----

  #* Set up data ----
source("code/pre_processing_fxns.R")

    #** White Males -----
filePath <- list.files(path = "data/male_white", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

male_white <- ldply(filePath, read_csv) #combine all csv's in file path defined above into one dataframe

male_white <- male_white |> 
  mutate(male = 1) |>
  mutate(poc = 0) # Code the male column as 1 for all in dataset and 0 for the poc column

male_white$names <- str_remove(male_white$names, "\\.") # Remove periods from names column for filtering

male_white <- male_white |>
  separate(names, c("title", "names"), sep = " ") #Create title and name column

    #** White Females ----

filePath2 <- list.files(path = "data/female_white", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

female_white <- ldply(filePath2, read_csv) #combine all csv's in file path defined above into one dataframe

female_white <- female_white |> 
  mutate(male = 0) |>
  mutate(poc = 0)# Code the male column as 1 for all in dataset and 0 for the poc column


female_white$names <- str_remove(female_white$names, "\\.") # Remove periods from names column for filtering

female_white <- female_white |>
  separate(names, c("title", "names"), sep = " ") #Create title and name column

    #** POC Males ----

filePath3 <- list.files(path = "data/male_poc", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

male_poc <- ldply(filePath3, read_csv) #combine all csv's in file path defined above into one dataframe

male_poc <- male_poc |> 
  mutate(male = 1) |>
  mutate(poc = 1 )# Code the male column as 1 for all in dataset and 0 for the poc column

male_poc$names <- str_remove(male_poc$names, "\\.") # Remove periods from names column for filtering

male_poc <- male_poc |>
  separate(names, c("title", "names"), sep = " ") #Create title and name column

    #** POC Females ----
filePath4 <- list.files(path = "data/female_poc", pattern = "*.csv", full.names = TRUE) # define file path of csv's to pull in

female_poc <- ldply(filePath4, read_csv) #combine all csv's in file path defined above into one dataframe

female_poc <- female_poc |> 
  mutate(male = 0) |>
  mutate(poc = 1)# Code the male column as 1 for all in dataset and 0 for the poc column

female_poc$names <- str_remove(female_poc$names, "\\.") # Remove periods from names column for filtering

female_poc <- female_poc |>
  separate(names, c("title", "names"), sep = " ") #Create title and name column

topicDFtext <- rbind(male_white, male_poc, female_white, female_poc) #bind the datasets
pattern = str_c(c('\\n', '-\\n', '\n', '-\n', '\r\n'), collapse = '|')#define patterns for newlines and hypenations
topicDFtext$text <- str_remove_all(topicDFtext$text, pattern)#remove patterns from the documents

    #** Set up as corpus
topicDF = corpus(topicDFtext)
    #** Tokenize the corpus
topicDFClean = tokens(topicDF, remove_punct = TRUE, remove_numbers = TRUE) |> #remove punctuation and numbers
  tokens_remove(stopwords('en'), min_nchar = 4) |> # remove stopwords and words with less than 4 characters
  tokens_select(c('jkt', 'frm', 'fmt','sfmt', 'nverdate', 'psn', 'cmorc', 'gpo', 'sjud1', 'sjud4', 'nto', 'txt', 'nand', 'nthe','nin'), selection = 'remove', padding = FALSE) |> #remove these particular words
  tokens_select("^\\d+|\\d+$", selection = 'remove', valuetype = 'regex') |> #remove any numbers connected to alphanumeric characters - remove_numbers wouldn't get these
  dfm() |> # construct as a quanteda dfm
  dfm_wordstem() # lemmatize the words in the dfm
topicDFClean = dfm_subset(topicDFClean, ntoken(topicDFClean) > 0) #remove documents without any tokens
topicDFCleanSTM = asSTMCorpus(topicDFClean, data = docvars(topicDFClean)) # to make things easier later, convert this to a STM dtm

save.image()

  #* Models
    #** POC ----
      #*** Find number of topics ----
storage_poc = searchK(topicDFCleanSTM$documents, topicDFCleanSTM$vocab, prevalence = ~ poc, K = 10:30, data = docvars(topicDFClean), init.type = 'Spectral', seed = 060906) #iteratively run the model varying the number of topics and store it
saveRDS(storage_poc, 'data/search_k_poc.RDS')#save the results of this - it takes a very long time to run
jpeg('tables_figures/search_k_poc.jpeg')#create a jpeg file
plot(storage_poc)#plot the various metrics of model performance
dev.off()#clear this plot after saving it
      #*** With metrics about model performance across number of topics, run the model ----
stm_model_poc = stm(topicDFClean, prevalence = ~ poc, data = docvars(topicDFClean), K = 30, init.type = 'Spectral', seed = 060906)#run the model
saveRDS(stm_model_poc, "data/stm_model_poc.RDS")#save the model
      #*** Figure 7 ----
topics_poc <- labelTopics(stm_model_poc, n=30)
topics_poc$frex
jpeg('tables_figures/stm_poc_frex.jpg')
plot(stm_model_poc, type = "summary", xlim = c(0,1), n=5, labeltype = "frex")
dev.off()
      #*** Estimate effect of nominee race on topic ----
predict_topic_poc = estimateEffect(formula = 1:30 ~ poc, stmobj = stm_model_poc, metadata = docvars(topicDFClean), uncertainty = "Global")
      #*** Figure 5 ----
jpeg('tables_figures/stm_poc_predict.jpg')
plot(predict_topic_poc, covariate = "poc", topics = 1:30,
     model = stm_model_poc, method = "difference",
     cov.value1 = "poc", cov.value2 = "true",
     main = "Topics of POC and non-poc nominee hearings", verbose.labels = FALSE, sub = "Data Source: Senate Judiciary Committee Federal Judiciary Hearing Transcripts.\n Note: Coefficient plot of nominee race predicting topic.")
dev.off()
summary(predict_topic_poc)
save.image()

pocThoughts = findThoughts(stm_model_poc, texts = topicDFtext$text, n = 2 topics = 1)
  #** Male ----
    #*** Find number of topics
storage_male = searchK(topicDFCleanSTM$documents, topicDFCleanSTM$vocab, prevalence = ~ male, K = 10:30, data = docvars(topicDFClean), init.type = 'Spectral', seed = 060906)#iteratively run the model varyi8ng the number of topics and store it
saveRDS(storage_male, 'data/search_k_male.RDS')#save the results of this - it also takes a very long time to run
jpeg('tables_figures/search_k_male.jpeg')#create a jpeg file
plot(storage_male)#plot the various metrics of model performance
dev.off()#clear this plot after saving it
      #*** With metrics about model performance across number of topics, run the model ----
stm_model_male = stm(topicDFClean, prevalence = ~ male, data = docvars(topicDFClean), K = 30, init.type = 'Spectral', seed = 060906)#run the model
saveRDS(stm_model_male, "data/stm_model_male.RDS")#save the model
      #*** Figure 8 ----
topics_male <- labelTopics(stm_model_male, n = 10)
topics_male$frex
jpeg('tables_figures/stm_male_frex.jpg')
plot(stm_model_male, type = "summary", xlim = c(0,1), n=4, labeltype = "frex")
dev.off()
      #*** Estimate effect of nominee gender on topic ----
predict_topic_male = estimateEffect(formula = 1:30 ~ male, stmobj = stm_model_male, metadata = docvars(topicDFClean), uncertainty = "Global")
      #*** Figure 6 ----
jpeg('tables_figures/stm_male_predict.jpg')
plot(predict_topic_male, covariate = "male", topics = 1:30,
     model = stm_model_poc, method = "difference",
     cov.value1 = "male", cov.value2 = "true",
     main = "Topics of Female and Male nominee hearings", verbose.labels = FALSE, sub = "Data Source: Senate Judiciary Committee Federal Judiciary Hearing Transcripts.\n Note: Coefficient plot of nominee gender predicting topic")
dev.off()
save.image()

#topic1_poc = findThoughts(stm_model_poc, texts = topicDFtext$text, n = 1, topics = 1)
#topic3_poc = findThoughts(stm_model_poc, texts = topicDFtext$text, n = 1, topics = 3)
#jpeg('tables_figures/sample_poc_doc.jpg')
#par(mfrow = c(1,2), mar = c(.5, .5, 1, .5))
#plotQuote(topic1_poc, width = 30, main = 'Topic 1')
#plotQuote(topic3_poc, width = 30, main = 'Topic 3')
#dev.off()