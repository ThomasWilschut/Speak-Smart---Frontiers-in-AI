## MAGDEBURG DATA ANALYSES ##


# load packages:

library('lme4') # for the analysis
library('haven') # to load the SPSS .sav file
library('tidyverse') # needed for data manipulation.
library('lmerTest') # to get p-value estimations that are not part of the standard lme4 packages
library('readxl') # to load in the data from exel
library('ggplot2') # to make plots
library('nlme') # for the linear mixed effects models 
library('effects') # to get statistic values
library('ggpubr') # for citations
library('rstatix')  # to get statistic values
library('plyr') # data manipulation 
library('dplyr')
library('purrr')
library('data.table')
library('ggplot2')
library('reshape2') # for melt()
install.packages('psych')
install.packages('BayesFactor')
library('BayesFactor')
library('psych') # for corr.test()

# set working directory
setwd("~/Desktop/german-cities")

# load ss data:
myfiles = list.files(path="~/Desktop/german-cities", pattern="*.csv", full.names=TRUE)
dat = ldply(myfiles, read_csv)

#memory data:
mem = read_excel('memory_test_results.xlsx')

# import slimstampen functions (make sure they are in the same wd):
source("cor_heatmap_BF.R")

# correlation heatmap functions:
source("slimstampen_model_funs.R")

# remove instruction trials 
dat <- dat[(dat['trial_type'] == 'html-voicekey-response'),]

# drop irrelevant columns 
dat$value <- NULL
dat$trial_type <- NULL
dat$internal_node_id <- NULL
dat$subject <- NULL
dat$browser_info <- NULL
dat$view_history <- NULL
dat$keypresses <- NULL
dat$responses <- NULL
dat$text <- NULL
dat$threshold <- -0.8

# rename columns that need to be renamed 
colnames(dat)[which(names(dat) == "id")] <- "fact_id"
colnames(dat)[which(names(dat) == "presentation_start_time")] <- "start_time"
colnames(dat)[which(names(dat) == "participant_number")] <- "userId"
colnames(dat)[which(names(dat) == "answer")] <- "text"

# create a unique id for each participant and each fact:
participants <- unique(dat$userId)
facts <- unique(dat$fact_id)

# create a dataframe to store the alpha values
rofs <- as.data.frame(matrix(NA, 0, 3))
colnames(rofs)[which(names(rofs) == "V1")] <- "ppn"
colnames(rofs)[which(names(rofs) == "V2")] <- "fact_id"
colnames(rofs)[which(names(rofs) == "V3")] <- "rof"

# compute alpha for each participant:
i = 0
for (participant in participants) {
  for (current_fact_id in facts) {
    i = i+1
    #print (current_fact_id)
    # calculate the activation and alpha for the fact 
    alpha_for_participant_and_fact <-calculate_alpha(Inf, current_fact_id, 0.3, dat[dat$userId == participant,])
    rofs[i,1] <- participant
    rofs[i,2] <- current_fact_id
    rofs[i,3] <- alpha_for_participant_and_fact
    }
  }
}

# alpha's:
summary <- aggregate(rofs[,3], list(rofs$ppn), mean)

mem$RoF <- summary$x

correlations <- select(mem, CeradDistance, Age, MOCA, MOCAMem, RoF)


cor_heatmap_BF(correlations)


cor(mem$RoF, mem$CeradDistance, method = c("pearson")) 
