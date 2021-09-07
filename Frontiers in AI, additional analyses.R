### Additional Frontiers in AI Analysis script SPEAK SMART ###

# 1. installing and loading packages, importing data

#install.packages('lme4')
#install.packages('haven')
#install.packages('tidyverse')
#install.packages('lmerTest')
#install.packages('readxl')
#install.packages('effects')
#install.packages('ggpubr')
#install.packages('rstatix')
#install.packages('AICcmodavg')
#install.packages("rmarkdown")
#install.packages('plyr', repos = "http://cran.us.r-project.org")
# install.packages('pROC')

# load packages
oldw <- getOption("warn")
options(warn = -1)

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
library('AICcmodavg') # computing AIC's 
library('rmarkdown')
library('pROC')
library('Rcpp')

# function to compute model fit

r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

# import data (NB. data is already cleaned and pre-processed (unlikely responses have been removed)
dat <- read_excel('full.xlsx')

#2. preprocessing

# convert timestamps to minutes

dat$time <- dat$timestamp/60000

# Set time to zero at the start of each block for each participant

for(i in unique(dat$ppn)) {
    dat_select <-dat[(dat['ppn'] == i),]
    individual_block_start_times <- (dat_select[!duplicated(dat_select$block),'time'])
    dat[(dat['ppn'] == i & dat['block'] == 'A'), 'time'] <- (dat[(dat['ppn'] == i & dat['block'] == 'A'), 'time'] - individual_block_start_times[[1,1]])
    dat[(dat['ppn'] == i & dat['block'] == 'B'), 'time'] <- (dat[(dat['ppn'] == i & dat['block'] == 'B'), 'time'] - individual_block_start_times[[2,1]])
    dat[(dat['ppn'] == i & dat['block'] == 'C'), 'time'] <- (dat[(dat['ppn'] == i & dat['block'] == 'C'), 'time'] - individual_block_start_times[[3,1]])
} 


# select study trials for analysis
dat_study <-dat[(dat['types'] == 'study'),]
dat_study <-dat_study[(dat_study['block'] != 'C'),]     # do not analyse block C

# round time stamps   
dat_study$time_bins <-round(dat_study$time)

# 2. Descriptives: differences in RT's between the speaking- and typing conditions 
# (NB. ss typing = A; SS speaking = B; flashcard speaking = C)

# RT's over time: 
rt_over_time <- ggplot(data = dat_study, aes(time, rt, color = block)) + geom_smooth()
rt_over_time

# Alpha over time: 
alpha_over_time <- ggplot(data = dat_study, aes(time, alpha, color = block)) + geom_smooth()
alpha_over_time

# Histogram RT's:
hist_rt <- ggplot(data = dat_study, aes(x=rt, color = block)) +
  geom_histogram(fill='white', alpha = 0.5, position = 'identity') 
hist_rt

# Correlations 

ggplot(data = dat_study) + stat_summary(
  mapping = aes(y = rt, x = factor(ppn), color = factor(block)),
  fun.ymin = min,
  fun.ymax = max,
  fun.y = median
)

# further parse data:
dat_study_A <-dat_study[(dat_study['block'] == 'A'),]     # ss typing
dat_study_B <-dat_study[(dat_study['block'] == 'B'),]     # ss speaking
dat_study_C <-dat_study[(dat_study['block'] == 'C'),]     # flashcard speaking 

rt_type = aggregate(dat_study_A$rt, list(dat_study_A$ppn), FUN=mean, na.rm = TRUE)
rt_speak = aggregate(dat_study_B$rt, list(dat_study_B$ppn), FUN=mean, na.rm = TRUE)

cor(rt_type$x, rt_speak$x, method = c("pearson"))  # pearson's R = 0.1

# Boxplot RT's:

ggplot(dat_study, aes(x= factor(time_bins), y=rt, fill=block)) + 
  geom_boxplot()

#3. Analysis: which  (typing-based RT or speech-based RT) is a better predictor of performance?

# ACT-R mapping from activation to RT can be used to compute a predicted activation using
# the recorded reaction times:

dat_study$rt = dat_study$rt/1000  # rt to seconds

dat_study$expected_activation_from_rt = -log(dat_study$rt-0.3) # compute predicted activation based on RT

dat_study$expected_accuracy_from_rt = 1/(1+exp(((-0.8 - dat_study$expected_activation_from_rt)/0.1)))  # compute predicted accuracy based on prdicted RT


# further parse data:
dat_study_A <-dat_study[(dat_study['block'] == 'A'),]     # ss typing
dat_study_B <-dat_study[(dat_study['block'] == 'B'),]     # ss speaking
dat_study_C <-dat_study[(dat_study['block'] == 'C'),]     # flashcard speaking 

# 3.1 predict actual acccuracy from rt-based expected activation, adding ppn and factid to the model.
# This model is conceptually simmilar to the the analysis in the UMAP paper. 

typing = glmer(correct ~ expected_activation_from_rt  + ((1 | ppn) + (1 | fact_id)), data = dat_study_A, family = 'binomial')
speaking = glmer(correct ~ expected_activation_from_rt + ((1 | ppn) + (1 | fact_id)), data = dat_study_B, family = 'binomial')

# AIC:
summary(typing)        # p < 0.001, AIC: 1817 
summary(speaking)      # p < 0.001, AIC: 1961

# pseudo-R2:
r2.corr.mer(typing)    # R2 = 0.14
r2.corr.mer(speaking)  # R2 = 0.30

# plot (predicting accuracy from RT-based estimated accuracy)
ggplot(dat_study[(dat_study['block'] != 'C'),], aes(y=correct,x=expected_activation_from_rt, color = block)) + geom_point(alpha = 0.1) +
  stat_smooth(method='glm', method.args=list(family='binomial'),se=TRUE)

# 3.2 also add trial number to the analysis:

typing = glmer(correct ~ expected_activation_from_rt + trial + ((1 | ppn) + (1 | fact_id)), data = dat_study_A, family = 'binomial')
speaking = glmer(correct ~ expected_activation_from_rt + trial + ((1 | ppn) + (1 | fact_id)), data = dat_study_B, family = 'binomial')

# create a simulated dataset containting very short (500ms) normal (1500ms) slow (3000ms) and very slow (4500ms) responses:
simulated_data = crossing(rt = c(0.5, 1.5, 3, 4.5), fact_id = 1:10, ppn = 1:30, trial = 1:100)

# calculated expected actiavtions for the simulated dataset
simulated_data$expected_activation_from_rt =  -log(simulated_data$rt-0.3)

# estimate the model predictions for the typing-rt based model for the simulated dataset
simulated_data$typing_prediction <- predict(typing, newdata=simulated_data, type = 'response', allow.new.levels = TRUE)

# estimate the model predictions for the speaking-rt based model for the simulated dataset
simulated_data$speaking_prediction <- predict(speaking, newdata=simulated_data, type = 'response', allow.new.levels = TRUE)

# plot the typing model
ggplot(dat = simulated_data, aes(x = trial, y = typing_prediction, color = factor(rt))) + geom_smooth()

# plot the speaking model
ggplot(dat = simulated_data, aes(x = trial, y = speaking_prediction, color = factor(rt))) + geom_smooth() 



# Compute RoC's

# add actual and predicted values to the dataframe:
dat_study_A$typing_prediction <- predict(typing, newdata=dat_study_A, type = 'response', allow.new.levels = TRUE)
dat_study_B$speaking_prediction <- predict(speaking, newdata=dat_study_B, type = 'response', allow.new.levels = TRUE)

# plot ROC for typing
pROC <- roc(dat_study_A$correct,dat_study_A$typing_prediction,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# plot ROC for speaking
pROC <- roc(dat_study_B$correct,dat_study_B$speaking_prediction,
            smoothed = TRUE,
            # arguments for ci
            ci=TRUE, ci.alpha=0.9, stratified=FALSE,
            # arguments for plot
            plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
            print.auc=TRUE, show.thres=TRUE)


options(warn = oldw)






