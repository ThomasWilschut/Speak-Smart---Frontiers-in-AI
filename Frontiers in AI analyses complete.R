### Frontiers - Additional Analysis script SPEAK SMART ###

oldw <- getOption("warn")
options(warn = -1)
 
## 1. PACKAGES and FUNCTIONS ##

# install.packages('lme4')
# install.packages('haven')
# install.packages('tidyverse')
# install.packages('lmerTest')
# install.packages('readxl')
# install.packages('effects')
# install.packages('ggpubr')
# install.packages('rstatix')
# install.packages('AICcmodavg')
# install.packages("rmarkdown")
# install.packages('plyr', repos = "http://cran.us.r-project.org")
# install.packages('pROC')
# install.packages('patchwork')
# install.packages('psych')
# install.packages('BayesFactor')
# install.packages('reshape2')
# install.packages('ggridges')
# install.packages('BBmisc')

# load packages

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
library('dplyr') # data manipulation 
library('AICcmodavg') # computing AIC's 
library('rmarkdown')
library('pROC')
library('Rcpp')
library('patchwork')
library('BayesFactor')
library('psych') # for corr.test()
library('reshape2')
library('ggridges')
library('BBmisc')

# set theme for plots:
theme_set(theme_minimal())

# color palettes:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette2 <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# function to compute model fit:

r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
}

# correlation heatmap functions:
source("cor_heatmap_BF.R")

# import slimstampen functions (make sure they are stored in the same wd):
source("slimstampen_model_funs.R")

## 2. IMPORTING DATA  ##

# import minor thesis slimstampen data: 
dat <- read_excel('full.xlsx')

#import magdeburg slimstampen data:
myfiles <- list.files(pattern="*.csv", full.names=TRUE)
datm <- ldply(myfiles, read_csv)

# import memory questionnaire data:
mem <- read_excel('memory_test_results.xlsx')

## 3. COMPARISON OF TYPING- AND SPEAKING BASED RT's ##

# 3.1 preprocessing

dat$time <- dat$timestamp/60000 # convert timestamps to minutes

# set time to zero at the start of each block for each participant:

for(i in unique(dat$ppn)) {
    dat_select <-dat[(dat$ppn == i),]
    individual_block_start_times <- (dat_select[!duplicated(dat_select$block),'time'])
    dat[(dat$ppn == i & dat$block == 'A'), 'time'] <- (dat[(dat$ppn == i & dat$block == 'A'), 'time'] - individual_block_start_times[[1,1]])
    dat[(dat$ppn == i & dat$block == 'B'), 'time'] <- (dat[(dat$ppn == i & dat$block == 'B'), 'time'] - individual_block_start_times[[2,1]])
    dat[(dat$ppn == i & dat$block == 'C'), 'time'] <- (dat[(dat$ppn == i & dat$block == 'C'), 'time'] - individual_block_start_times[[3,1]])
}

# compute fact repetitions:
df_total = NULL

for(i in unique(dat$ppn)) {
  for (ii in unique(dat$fact_id)) {
    dat_select_ppn <- dat[(dat$ppn == i),]
    dat_select_ppn_fact <- dat_select_ppn[(dat_select_ppn$fact_id == ii),]
    dat_select_ppn_fact$repetition <- seq_along(dat_select_ppn_fact$time)
    df_total <- rbind(df_total, dat_select_ppn_fact)
  }
}

dat <- df_total
hist(dat$repetition) # how many reps do users usually have?

# select study trials for analysis
dat_study <-dat[(dat['types'] == 'study'),]
dat_study <-dat_study[(dat_study['block'] != 'C'),]     # do not analyse block C

# round time stamps   
dat_study$time_bins <-floor(dat_study$time) + 1
hist(dat_study$time)

# 3.2 Descriptives: differences in RT's between the speaking- and typing conditions 
# (NB. ss typing = A; SS speaking = B; flashcard speaking = C)

# RT's over time: 
rt_over_time <- ggplot(data = dat_study, aes(time, rt, color = block)) + geom_smooth() + 
  ggtitle("Reaction times over time") +  labs(x="Time (min)", y="RT (ms)")
rt_over_time

# Alpha over time: 
alpha_over_time <- ggplot(data = dat_study, aes(time, alpha, color = block)) + geom_smooth() + 
  ggtitle("Rate of Forgetting over time") +  labs(x="Time (min)", y="RoF")
alpha_over_time

# Histogram RT's:
hist_rt <- ggplot(data = dat_study, aes(x=rt, color = block)) +
  geom_histogram(fill='white', alpha = 0.5, position = 'identity') +
  ggtitle("Distribution of reaction times") +  labs(x="RT (ms)", y="Count")
hist_rt

# plots of RT distributions in time bins: 

## FIGURE 3 ##
dat_study %>% 
  filter(rt < 6000) %>% 
  filter(correct == 1) %>%  # only plot correct responses
  mutate(t_min = factor(floor(time) + 1, ordered = TRUE)) %>% 
  mutate(block_lbl = ifelse(block == "A", "AT", "AS")) %>%  # make nicer labels
  ggplot(aes(rt, t_min, fill = block_lbl)) +
  scale_fill_manual(values=cbPalette) +
  scale_x_discrete(labels = c('Typing','Speaking')) +
  geom_density_ridges(scale = 2, alpha = .6,
                      # maybe adding medians will make the trend easier to see:
                      quantile_lines = TRUE, quantiles = 2) + 
  scale_y_discrete(expand = c(0, 0), breaks = 1:12, labels = 1:12) +     # remove the empty bin with 13 minutes at the top of the graph
  scale_x_continuous(expand = c(0, 0)) +   
  coord_cartesian(clip = "off") + 
  theme_ridges() +
  NULL +
  ggtitle("") + labs(x="RT (ms)", y="Time (minutes)", fill = "Block")

ggsave("fig3.pdf", width = 10, height = 5, path = "~/Desktop/plots" )


# parse the data:
dat_study_A <-dat_study[(dat_study$block == 'A'),]     # ss typing
dat_study_B <-dat_study[(dat_study$block == 'B'),]     # ss speaking
dat_study_C <-dat_study[(dat_study$block == 'C'),]     # flashcard speaking 

# calculate the number of items in the filtered dataset:
nrow(dat_study_A)
nrow(dat_study_B)
nrow(dat_study_A %>% 
       filter(rt < 6000))
nrow(dat_study_B %>% 
       filter(rt < 6000))

# In block A:
2151-2027           # 124 trials were removed...
(1-2027/2152)*100   # ... which is 5.4%

#In block B:
2685-2667               # 18 trials removed
(1 - (2667/2685))*100   # Which is 0.7 percent 

# Correlations 

ggplot(data = dat_study) + stat_summary(
  mapping = aes(y = rt, x = factor(ppn), color = factor(block)),
  fun.ymin = min,
  fun.ymax = max,
  fun.y = median
)

# make a scatter plot:
aggregate(rt ~ ppn + block, data = dat_study, FUN = median, na.rm = TRUE) %>% 
  pivot_wider(names_from = block, values_from = rt) %>% 
  ggplot(aes(A, B)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  coord_fixed() +
  ggtitle("Correlation plot") +  labs(x="Typing RT", y="Speaking RT")

rt_type = aggregate(dat_study_A$rt, list(dat_study_A$ppn), FUN=mean, na.rm = TRUE)
rt_speak = aggregate(dat_study_B$rt, list(dat_study_B$ppn), FUN=mean, na.rm = TRUE)

cor(rt_type$x, rt_speak$x, method = c("pearson"))  # pearson's R = 0.1
cor(rt_type$x, rt_speak$x, method = c("kendall"))  # pearson's K = 0.12

# Boxplot RT's:

boxplot_comp <- ggplot(dat_study, aes(x= factor(time_bins), y=rt, fill=block)) + 
  geom_boxplot(outlier.colour = NA) +
  ggtitle("Distribution RT over time") +  labs(x="Time (min)", y="RT (ms)")

# Show RT graphics in a single plot:
boxplot_comp + hist_rt

# 3.3. Analysis: which  (typing-based RT or speech-based RT) is a better predictor of performance?

# ACT-R mapping from activation to RT can be used to compute a predicted activation using
# the recorded reaction times:

dat_study$rt = dat_study$rt/1000  # rt to seconds
dat_study$expected_activation_from_rt = -log(dat_study$rt-0.3) # compute predicted activation based on RT
dat_study$expected_accuracy_from_rt = 1/(1+exp(((-0.8 - dat_study$expected_activation_from_rt)/0.1)))  # compute predicted accuracy based on prdicted RT

dat_study$exp_act_norm = normalize(dat_study$expected_activation_from_rt)

# again, parse data:
dat_study_A <-dat_study[(dat_study$block == 'A'),]     # ss typing
dat_study_B <-dat_study[(dat_study$block == 'B'),]     # ss speaking
dat_study_C <-dat_study[(dat_study$block == 'C'),]     # flashcard speaking 

# 3.3.1 predict actual acccuracy from rt-based expected activation, adding ppn and factid to the model.
# This model is conceptually simmilar to the the analysis in the UMAP paper. 

typing = glmer(correct ~ expected_activation_from_rt  + ((1 | ppn) + (1 | fact_id)), data = dat_study_A, family = 'binomial')
speaking = glmer(correct ~ expected_activation_from_rt + ((1 | ppn) + (1 | fact_id)), data = dat_study_B, family = 'binomial')

# AIC:
summary(typing)        # p < 0.001, AIC: 1817 
summary(speaking)      # p < 0.001, AIC: 1961

# pseudo-R2:
r2.corr.mer(typing)    # R2 = 0.14
r2.corr.mer(speaking)  # R2 = 0.30


# models with normalized RT's:
typing_norm = glmer(correct ~ exp_act_norm + ((1 | ppn) + (1 | fact_id)), data = dat_study_A, family = 'binomial')
speaking_norm = glmer(correct ~ exp_act_norm + ((1 | ppn) + (1 | fact_id)), data = dat_study_B, family = 'binomial')

# AIC:
summary(typing_norm)             # p < 0.001, AIC: 1817 
summary(speaking_norm)           # p < 0.001, AIC: 1961

# pseudo-R2:
r2.corr.mer(typing_norm)         # R2 = 0.14
r2.corr.mer(speaking_norm)       # R2 = 0.30


# plot (predicting accuracy from RT-based estimated accuracy)
ggplot(dat_study[(dat_study$block != 'C'),], aes(y=correct,x=expected_activation_from_rt, color = block)) + 
  geom_point(alpha = 0.1) +
  stat_smooth(method='glm', method.args=list(family='binomial'),se=TRUE) +
  ggtitle("Predicitng accuracy from RT") +  labs(x="Model estimated memory activation based on RT", y="Accuracy")


## FIGURE 4 ##
# plot (predicting accuracy from RT-based estimated accuracy (normalized))
acc_offset <- .025
dat_study %>% 
  mutate(Block = ifelse(block == "A", "AT", "AS")) %>% 
  # offset the actual accuracy a bit so the blocks don't overlap:
  mutate(offset_correct = ifelse(Block == "AT", correct - acc_offset, correct + acc_offset)) %>% 
  ggplot(aes(y=correct,x=normalize(expected_activation_from_rt), color = Block)) + 
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values=cbPalette) +
  geom_point(aes(y = offset_correct),
             alpha = 0.1, position = position_jitter(height = acc_offset), size = 1) +
  stat_smooth(method='glm', method.args=list(family='binomial'),se=TRUE) +
  #ggtitle("Predicting accuracy from reaction times") +  
  labs(x="Estimated activation (normalized)", y="Accuracy") + 
  theme_ridges() +
  scale_x_continuous(name = "Estimated activation (normalized)", sec.axis = sec_axis(~ exp(-(.) + 0.3) , name = 'RT(s)', breaks = c(12,6,3,1.5,0.8,0.4,0.2,0.1))) +
  NULL 
ggsave("fig4.pdf", width = 6, height = 5, path = "~/Desktop/plots" )


# 3.3.2 also add trial number or repetition to the analysis:

typing_w_trial = glmer(correct ~ expected_activation_from_rt + trial + ((1 | ppn) + (1 | fact_id)), data = dat_study_A, family = 'binomial')
speaking_w_trial = glmer(correct ~ expected_activation_from_rt + trial + ((1 | ppn) + (1 | fact_id)), data = dat_study_B, family = 'binomial')

typing_w_reps = glmer(correct ~ expected_activation_from_rt + repetition + ((1 | ppn) + (1 | fact_id)), data = dat_study_A, family = 'binomial')
speaking_w_reps = glmer(correct ~ expected_activation_from_rt + repetition + ((1 | ppn) + (1 | fact_id)), data = dat_study_B, family = 'binomial')

summary(typing_w_trial)        # p < 0.001, AIC: 1816
summary(speaking_w_trial)      # p < 0.001, AIC: 1944

r2.corr.mer(typing_w_trial)    # R2 = 0.15
r2.corr.mer(speaking_w_trial)  # R2 = 0.32

summary(typing_w_reps)        # p < 0.001, AIC: 1791
summary(speaking_w_reps)      # p < 0.001, AIC: 1935

r2.corr.mer(typing_w_reps)    # R2 = 0.17
r2.corr.mer(speaking_w_reps)  # R2 = 0.32

# 3.4 Estimate model fit: create a simulated dataset containting very short (500ms) normal (1500ms) slow (3000ms) and very slow (4500ms) responses:
# simulated_data = crossing(rt = c(0.5, 1.5, 3, 4.5), fact_id = 1:10, ppn = unique(dat$ppn), trial = 1:100)

simulated_data <- crossing(rt = c(0.5, 1.5, 3, 4.5), repetition = 1:15)

# calculated expected actiavtions for the simulated dataset:
simulated_data$expected_activation_from_rt =  -log(simulated_data$rt-0.3)
simulated_data$exp_act_norm =  normalize(-log(simulated_data$rt-0.3))

# estimate the model predictions for the typing-rt based model for the simulated dataset:
simulated_data$typing_prediction <- predict(typing_w_reps, newdata=simulated_data, type = 'response', re.form = NA)

# estimate the model predictions for the speaking-rt based model for the simulated dataset:
simulated_data$speaking_prediction <- predict(speaking_w_reps, newdata=simulated_data, type = 'response', re.form = NA)

# and normalized:
simulated_data$typing_prediction_norm <- predict(typing_norm, newdata=simulated_data, type = 'response', re.form = NA)
simulated_data$speaking_prediction_norm <- predict(speaking_norm, newdata=simulated_data, type = 'response', re.form = NA)


# plot the typing model
p1 <- ggplot(dat = simulated_data, aes(x = repetition, y = typing_prediction, color = factor(rt))) + 
  theme(legend.position="none") +
  geom_line() +
  geom_point () +
  scale_y_continuous(limits = c(0.5, 1)) +
  ggtitle("Simulated model fit for typing-based RT's") +  labs(x="Number of item repetitions", y="Predicted accuracy")

p1
# plot the speaking model:
p2 <- ggplot(dat = simulated_data, aes(x = repetition, y = speaking_prediction, color = factor(rt))) + 
  geom_line() +
  geom_point () +
  scale_y_continuous(limits = c(0.5, 1)) +
  ggtitle("Simulated model fit for speaking-based RT's") +  labs(x="Number of item repetitions", y="Predicted accuracy") +
  labs(color = "Reaction time (s)")

p1 + p2  # direct comparison on same y-scale

# Compute RoC's

# add predicted values to the dataframe:
dat_study_A$typing_fit <- predict(typing_norm, newdata=dat_study_A, type = 'response', allow.new.levels = TRUE)
dat_study_B$speaking_fit <- predict(speaking_norm, newdata=dat_study_B, type = 'response', allow.new.levels = TRUE)

# plot ROC for typing
### These calls to `roc()` crash my R session :O Not sure what's going on here...
pROC_typing <- roc(dat_study_A$correct,dat_study_A$typing_fit,
                   smoothed = TRUE,
                   # arguments for ci
                   ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                   # arguments for plot
                   plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                   print.auc=TRUE, show.thres=TRUE)


# plot ROC for speaking
pROC_speaking <- roc(dat_study_B$correct,dat_study_B$speaking_fit,
                     smoothed = TRUE,
                     # arguments for ci
                     ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                     # arguments for plot
                     plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                     print.auc=TRUE, show.thres=TRUE)

roc.test(pROC_typing, pROC_speaking)  # speaking is sign. better

## 4. MAGDEBURUG DATA ##

# 4.1 preprocessing and computing average RoF values

# remove instruction trials 
datm <- datm[(datm['trial_type'] == 'html-voicekey-response'),]


# drop irrelevant columns 
datm$value <- NULL
datm$trial_type <- NULL
datm$internal_node_id <- NULL
datm$subject <- NULL
datm$browser_info <- NULL
datm$view_history <- NULL
datm$keypresses <- NULL
datm$responses <- NULL
datm$text <- NULL
datm$threshold <- -0.8

# rename columns that need to be renamed 
colnames(datm)[which(names(datm) == "id")] <- "fact_id"
colnames(datm)[which(names(datm) == "presentation_start_time")] <- "start_time"
colnames(datm)[which(names(datm) == "participant_number")] <- "userId"
colnames(datm)[which(names(datm) == "answer")] <- "text"

# create a unique id for each participant and each fact:
participants <- unique(datm$userId)
facts <- unique(datm$fact_id)

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
    alpha_for_participant_and_fact <-calculate_alpha(Inf, current_fact_id, 0.3, datm[datm$userId == participant,])
    rofs[i,1] <- participant
    rofs[i,2] <- current_fact_id
    rofs[i,3] <- alpha_for_participant_and_fact
  }
}


# alpha summaries:
summary <- aggregate(rofs[,3], list(rofs$ppn), mean)
mem$RoF <- summary$x
correlations <- select(mem, CeradDistance, MOCA, RoF)
cor_heatmap_BF(correlations)


cor.test(correlations$RoF, correlations$CeradDistance, method = c("pearson"))  

# add memory scores to ss dataset 
datm$MOCA <- NA
datm$CERADd <- NA
for(i in unique(datm$userId)) {
  datm[datm$userId == i, 'MOCA']  <- mem[mem$Subject == i, 'MOCA']
  datm[datm$userId == i, 'CERADd']  <- mem[mem$Subject == i, 'CeradDistance']
}


# 4.2 Analyses: can we used speech-based RT as a proxy of actual accuracy, even in elderly/MCI populations?
datm[datm == Inf] <- 6000
datm[datm == 'Infinity'] <- 6000
datm[datm == '-Infinity'] <- -6000
datm[datm == -Inf] <- -6000
datm$rt <- as.numeric(datm$rt)/1000  # rt to seconds
datm$expected_activation_from_rt = -log(datm$rt-0.3) # compute predicted activation based on RT
datm$expected_accuracy_from_rt = 1/(1+exp(((-0.8 - datm$expected_activation_from_rt)/0.1)))  # compute predicted accuracy based on prdicted RT
datm$exp_act_norm = normalize(datm$expected_activation_from_rt)



# fit different models, using the MOCA-memroy score and the CERAD distance memory score 

# moca:
magdeburg_moca <- glmer(correct ~ expected_activation_from_rt + MOCA +((1 | userId) + (1 | fact_id)), data = datm, family = 'binomial')

# CERAD distance:
magdeburg_cerad <- glmer(correct ~ expected_activation_from_rt + CERADd +((1 | userId) + (1 | fact_id)), data = datm, family = 'binomial')

# none of both:
magdeburg <- glmer(correct ~ expected_activation_from_rt + ((1 | userId) + (1 | fact_id)), data = datm, family = 'binomial')

# moca w interaction effect:
magdeburg_moca_int <- glmer(correct ~ expected_activation_from_rt * MOCA +((1 | userId) + (1 | fact_id)), data = datm, family = 'binomial')

# cerad w interaction effect:
magdeburg_cerad_int <- glmer(correct ~ expected_activation_from_rt * CERADd +((1 | userId) + (1 | fact_id)), data = datm, family = 'binomial')

magdeburg_cerad_int_norm <- glmer(correct ~ exp_act_norm* CERADd +((1 | userId) + (1 | fact_id)), data = datm, family = 'binomial')


summary(magdeburg_cerad_int_norm)


anova(magdeburg, magdeburg_moca, magdeburg_cerad, magdeburg_cerad_int, magdeburg_moca_int)

# the CERAD-distance interaction model is the best model!

# simulate some data to check model fits:
simulated_data <- crossing(rt = c(0.5, 1.5, 3, 4.5), CERADd = c(-5,-2.5, 0,2.5, 5))
simulated_data$expected_activation_from_rt =  -log(simulated_data$rt-0.3)
simulated_data$prediction <- predict(magdeburg_cerad_int, newdata=simulated_data, type = 'response', re.form = NA)
simulated_data$exp_act_norm <- normalize(simulated_data$expected_activation_from_rt)
simulated_data$prediction_norm <- predict(magdeburg_cerad_int_norm, newdata=simulated_data, type = 'response', re.form = NA)

## FIGURE 6 ##
# plot cerad distance:
p1 <- ggplot(dat = simulated_data, aes(x = CERADd, y = prediction_norm, color = factor(rt))) + 
  #theme(legend.position="none") +
  geom_line(size=1) +
  geom_point () +
  scale_color_manual(values=cbPalette2) +
  scale_y_continuous(limits = c(0.4, 1)) +
  ggtitle("") +  labs(x="CERAD distance", y="Predicted accuracy") +
  theme_ridges() 
p1 + labs(color = "Reaction time (s)")

ggsave("fig6.pdf", width = 8, height = 5, path = "~/Desktop/plots" )


# plot MOCA:
simulated_data <- crossing(rt = c(0.5, 1.5, 3, 4.5), MOCA = c(20,22,24,26,28,30))
simulated_data$expected_activation_from_rt =  -log(simulated_data$rt-0.3)
simulated_data$prediction <- predict(magdeburg_moca_int, newdata=simulated_data, type = 'response', re.form = NA)

p2 <- ggplot(dat = simulated_data, aes(x = MOCA, y = prediction, color = factor(rt))) + 
  geom_line() +
  geom_point () +
  scale_y_continuous(limits = c(0.4, 1)) +
  ggtitle("Simulated model fit") +  labs(x="MOCA-score", y="Predicted accuracy")


p2 <- p2 + labs(color = "Reaction time (s)")

p1 <- p1 + labs(color = "Reaction time (s)")
p1 + theme_ridges()
options(warn = oldw)

## 5: redoing the originial UMAP analyses comparing block B and block C:
dat_BC <- dat[(dat$block != 'A'),]  #remove block A

correct <- glmer(correct ~ block + types + ((1 | ppn) + (1 | fact_id)), data = dat_BC, family = 'binomial')
summary(correct)

rt <- lmer(rt ~ block + types + ((1 | ppn) + (1 | fact_id)), data = dat_BC)
summary (rt)
  
## FIGURE 5 ##
pd <- position_dodge(0.4) # move them .05 to the left and right

RT <- dat_BC %>% 
  mutate(Block = ifelse(block == "B", "AS", "FS")) %>% 
  ggplot(aes(x=Block, y=rt, color = as.factor(types))) + 
  geom_line(stat="summary", fun.y="median", position = pd, alpha = 3/4, aes(group = types), size =1) +
  scale_color_manual(values=cbPalette2) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), position = pd, width = .2, size =1) +
  geom_point(stat="summary", fun.y="median", position = pd, size = 2.8) +
  ggtitle("") +  labs(x=NULL, y="Median RT (ms)") +
  theme_ridges() +
  NULL

correct <- dat_BC %>% 
  mutate(Block = ifelse(block == "B", "AS", "FS")) %>% 
  ggplot(aes(x=Block, y=correct, color = as.factor(types))) + 
  geom_line(stat="summary", fun.y="mean", position = pd, alpha = 3/4, aes(group = types), size =1) +
  scale_color_manual(values=cbPalette2) +
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), position = pd, width = .2, size =1) +
  geom_point(stat="summary", fun.y="mean", position = pd, size = 2.8) +
  ggtitle("") +  labs(x=NULL, y="Correctness (%)") +
  theme_ridges() +
  theme(legend.position="none")

correct +  RT + labs(color = "Session type")

ggsave("fig5.pdf", width = 10, height = 5, path = "~/Desktop/plots" )


## Supplementart figure on RT distributions ##
dat_study %>% 
  filter(correct != 'NA') %>%  # only plot correct responses
  mutate(block_lbl = ifelse(block == "A", "AT", "AS")) %>%  # make nicer labels
  ggplot(aes(rt, block_lbl, fill = as.factor(correct))) +
  scale_fill_manual(values=cbPalette2) +
  scale_x_discrete(labels = c('Typing','Speaking')) +
  geom_density_ridges(scale = 0.9, alpha = .6,
                      # maybe adding medians will make the trend easier to see:
                      quantile_lines = TRUE, quantiles = 2) + 
  scale_x_continuous(expand = c(0, 0)) +   
  coord_cartesian(clip = "off") + 
  theme_ridges() +
  NULL +
  ggtitle("") + labs(x="RT (s)", y="Block", fill = "Correct")
ggsave("fig_supp.pdf", width = 10, height = 5, path = "~/Desktop/plots" )

