library("tidyverse")
library("dplyr")
library("ggplot2")

#Read file
df <- read.csv("Exp1_results.csv")

#Remove fillers
(critical_items <- filter(df, condition1 %in% c('norep_vrep', 'norep_crep', 'vrep_norep', 'crep_norep')))

#Remove other columns
trial_data <- select(critical_items, filename, condition1, RT)

#Unify conditions (from 4, to 2)
trials_by_condition <- mutate(trial_data, condition = if_else(grepl('vrep',condition1) , 'vrep','crep'),
                              RT = if_else(RT==1,1,0))

#Remove NAs, keep only correct responses and average by condition (72 data points)
remove_na <- trials_by_condition %>% group_by(filename, condition) %>% 
    summarise(proportion_of_repetitions = sum(RT, na.rm=TRUE)/n())

#Plot data as histogram
ggplot(data = remove_na, aes(x=proportion_of_repetitions, fill=condition)) +
    geom_histogram(position='dodge', bins=10) + facet_grid(condition~.,)

#Plot data as box plots
ggplot(remove_na, aes (x = condition, y = proportion_of_repetitions)) + geom_boxplot() + xlab("Condition") + ylab("Proportion of responses")
    
#Paired t-test
paired_t_test <- t.test(proportion_of_repetitions ~ condition, data = remove_na, paired = TRUE)


#Separate conditions (one column for vreps, another one for creps)
results <- spread(remove_na, condition, proportion_of_repetitions)

#Statistical summary for creps
summary(results$crep)

#Statistical summary for vreps
summary(results$vrep)

#One-sample t-test for creps
t_test_crep <- t.test(results$crep, mu=0.5)


#One-sample t-test for vreps
t_test_vrep <- t.test(results$vrep, mu=0.5)


#############################
#Analysis without participants who studied Spanish

#Check for participants who studied Spanish
studied_Spanish <- filter(df, grepl('Spanish',response)) %>% select(filename)

#Remove files of participants who have studied Spanish
no_Spanish <- filter (df, !(filename %in% c('274389_210629_112956.csv', '274389_210629_113711.csv', '274389_210630_114138.csv', '274389_210630_115643.csv', '274389_210630_121047.csv', '274389_210630_121347.csv', '274389_210630_121838.csv')))

#Remove fillers
no_Spanish_critical_items <- filter(no_Spanish, condition1 %in% c('norep_vrep', 'norep_crep', 'vrep_norep', 'crep_norep'))

#Remove other columns
no_Spanish_trial_data <- select(no_Spanish_critical_items, filename, condition1, RT)

#Unify conditions (from 4, to 2)
no_Spanish_trials_by_condition <- mutate(no_Spanish_trial_data, condition = if_else(grepl('vrep',condition1) , 'vrep','crep'),
                              RT = if_else(RT==1,1,0))

#Remove NAs, keep only correct responses and average by condition (72 data points)
no_Spanish_remove_na <- no_Spanish_trials_by_condition %>% group_by(filename, condition) %>% 
    summarise(proportion_of_repetitions = sum(RT, na.rm=TRUE)/n())

#Plot data as histogram
ggplot(data = no_Spanish_remove_na, aes(x=proportion_of_repetitions, fill=condition)) +
    geom_histogram(position='dodge', bins=10) + facet_grid(condition~.,)

#Plot data as box plots
ggplot(no_Spanish_remove_na, aes (x = condition, y = proportion_of_repetitions)) + geom_boxplot() + xlab("Condition") + ylab("Proportion of responses")

#Paired t-test
no_Spanish_paired_t_test <- t.test(proportion_of_repetitions ~ condition, data = no_Spanish_remove_na, paired = TRUE)
paired_t_test

#Separate conditions (one column for vreps, another one for creps)
no_Spanish_results <- spread(no_Spanish_remove_na, condition, proportion_of_repetitions)

#Statistical summary for creps
summary(no_Spanish_results$crep)

#Statistical summary for vreps
summary(no_Spanish_results$vrep)

#One-sample t-test for creps
no_Spanish_t_test_crep <- t.test(no_Spanish_results$crep, mu=0.5)

#One-sample t-test for vreps
no_Spanish_t_test_vrep <- t.test(no_Spanish_results$vrep, mu=0.5)

#############################
#Analysis without very high RTs


#Look at RTs
summary(critical_items$responseCode)
meanRT <- mean(critical_items$responseCode)

#Plot data as histogram
ggplot(data = critical_items, aes(x=responseCode/1000)) +
    geom_histogram(bins= 100, fill = "white", colour = "black") + 
    scale_y_continuous(trans='log')+
    xlab("Response in seconds")+
    coord_cartesian(xlim = c(0,20))

#Let's say that RTs above 2.5SDs are outliers, and below 250ms and remove them from the data.
RTCut <- critical_items %>% filter(responseCode <= meanRT*2.5 & responseCode > 250) %>% 
         select(filename, stim1, condition1, RT) %>% 
    mutate(condition = if_else(grepl('vrep',condition1) , 'vrep','crep'),
                RT = if_else(RT==1,1,0))
RTCut$RT[is.na(RTCut$RT)] <- 0

#Plot data as box plots
ggplot(RTCut, aes (x = condition, y = RT)) + geom_boxplot() + xlab("Condition") + ylab("Proportion of responses")

# Run binomial analysis
library(lme4)
m.bin <- glmer(RT ~ condition + (1|filename) + (1|stim1), family=binomial, 
               data=RTCut, control=glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5)))
summary(m.bin)

# Run power analysis of the mixed effects model
library(simr)
power <-powerSim(m.bin, nsim=200)
power

# With this sample size, there is a 93.5% chance that the
# null effect of Condition is a false negative. However, the 
# The difference between c-rep (mean 0.440) and v-rep (0.453) is very small. 
# And descriptively c-rep is more dispreferred than v-rep.

# continued
no_high_RT <- RTCut %>% 
    group_by(filename, condition) %>% 
    summarise(proportion_of_repetitions = sum(RT, na.rm=TRUE)/n())


#Plot data as histogram
ggplot(data = no_high_RT, aes(x=proportion_of_repetitions, fill=condition)) +
    geom_histogram(position='dodge', bins=10) + facet_grid(condition~.,)

#Plot data as box plots
ggplot(no_high_RT, aes (x = condition, y = proportion_of_repetitions)) + geom_boxplot() + xlab("Condition") + ylab("Proportion of responses")

#Paired t-test
no_high_RT_paired_t_test <- t.test(proportion_of_repetitions ~ condition, data = no_high_RT, paired = TRUE)

#Separate conditions (one column for vreps, another one for creps)
no_high_RT_results <- spread(no_high_RT, condition, proportion_of_repetitions)

#Statistical summary for creps
summary(no_high_RT_results$crep)

#Statistical summary for vreps
summary(no_high_RT_results$vrep)

#One-sample t-test for creps
no_high_RT_t_test_crep <- t.test(no_high_RT_results$crep, mu=0.5)


#One-sample t-test for vreps
no_high_RT_t_test_vrep <- t.test(no_high_RT_results$vrep, mu=0.5)

###############

#Should I do an analysis removing both participants who studied Spanish and very high RTs?
