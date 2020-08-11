####################################################################################################
# R code for the supplementary analysis of the PILOT DATA
# "Does anxiety moderate training duration effects on habits in humans?  Determining the effects of
# anxiety on the experimental induction of habits in an instrumental outcome devaluation task"

## Last modified by Eva on NOVEMBER 2018
####################################################################################################

#----------------------------------    PRELIMINARY STUFF     --------------------------------------

if(!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
pacman::p_load(car, lme4, lmerTest, pbkrtest, ggplot2, dplyr, plyr, tidyr, multcomp, mvoutlier, HH, doBy, psych, pastecs, reshape, reshape2,
               jtools, effects, compute.es, DescTools, MBESS, afex, ez, metafor, influence.ME, olsrr)

require(lattice)

# Set path
full_path       <- dirname(rstudioapi::getActiveDocumentContext()$path)
pos             <- regexpr("PILOT_DATA", full_path)
home_path       <- substr(full_path, 1, pos+9)
utilities_path  <- file.path(home_path,'ANALYSIS','interindividual','R')
setwd (home_path)

source (file.path(utilities_path, 'normalizeVariablesBehavior.R'))
source (file.path(utilities_path, 'normalizeVariablesQuestionnaires.R'))

#----------------------------- DETERMINE THE INFLUENCE OF CALTECH OUTLEYER ----------------------------

# get databases
Q.CALTECH2 <- read.delim(file.path(home_path,'DATABASES/CALTECH_V2_QUESTIONNARIES.txt'), header = T, sep ='') # read in dataset
Q.CALTECH2 <- normalizeVariablesQuestionnaires(Q.CALTECH2) # we need to normalize for each center individually

P.CALTECH2 <- read.delim(file.path(home_path,'DATABASES/CALTECH_V2.txt'), header = T, sep ='') # read in dataset
P.CALTECH2 <- normalizeVariablesBehavior(P.CALTECH2) # we need to normalize for each center individually

FULL   <- join (Q.CALTECH2, P.CALTECH2, type = "full")

# define factors
FULL$site      <- factor(FULL$site)
FULL$ID        <- factor(FULL$ID)
FULL$session   <- factor(FULL$session)
FULL$run       <- factor(FULL$run)
FULL$trial     <- factor(FULL$trial)
FULL$cue       <- factor(FULL$cue)
FULL$prepost   <- factor(FULL$prepost)
FULL$group     <- factor(FULL$group)

# remove the baseline condition from the data
FULL <- subset(FULL, cue == 'Valued' | cue == 'Devalued')

# get the last run of the last training session and all the runs after satiation
DAY1   <- subset(FULL, group == '1-day')
DAY3   <- subset(FULL, group == '3-day')

DAY1 <- ddply(DAY1, .(ID), transform, averagePress  = mean(pressFreq[prepost=="pre"]))
DAY3 <- ddply(DAY3, .(ID), transform, averagePress  = mean(pressFreq[prepost=="pre"]))

C.DAY1 <- subset(DAY1, run == '2' | run == '3')
DAY3   <- subset(DAY3, session == '3') # we want the last day only
C.DAY3 <- subset(DAY3, run == '4' | run == '5')

CHANGE <- rbind(C.DAY1,C.DAY3)

# get variable of interest
CHANGE <- ddply(CHANGE, .(ID), transform, normChangeBehav  = (mean(normPressFreq[prepost=="post" & cue=='Valued']) - mean(normPressFreq[prepost=="pre" & cue=='Valued'])) - (mean(normPressFreq[prepost=="post" & cue=='Devalued']) - mean(normPressFreq[prepost=="pre" & cue=='Devalued'])))
CHANGE <- ddply(CHANGE, .(ID), transform, normChangeLiking = (mean(normLiking[prepost=="post" & cue=='Valued']) - mean(normLiking[prepost=="pre" & cue=='Valued'])) - (mean(normLiking[prepost=="post" & cue=='Devalued']) - mean(normLiking[prepost=="pre" & cue=='Devalued'])))


# Plot distributions of interest
bg_b = ddply(CHANGE,.(ID,group),summarise,normChangeBehav=mean(normChangeBehav))

behav =data.frame(bg_b$normChangeBehav)
behav$group = bg_b$group
behav$typeMeasure <- 'changeBehavior'
colnames(behav) [1] <- "Normscore"

ggplot(behav, aes(Normscore, fill = group)) +
  geom_histogram(aes(y=..density..),alpha=0.9,binwidth=0.2)+
  theme_bw()+
  labs(
    title = '',
    x = 'Normalized Change in Behavior',
    y = "Density"
  )

ggplot(behav, aes(x = group, y = Normscore, fill = group, color = group)) +
  geom_point() +
  geom_violin(aes(color = group, fill = group), alpha = .3, size = .1) +
  theme_bw()+
  labs(
    title = '',
    x = 'Normalized Change in Behavior',
    y = "Density"
  )

# assess the influence of the outlyer on the data
change.inter = lmer(normPressFreq ~  cue*prepost*group  + (cue*prepost|ID) + (1|trial), data = CHANGE, REML=FALSE)
change.basic = lmer(normPressFreq ~ (cue+prepost+group)^2  + (cue*prepost|ID) + (1|trial), data = CHANGE, REML=FALSE)

influence.datapoint <- influence(change.inter, "ID")
plot(cooks.distance(influence.datapoint))

mdl = lm (Normscore ~ group, data = behav)
ols_plot_cooksd_bar(mdl)
ols_plot_dfbetas(mdl)
ols_plot_dffits(mdl)
ols_plot_resid_stand(mdl)
ols_plot_resid_lev(mdl)
ols_plot_resid_stud_fit(mdl)
ols_plot_hadi(mdl)


# more traditional indexes
mean_all    = mean(bg_b$normChangeBehav)
sd_all      = sd(bg_b$normChangeBehav)

median_all  = median(bg_b$normChangeBehav)
iqr_all     = IQR(bg_b$normChangeBehav)

thrashold1  = mean_all+(3*sd_all) #2.910910901
threshhold2 = median_all+(3*iqr_all)
