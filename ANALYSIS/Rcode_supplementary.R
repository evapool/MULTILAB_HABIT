####################################################################################################
#                                                                                                  #
#                                                                                                  #                      
#         CHALLENGES AND PROMISES IN THE EXPERIMENTAL INDUCTION OF HABITS BY VARYING               # 
#               THE AMOUNT OF TRAINING: A MULTI-LABORATORY INVESTIGATION:                          #
#                                   SUPPLEMENTARY                                                  #
#                                                                                                  #
#                    International consortium for the study of habits                              #
#                                                                                                  #
#                                                                                                  #
# Created by E.R.P. on NOVEMBER 2018                                                               #
# Verified by R.G on DECEMBER 2018                                                                 #
# Modified by E.R.P on JULY 2020                                                                   #
# Verified by R.G and A.F. on August 2020                                                          #
####################################################################################################


library(car)
library(afex)
library(doBy)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggExtra)
library(BayesFactor)
library(sjstats)
library(jtools)
library(plyr)
library(dplyr)
library(tidyr)
library(psych)
library(emmeans)
library(devtools)

#---------------------------------------------------------------------------
#                    PRELIMINARY STUFF 
#---------------------------------------------------------------------------

# Set path
full_path       <- dirname(rstudioapi::getActiveDocumentContext()$path)
pos             <- regexpr("MULTILAB_HABIT", full_path)
home_path       <- substr(full_path, 1, pos+13)
figures_path    <- file.path(home_path,'ANALYSIS', 'figures')
utilities_path  <- file.path(home_path,'ANALYSIS','R')
setwd (home_path)

# source my utilites
source (file.path(utilities_path, 'getChangeIndex.R'))
source (file.path(utilities_path, 'getClassicIndex.R'))
source (file.path(utilities_path, 'makeIndividualDiffPlot.R'))
source (file.path(utilities_path, 'makeSplitGroupPlot.R'))
source (file.path(utilities_path, 'countTrialxCondition.R'))

# get tool
devtools::source_gist("2a1bb0133ff568cbe28d", 
                      filename = "geom_flat_violin.R")

# get database
FULL <- read.delim(file.path(home_path,'DATA/FULL_DATABASE.txt'), header = T, sep ='') # read in dataset

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

# code itemxcondition
CHANGE <- ddply(CHANGE, .(ID,prepost), countTrialxCondition)

# get total number of participants included
plyr::count(CHANGE$ID) # note that Caltech2 used a slightly different protocol so there are less repeat per condition

CHANGE$countTrialxCondition <- factor(CHANGE$countTrialxCondition)


#---------------------------------------------------------------------------
#  SUPPLEMENTARY STARTEGY 1: INDIVIDUAL DIFFERENCES APPROACH 
#---------------------------------------------------------------------------

# non collinar factor for ancova-like approach

#---------------------------- DATA REDUCTION TO EXTRACT ORTHOGONAL FACTORS ------

# prepare database for the FA
Q_EFA.means.ID <- aggregate(ANXIETY ~ ID * TICS_SOOV * TICS_PREPE * TICS_WODI * TICS_EXWO * TICS_LACK * TICS_SOTE * TICS_SOIS * TICS_WORY * TICS_WOOV * BIS_motor * BIS_attentional * BIS_nonplanning,
                            data = CHANGE, FUN = mean, na.action = na.pass) # we do not include the total scales
Q_EFA.means <- Q_EFA.means.ID
Q_EFA.means$ID <- NULL


# determine the number of factors
nFactor  <- fa.parallel(Q_EFA.means, fm = "ml")


# apply EFA with varimax rotation
quest.1.efa <- fa(r = Q_EFA.means, nfactors = 4, rotate = "varimax", fm = "ml")

print(quest.1.efa$loadings,cutoff = 0.0)

# create figure with EFA solution
fa.diagram(quest.1.efa)

# save the plot in the figures folder
dev.print(pdf, file.path(figures_path,'Figure_EFA_varimax.pdf'))
dev.off()

# calculate the factors loadings
s = factor.scores (Q_EFA.means, quest.1.efa) # 
s
#---------------------------- USE FACTOR AS AS MODERATOR IN THE MAIN ANALYSIS ----------

# merge with the FULL database
axes <- s$scores

# combine it with the participants ID
dat <- cbind(Q_EFA.means.ID, axes)
EFA_CHANGE <- join (CHANGE,dat, type = "full")

# run full model for each factor simoutanously
inter.whole = lmer(normPressFreq~ group*cue*prepost*(ML1+ML2+ML3+ML4) + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE, control = lmerControl(optimizer ="bobyqa"))
summary(inter.whole)
Confint(inter.whole, level = 0.95) 

# ----- assumptions check
plot(fitted(inter.whole),residuals(inter.whole)) 
qqnorm(residuals(inter.whole))
hist(residuals(inter.whole))

# test and different points of the model to understand interaction

# Stress affective -1 SD people low in axiety/stress have effect of overtraining
EFA_CHANGE$AFF_pSD <- scale(EFA_CHANGE$ML4, scale = T) + 1 # here I'm going to test at - 1SD (so people that are low in anxiety)
sslop.pSD = lmer(normPressFreq~ group*cue*prepost*AFF_pSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(sslop.pSD)
Confint(sslop.pSD, level = 0.95) 

# Stress affective +1 SD people high in axiety/stress have effect of overtraining
EFA_CHANGE$AFF_mSD <- scale(EFA_CHANGE$ML4, scale = T) - 1 # here I'm going to test at + 1SD (so people that are high in anxiety)
sslop.mSD = lmer(normPressFreq ~ group*cue*prepost*AFF_mSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(sslop.mSD)
Confint(sslop.mSD, level = 0.95) 

#---------------------------- FIGURE S1 ---------------------------

# this tests the model predictions as we do in lmer but does not allow to display distributions
AFF.means <- aggregate(EFA_CHANGE$normChangeBehav, by = list(EFA_CHANGE$ID, EFA_CHANGE$group, EFA_CHANGE$site, EFA_CHANGE$AFF_pSD, EFA_CHANGE$AFF_mSD, EFA_CHANGE$ML4), FUN='mean', na.rm = T) # extract means
colnames(AFF.means) <- c('ID','group','site', 'AFF_pSD', 'AFF_mSD','AFF', 'normChangeBehav')


# ADJUSTED MEANS in case we want see the estimations from the model
acqC1.aov      <- aov_car(normChangeBehav  ~ group*AFF +Error(ID), data = AFF.means, observed = c("AFF"), factorize = F, anova_table = list(es = "pes"))
acqC1.adjmeans <- emmeans(acqC1.aov, specs = c("group"), by = "AFF", at = list(AFF= c(-1, 1)))
acqC1.adjmeans

# to compute BF10
full <- lmBF(normChangeBehav~ group*AFF  + ID, data = AFF.means, 
whichRandom = "ID", iterations = 50000)
null <- lmBF(normChangeBehav~ group+AFF  + ID, data = AFF.means, 
whichRandom = "ID", iterations = 50000)
full/null

acqC1.low.aov      <- aov_car(normChangeBehav  ~ group*AFF_pSD +Error(ID), data = AFF.means, observed = c("AFF"), factorize = F, anova_table = list(es = "pes"))
acqC1.high.aov     <- aov_car(normChangeBehav  ~ group*AFF_mSD +Error(ID), data = AFF.means, observed = c("AFF"), factorize = F, anova_table = list(es = "pes"))


AFF.means$group           <- dplyr::recode(AFF.means$group, "1-day" = "Moderate", "3-day" = "Extensive" )


pp <- ggplot(AFF.means, aes(x = AFF, y = normChangeBehav, fill = group, color = group)) +
  geom_point(alpha = .2, position = position_jitterdodge(jitter.width = .0, jitter.height = 0)) +
  geom_smooth(method = lm, level = .95, alpha = .1, fullrange=TRUE) +
  ylab('Behavioral adaptation index ')+
  xlab('Stress Affect')+
  annotate("rect", xmin=0.95, xmax=1.05, ymin=min(AFF.means$normChangeBehav), ymax=max(AFF.means$normChangeBehav), alpha=0.3, fill="gray") +
  annotate("rect", xmin=-0.95, xmax=-1.05, ymin=min(AFF.means$normChangeBehav), ymax=max(AFF.means$normChangeBehav), alpha=0.3, fill="gray") +
  scale_fill_manual(values=c("#56B4E9", "#0F2080")) +
  scale_color_manual(values=c("#56B4E9", "#092C48")) +
  scale_x_continuous(breaks=seq(-2.5,2.5,0.5)) +
  theme_bw()

theme_continous_plot <- theme_bw(base_size = 18, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 18, face = "bold"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 22),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 22))

ppp <- pp + theme_continous_plot


pppp <- ggMarginal(ppp + theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.background = element_rect(color = "white")), 
                   type = "density", groupFill = T, color = NA, alpha = .2)

pdf(file.path(figures_path,'Figure_S1_IndividualDifferences_pannelA.pdf'))
print(pppp)
dev.off()

adj_meanCR  <- c(0.528, 0.163,  0.247, 0.369)
adj_lowerCL <- c(0.3293, -0.0307, 0.0480, 0.1759)
adj_upperCL <- c(0.726, 0.356, 0.446, 0.563)
adj_group   <- c("Moderate", "Extensive", "Moderate", "Extensive")
adj_SD      <- c("Lower Stress Affect (-1 SD)", "Lower Stress Affect (-1 SD)", "Higher Stress Affect (+1 SD)", "Higher Stress Affect (+1 SD)")
adj_means   <- data.frame(adj_meanCR, adj_lowerCL, adj_upperCL, adj_group, adj_SD)

adjmeans_plot <- ggplot(data = adj_means, aes(x = factor(adj_group, levels = c("Moderate","Extensive")), y = adj_meanCR, 
                                              color = adj_group,
                                              fill = adj_group)) + 
  geom_crossbar(aes(y = adj_meanCR, ymin =adj_lowerCL, ymax = adj_upperCL), width = 0.85 , alpha = 0.1) +
  facet_grid(~ factor(adj_SD, levels = c("Lower Stress Affect (-1 SD)","Higher Stress Affect (+1 SD)"))) +
  ylab('Behavioral adaptation index')+
  xlab('Amount of Training')+
  ylim(min= -1.5, max = 3)+
  scale_fill_manual(values=c("#0F2080","#56B4E9" )) +
  scale_color_manual(values=c( "#092C48","#56B4E9")) +
  theme_bw()


theme_means_plots <- theme_bw(base_size = 18, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 16, face = "bold"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22))

ppp <- adjmeans_plot+ theme_means_plots 


pdf(file.path(figures_path,'Figure_S1_IndividualDifferences_pannelB.pdf'))
print(ppp)
dev.off()



#---------------------------- MEDIAN SPLIT APPROACH NOT REPORTED ---------------------------

# figure for AFF: Streess Affect
AFF.means$StressAffect<- ntile(AFF.means$AFF, 2)
AFF.means$StressAffect<- factor(AFF.means$StressAffect)

# low stress affect
lowAff.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(AFF.means, StressAffect == '1'),
                          factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(3.65), df = c(1), df_error = c(95))


# Bayes factors 
lowAnx.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(AFF.means, StressAffect  == '1'), 
                     whichRandom = "ID", iterations = 50000)
lowAnx.BF <- recompute(lowAnx.BF, iterations = 50000)
lowAnx.BF[1]

# high anxiety
highAnx.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(AFF.means, StressAffect == '2'),
                           factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(0.41), df = c(1), df_error = c(94))


# Bayes factors
highAnx.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(AFF.means,  StressAffect == '2'), 
                      whichRandom = "ID", iterations = 50000)
highAnx.BF <- recompute(highAnx.BF, iterations = 50000)
highAnx.BF[1]

# rename variables for plot
AFF.means$StressAffect    <- dplyr::recode(AFF.means$StressAffect, "1" = "Lower Stress Affect", "2" = "Higher Stress Affect" )
AFF.means$group           <- dplyr::recode(AFF.means$group, "1-day" = "Moderate", "3-day" = "Extensive" )


pp <- ggplot(AFF.means, aes(x = group, y = normChangeBehav, fill = group, color = group)) +
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .2, aes(x = group, y = normChangeBehav, fill = factor(group, levels = c("Moderate","Extensive" ))), color = NA)+
  geom_point(alpha = .3, position = position_jitterdodge(jitter.width = .5, jitter.height = 0)) +
  geom_boxplot(alpha=0,outlier.alpha = 0) +
  ylab('Behavioral adaptation index')+
  xlab('Amount of Training')+
  facet_grid(~StressAffect)+
  scale_fill_manual(values=c("#56B4E9", "#0F2080")) +
  scale_color_manual(values=c("#56B4E9", "#092C48")) +
  theme_bw()

ppp <- pp + theme_bw(base_size = 20, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 18, face = "bold"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        legend.position="none",
        legend.text  = element_blank(),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22))

pdf(file.path(figures_path,'Figure_E1_IndividualDifferences.pdf'))
print(ppp)
dev.off()










#-----------------------------------------------------------------------------
# SUPPLEMENTARY STARTEGY 2: SEPARATE TESTS FOR ANXIETY AND WORRIES
#-----------------------------------------------------------------------------

# we conclude by intpreting our results in terms of anxiety and chronic worries 
# let make sure that is not an artifact of the EFA 

# -------------------- ANXIETY ----------------------------------------
CHANGE$ANXIETY_z <- scale(CHANGE$ANXIETY)
inter.anxiety = lmer(normPressFreq~ group*cue*prepost*ANXIETY_z + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = CHANGE, REML=FALSE, control = lmerControl(optimizer ="bobyqa"))
summary(inter.anxiety)
Confint(inter.anxiety, level = 0.95) 

# ----- assumptions check
plot(fitted(inter.anxiety),residuals(inter.anxiety)) 
qqnorm(residuals(inter.anxiety))
hist(residuals(inter.anxiety))

# test and different points of the model to understand interaction

# ANXIETY -1 SD people low in axiety/stress have effect of overtraining
CHANGE$ANX_pSD <- scale(CHANGE$ANXIETY_z, scale = T) + 1 # here I'm going to test at - 1SD (so people that are low in anxiety)
sslop.pSD = lmer(normPressFreq~ group*cue*prepost*ANX_pSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = CHANGE, REML=FALSE, control = lmerControl(optimizer ="bobyqa"))
summary(sslop.pSD)
Confint(sslop.pSD, level = 0.95) 

# ANXIETY +1 SD people high in axiety/stress have effect of overtraining
CHANGE$ANX_mSD <- scale(CHANGE$ANXIETY_z, scale = T) - 1 # here I'm going to test at + 1SD (so people that are high in anxiety)
sslop.mSD = lmer(normPressFreq ~ group*cue*prepost*ANX_mSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = CHANGE, REML=FALSE, control = lmerControl(optimizer ="bobyqa"))
summary(sslop.mSD)
Confint(sslop.mSD, level = 0.95) 


# -------------------- CHRONIC WORRIES ----------------------------------------
CHANGE$TICS_WORY_z <- scale(CHANGE$TICS_WORY)
inter.wory = lmer(normPressFreq~ group*cue*prepost*TICS_WORY_z + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(inter.wory)
Confint(inter.wory , level = 0.95) 

# ----- assumptions check
plot(fitted(inter.wory),residuals(inter.wory)) 
qqnorm(residuals(inter.wory))
hist(residuals(inter.wory))

# test and different points of the model to understand interaction

# WORRIES -1 SD people low in axiety/stress have effect of overtraining
CHANGE$WORY_pSD <- scale(CHANGE$TICS_WORY_z, scale = T) + 1 # here I'm going to test at - 1SD (so people that are low in anxiety)
sslop.pSD = lmer(normPressFreq~ group*cue*prepost*WORY_pSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(sslop.pSD)
Confint(sslop.pSD, level = 0.95) 

# WORRIES +1 SD people high in axiety/stress have effect of overtraining
CHANGE$WORY_mSD <- scale(CHANGE$TICS_WORY_z, scale = T) - 1 # here I'm going to test at + 1SD (so people that are high in anxiety)
sslop.mSD = lmer(normPressFreq ~ group*cue*prepost*WORY_mSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(sslop.mSD)
Confint(sslop.mSD, level = 0.95) 



# -------------------- FIGURE S2 ----------------------------------------
# this tests the model predictions as we do in lmer but does not allow to display distributions
ANXWORY.means <- aggregate(CHANGE$normChangeBehav, by = list(CHANGE$ID, CHANGE$group, CHANGE$site, CHANGE$TICS_WORY_z, CHANGE$ANXIETY_z), FUN='mean', na.rm = T) # extract means
colnames(ANXWORY.means) <- c('ID','group','site','Chronic_Worries','Anxiety', 'normChangeBehav')

# ADJUSTED MEANS in case we want see the estimations from the model
acqC1.anx      <- aov_car(normChangeBehav  ~ group*Anxiety +Error(ID), data = ANXWORY.means, observed = c("ANXIETY"), factorize = F, anova_table = list(es = "pes"))
acqC1.wory      <- aov_car(normChangeBehav  ~ group*Chronic_Worries +Error(ID), data = ANXWORY.means, observed = c("TICS_WORY"), factorize = F, anova_table = list(es = "pes"))


# rename variables for plot
ANXWORY.means$group    <- dplyr::recode(ANXWORY.means$group, "1-day" = "Moderate", "3-day" = "Extensive" )

#******************  anxiety ******************************************* 
pp <- ggplot(ANXWORY.means, aes(x =Anxiety, y = normChangeBehav, fill = group, color = group)) +
  geom_point(alpha = .2, position = position_jitterdodge(jitter.width = .0, jitter.height = 0)) +
  geom_smooth(method = lm, level = .95, alpha = .1, fullrange=TRUE) +
  ylab('Behavioral adaptation index') +
  xlab('Anxiety Level')+
  annotate("rect", xmin=0.95, xmax=1.05, ymin=min(ANXWORY.means$normChangeBehav), ymax=max(ANXWORY.means$normChangeBehav), alpha=0.3, fill="gray") +
  annotate("rect", xmin=-0.95, xmax=-1.05, ymin=min(ANXWORY.means$normChangeBehav), ymax=max(ANXWORY.means$normChangeBehav), alpha=0.3, fill="gray") +
  scale_fill_manual(values=c("#56B4E9", "#0F2080")) +
  scale_color_manual(values=c("#56B4E9", "#092C48")) +
  scale_x_continuous(breaks=seq(-2.5,2.5,0.5)) +
  theme_bw()

ppp <- pp + theme_continous_plot


pppp <- ggMarginal(ppp + theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.background = element_rect(color = "white")), 
                   type = "density", groupFill = T, color = NA, alpha = .2)

pdf(file.path(figures_path,'Figure_S2_PannelA.pdf'))
print(pppp)
dev.off()

#******************  chronic worries ******************************************* 
pp <- ggplot(ANXWORY.means, aes(x =Chronic_Worries, y = normChangeBehav, fill = group, color = group)) +
  geom_point(alpha = .2, position = position_jitterdodge(jitter.width = .0, jitter.height = 0)) +
  geom_smooth(method = lm, level = .95, alpha = .1, fullrange=TRUE) +
  ylab('Behavioral adaptation index') +
  xlab('Chronic Worries Level')+
  annotate("rect", xmin=0.95, xmax=1.05, ymin=min(ANXWORY.means$normChangeBehav), ymax=max(ANXWORY.means$normChangeBehav), alpha=0.3, fill="gray") +
  annotate("rect", xmin=-0.95, xmax=-1.05, ymin=min(ANXWORY.means$normChangeBehav), ymax=max(ANXWORY.means$normChangeBehav), alpha=0.3, fill="gray") +
  scale_fill_manual(values=c("#56B4E9", "#0F2080")) +
  scale_color_manual(values=c("#56B4E9", "#092C48")) +
  scale_x_continuous(breaks=seq(-2.5,2.5,0.5)) +
  theme_bw()

ppp <- pp + theme_continous_plot


pppp <- ggMarginal(ppp + theme(legend.position = c(1, 1), legend.justification = c(1, 1), legend.background = element_rect(color = "white")), 
                   type = "density", groupFill = T, color = NA, alpha = .2)

pdf(file.path(figures_path,'Figure_S2_Pannelb.pdf'))
print(pppp)
dev.off()


# -------------------- FIGURE S2 media split not reported ----------------------------------------
# *********  ANXIETY   ***************************************************
# this tests the model predictions as we do in lmer but does not allow to display distributions
ANX.means <- aggregate(CHANGE$normChangeBehav, by = list(CHANGE$ID, CHANGE$group, CHANGE$site, CHANGE$ANXIETY), FUN='mean', na.rm = T) # extract means
colnames(ANX.means) <- c('ID','group','site','ANXIETY', 'normChangeBehav')
# create median splits
ANX.means$AnxietySplit <- factor(ntile(ANX.means$ANXIETY, 2))
plyr::count(ANX.means$ID)

# low anxiety affect
lowAnx.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(ANX.means, AnxietySplit == '1'),
                          factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(6.19), df = c(1), df_error = c(100))

# Bayes factors 
lowAnx.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(ANX.means, AnxietySplit == '1'), 
                     whichRandom = "ID", iterations = 50000)
lowAnx.BF <- recompute(lowAnx.BF, iterations = 50000)
lowAnx.BF[1]

# high worries
highAnx.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(ANX.means, AnxietySplit == '2'),
                           factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(0.19), df = c(1), df_error = c(99))

# Bayes factors
highAnx.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(ANX.means, AnxietySplit == '2'), 
                      whichRandom = "ID", iterations = 50000)
highAnx.BF <- recompute(highAnx.BF, iterations = 50000)
highAnx.BF[1]


# *********  WORRIES   ***************************************************
# this tests the model predictions as we do in lmer but does not allow to display distributions
WORY.means <- aggregate(CHANGE$normChangeBehav, by = list(CHANGE$ID, CHANGE$group, CHANGE$site, CHANGE$TICS_WORY), FUN='mean', na.rm = T) # extract means
colnames(WORY.means) <- c('ID','group','site','TICS_WORY', 'normChangeBehav')
# create median splits
WORY.means$WorriesSplit <- factor(ntile(WORY.means$TICS_WORY, 2))
plyr::count(WORY.means$ID)

# low anxiety affect
lowWory.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(WORY.means, WorriesSplit == '1'),
                           factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(5.18), df = c(1), df_error = c(99))

# Bayes factors 
lowWory.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(WORY.means, WorriesSplit == '1'), 
                      whichRandom = "ID", iterations = 50000)
lowWory.BF <- recompute(lowWory.BF, iterations = 50000)
lowWory.BF[1]

# high anxiety
highWory.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(WORY.means, WorriesSplit  == '2'),
                            factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(0.11), df = c(1), df_error = c(99))

# Bayes factors
highWory.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(WORY.means, WorriesSplit  == '2'), 
                       whichRandom = "ID", iterations = 50000)
highWory.BF <- recompute(highWory.BF, iterations = 50000)
highWory.BF[1]


# ------------- Create databse and do figure

# this tests the model predictions as we do in lmer but does not allow to display distributions
ANXWORY.means <- aggregate(CHANGE$normChangeBehav, by = list(CHANGE$ID, CHANGE$group, CHANGE$site, CHANGE$TICS_WORY, CHANGE$ANXIETY), FUN='mean', na.rm = T) # extract means
colnames(ANXWORY.means) <- c('ID','group','site','TICS_WORY','ANXIETY', 'normChangeBehav')


# create median splits
ANXWORY.means$Anxiety <- factor(ntile(ANXWORY.means$ANXIETY, 2))
ANXWORY.means$ChronicWorries <- factor(ntile(ANXWORY.means$TICS_WORY, 2))

MC <- gather(ANXWORY.means, scale, level, Anxiety, ChronicWorries, factor_key=TRUE)

# rename variables for plot
MC$level    <- dplyr::recode(MC$level, "1" = "Lower Level", "2" = "Higher Level" )
MC$group    <- dplyr::recode(MC$group, "1-day" = "Moderate", "3-day" = "Extensive" )


pp <- ggplot(MC, aes(x = group, y = normChangeBehav, fill = group, color = group)) +
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .3, aes(x = group, y = normChangeBehav, fill = factor(group, levels = c("Moderate","Extensive" ))), color = NA)+
  geom_point(alpha = .2, position = position_jitterdodge(jitter.width = .5, jitter.height = 0)) +
  geom_boxplot(alpha=0,outlier.alpha = 0) +
  ylab('Behavioral adaptation index')+
  xlab('Amount of Training')+
  facet_grid(scale~factor(level,levels=c("Lower Level","Higher Level")))+
  scale_fill_manual(values=c("#56B4E9", "#0F2080")) +
  scale_color_manual(values=c("#56B4E9", "#092C48")) +
  theme_bw()


ppp <- pp + theme_bw(base_size = 20, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 18, face = "bold"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        legend.position="none",
        legend.text  = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

pdf(file.path(figures_path,'Figure_S2_Anxiety_Worries.pdf'),width=7,height=8)
print(ppp)
dev.off()






