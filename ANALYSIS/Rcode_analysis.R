####################################################################################################
#                                                                                                  #
#                                                                                                  #                      
#         CHALLENGES AND PROMISES IN THE EXPERIMENTAL INDUCTION OF HABITS BY VARYING               # 
#               THE AMOUNT OF TRAINING: A MULTI-LABORATORY INVESTIGATION                           #
#                                                                                                  #
#                                                                                                  #
#                    International consortium for the study of habits                              #
#                                                                                                  #
#                                                                                                  #
# Created by E.R.P. on NOVEMBER 2018                                                               #
# Verified by R.G on DECEMBER 2018                                                                 #
# Modified by E.R.P on JULY 2020                                                                   #
# Verified by R.G  on August 2020                                                                  #
####################################################################################################


library(car)
library(afex)
library(doBy)
library(lme4)
library(lmerTest)
library(ggplot2)
library(BayesFactor)
library(sjstats)
library(jtools)
library(plyr)
library(dplyr)
library(tidyr)
library(metafor)
library(rmcorr)
library(flexmix)
library(psych)
library(emmeans)
library(devtools)
library(effectsize)

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

# subset by site
C.CALTECH = subset(CHANGE, site == 'Caltech1')
C.CALTECH2= subset(CHANGE, site == 'Caltech2')
C.HAMBURG = subset(CHANGE, site == 'Hamburg')
C.SYDNEY  = subset(CHANGE, site == 'Sydney')
C.TELAVIV = subset(CHANGE, site == 'Tel_Aviv')






#---------------------------------------------------------------------------
#                   MANIPULACTION CHECKS 
#---------------------------------------------------------------------------

#----------------------------- HUNGER-------------------------------------
HUNGER.means <- aggregate(CHANGE$hunger, by = list(CHANGE$ID, CHANGE$group, CHANGE$prepost,CHANGE$site), FUN='mean') # extract means
colnames(HUNGER.means) <- c('ID','group','prepost','site','hunger')
HUNGER.means = subset(HUNGER.means, !ID == "115") # the recording of the  hunger rating  for this participant was done on the wrong scale

#--------- pasadena 1
# main
hunger.c1.stat <- aov_car(hunger ~ group*prepost + Error (ID/prepost), data = subset(HUNGER.means, site == 'Caltech1'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(197.80), df = c(1), df_error = c(60))

# Bayes factors
hunger.c1.BF <- anovaBF(hunger ~ group*prepost  + ID, data = subset(HUNGER.means, site == 'Caltech1'), 
                      whichRandom = "ID", iterations = 50000)
hunger.c1.BF <- recompute(hunger.c1.BF, iterations = 50000)
hunger.c1.BF[2]# main prepost


#--------- pasadena 2
# main
hunger.c2.stat <- aov_car(hunger ~ group*prepost + Error (ID/prepost), data = subset(HUNGER.means, site == 'Caltech2'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(92.40), df = c(1), df_error = c(57))

# Bayes factors
hunger.c2.BF <- anovaBF(hunger ~ group*prepost  + ID, data = subset(HUNGER.means, site == 'Caltech2'), 
                        whichRandom = "ID", iterations = 50000)
hunger.c2.BF <- recompute(hunger.c2.BF, iterations = 50000)
hunger.c2.BF[2] # main prepost


#--------- hamburg

#main
hunger.h.stat <- aov_car(hunger ~ group*prepost + Error (ID/prepost), data = subset(HUNGER.means, site == 'Hamburg'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(230.55), df = c(1), df_error = c(62))

# Bayes factors
hunger.h.BF <- anovaBF(hunger ~ group*prepost  + ID, data = subset(HUNGER.means, site == 'Hamburg'), 
                        whichRandom = "ID", iterations = 50000)
hunger.h.BF <- recompute(hunger.h.BF, iterations = 50000)
hunger.h.BF[2] # main prepost



#--------- sydney
#main
hunger.s.stat <- aov_car(hunger ~ group*prepost + Error (ID/prepost), data = subset(HUNGER.means, site == 'Sydney'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(309.26), df = c(1), df_error = c(58))

# Bayes factors
hunger.s.BF <- anovaBF(hunger ~ group*prepost  + ID, data = subset(HUNGER.means, site == 'Sydney'), 
                       whichRandom = "ID", iterations = 50000)
hunger.s.BF <- recompute(hunger.s.BF, iterations = 50000)
hunger.s.BF[2] # main prepost



#--------- tel-aviv
#main
hunger.t.stat <- aov_car(hunger ~ group*prepost + Error (ID/prepost), data = subset(HUNGER.means, site == 'Tel_Aviv'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(236.92), df = c(1), df_error = c(58))

# Bayes factors
hunger.t.BF <- anovaBF(hunger ~ group*prepost  + ID, data = subset(HUNGER.means, site == 'Tel_Aviv'), 
                       whichRandom = "ID", iterations = 50000)
hunger.t.BF <- recompute(hunger.t.BF, iterations = 50000)
hunger.t.BF[2] # main prepost


#----------------------------- SNACK PLEASANTNESS-------------------------------------

SNACK.means <- aggregate(CHANGE$outcomeliking, by = list(CHANGE$ID, CHANGE$prepost, CHANGE$cue, CHANGE$group, CHANGE$site), FUN='mean') # extract means
colnames(SNACK.means) <- c('ID','prepost','cue','group', 'site','outcomeliking')
SNACK.index <- ddply(SNACK.means, .(ID, prepost), transform, outcomeliking  = outcomeliking-outcomeliking[cue=="Devalued"])
SNACK.index <- subset(SNACK.index, cue!='Devalued')



#--------- pasadena 1
#main
snack.c1.stat <- aov_car(outcomeliking ~ group*prepost + Error (ID/prepost), data = subset(SNACK.index, site == 'Caltech1'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c( 173.84), df = c(1), df_error = c(60))

# Bayes factors
snack.c1.BF <- anovaBF(outcomeliking ~ group*prepost  + ID, data = subset(SNACK.index, site == 'Caltech1'), 
                        whichRandom = "ID", iterations = 50000)
snack.c1.BF <- recompute(snack.c1.BF, iterations = 50000)
snack.c1.BF[1]# main prepost

#--------- pasadena 2
#main
snack.c2.stat <- aov_car(outcomeliking ~ group*prepost + Error (ID/prepost), data = subset(SNACK.index, site == 'Caltech2'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(51.07), df = c(1), df_error = c(57))

# Bayes factors
snack.c2.BF <- anovaBF(outcomeliking ~ group*prepost  + ID, data = subset(SNACK.index, site == 'Caltech2'), 
                       whichRandom = "ID", iterations = 50000)
snack.c2.BF <- recompute(snack.c2.BF, iterations = 50000)
snack.c2.BF[1]# main prepost



#--------- hamburg

#main
snack.h.stat <- aov_car(outcomeliking ~ group*prepost + Error (ID/prepost), data = subset(SNACK.index, site == 'Hamburg'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(92.38), df = c(1), df_error = c(63))

# Bayes factors
snack.h.BF <- anovaBF(outcomeliking ~ group*prepost  + ID, data = subset(SNACK.index, site == 'Hamburg'), 
                       whichRandom = "ID", iterations = 50000)
snack.h.BF <- recompute(snack.h.BF, iterations = 50000)
snack.h.BF[1]# main prepost


#--------- sydeny
#main
snack.s.stat <- aov_car(outcomeliking ~ group*prepost + Error (ID/prepost), data = subset(SNACK.index, site == 'Sydney'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(87.72), df = c(1), df_error = c(58))

# Bayes factors
snack.s.BF <- anovaBF(outcomeliking ~ group*prepost  + ID, data = subset(SNACK.index, site == 'Sydney'), 
                      whichRandom = "ID", iterations = 50000)
snack.s.BF <- recompute(snack.s.BF, iterations = 50000)
snack.s.BF[1]



#--------- sydeny
#main
snack.t.stat <- aov_car(outcomeliking ~ group*prepost + Error (ID/prepost), data = subset(SNACK.index, site == 'Tel_Aviv'), anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(44.46), df = c(1), df_error = c(58))

# Bayes factors
snack.t.BF <- anovaBF(outcomeliking ~ group*prepost  + ID, data = subset(SNACK.index, site == 'Tel_Aviv'), 
                      whichRandom = "ID", iterations = 50000)
snack.t.BF <- recompute(snack.t.BF, iterations = 50000)
snack.t.BF[1]# main prepost


#----------------------------- FIGURE -------------------------
MC_wide <- join (HUNGER.means, SNACK.index)
MC <- gather(MC_wide, question, ratings, hunger, outcomeliking, factor_key=TRUE)

MC$question  <- dplyr::recode(MC$question, "hunger" = "Hunger ratings", "outcomeliking" = "Liking ratings [valued - devalued]" )
MC$prepost   <- dplyr::recode(MC$prepost, "pre" = "Pre", "post" = "Post" )
MC$site      <- dplyr::recode(MC$site, "Caltech1" = "Pasadena 1",
                              "Caltech2" = "Pasadena 2",
                              "Tel_Aviv" = "Tel-Aviv")


pp <- ggplot(MC, aes(x = factor(prepost, level = c("Pre","Post")), y = ratings, fill = site, color = site)) +
  geom_point(alpha = .1, position = position_jitterdodge(jitter.width = .5, jitter.height = 0)) +
  geom_line(data = MC, aes(group = ID, y = ratings, color = site), alpha = .2, size = 0.3) +
  geom_boxplot(alpha=0.3,outlier.alpha = 0) +
  ylab('')+
  xlab('Devaluation')+
  facet_grid(question~site,scales = "free")+
  scale_fill_manual(values=c("#660099", "#006600","#CD853F", "#0F2080","#990000")) +
  scale_color_manual(values=c("#660099", "#006600","#CD853F", "#0F2080","#990000")) +
  theme_bw()

ppp <- pp + theme_bw(base_size = 14, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        legend.position="none",
        legend.text  = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold"))

pdf(file.path(figures_path,'Figure_ManipulationCheck.pdf'))
print(ppp)
dev.off()





#---------------------------------------------------------------------------
#                  OUTCOME DEVALUATION CHANGES BY SITE
#---------------------------------------------------------------------------


# ---------------------------- PASADENA 1 ----------------------------------

# get database
C.CALTECH = subset(CHANGE, site == 'Caltech1')
CALTECH.index <- getChangeIndex(C.CALTECH)# aggregate based on pre-post

# stat
int.c1.stat <- aov_car(pressFreq ~ group*cue + Error (ID/cue), data = CALTECH.index, anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(0.73), df = c(1), df_error = c(60))

# Bayes factors
int.c1.BF <- anovaBF(pressFreq ~ group*cue  + ID, data = CALTECH.index, 
                        whichRandom = "ID", iterations = 50000)
int.c1.BF <- recompute(int.c1.BF, iterations = 50000)
int.c1.BF[4]/int.c1.BF[3]

# ---------------------------- PASADENA 2 ----------------------------------

#database
C.CALTECH = subset(CHANGE, site == 'Caltech2')
CALTECH2.index <- getChangeIndex(C.CALTECH)# aggregate based on pre-post

# stat
int.c2.stat <- aov_car(pressFreq ~ group*cue + Error (ID/cue), data = CALTECH2.index, anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(1.41), df = c(1), df_error = c(57))


# Bayes factors
int.c2.BF <- anovaBF(pressFreq ~ group*cue  + ID, data = CALTECH2.index, 
                     whichRandom = "ID", iterations = 50000)
int.c2.BF <- recompute(int.c2.BF, iterations = 50000)
int.c2.BF[4]/int.c2.BF[3]

# ---------------------------- HAMBURG  ----------------------------------

#database
C.HAMBURG = subset(CHANGE, site == 'Hamburg')
HAMBURG.index <- getChangeIndex(C.HAMBURG)# aggregate based on pre-post

# stat
int.h.stat <- aov_car(pressFreq ~ group*cue + Error (ID/cue), data = HAMBURG.index, anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(0.91), df = c(1), df_error = c(63))

# Bayes factors
int.h.BF <- anovaBF(pressFreq ~ group*cue  + ID, data = HAMBURG.index, 
                     whichRandom = "ID", iterations = 50000)
int.h.BF <- recompute(int.h.BF, iterations = 50000)
int.h.BF[4]/int.h.BF[3]

# ---------------------------- SYDNEY  ----------------------------------

#database
C.SYDNEY = subset(CHANGE, site == 'Sydney')
SYDNEY.index <- getChangeIndex(C.SYDNEY)# aggregate based on pre-post

# stat
int.s.stat <- aov_car(pressFreq ~ group*cue + Error (ID/cue), data = SYDNEY.index, anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(0.19), df = c(1), df_error = c(58))

# Bayes factors
int.s.BF <- anovaBF(pressFreq ~ group*cue  + ID, data = SYDNEY.index, 
                    whichRandom = "ID", iterations = 50000)
int.s.BF <- recompute(int.s.BF, iterations = 50000)
int.s.BF[4]/int.s.BF[3]# interaction with gorup


# ---------------------------- TEL-AVIV  ----------------------------------

# get database
C.TELAVIV = subset(CHANGE, site == 'Tel_Aviv')
TELAVIV.index <- getChangeIndex(C.TELAVIV)# aggregate based on pre-post

# stat
int.t.stat <- aov_car(pressFreq ~ group*cue + Error (ID/cue), data = TELAVIV.index, anova_table = list(correction = "GG", es = "pes"))

# effect sizes (90%CI)
F_to_eta2(f = c(0.42), df = c(1), df_error = c(58))

# Bayes factors
int.t.BF <- anovaBF(pressFreq ~ group*cue  + ID, data = TELAVIV.index, 
                    whichRandom = "ID", iterations = 50000)
int.t.BF <- recompute(int.t.BF, iterations = 50000)
int.t.BF[4]/int.t.BF[3]




#---------------------------------------------------------------------------
#                  OUTCOME DEVALUATION CHANGES META-ANALYSIS
#---------------------------------------------------------------------------

#---------------------------- OPEN AND FORMATOTHER DATABASES  -------------------
TRICOMI   <- read.delim(file.path(home_path,'DATA','Tricomi_2009.txt'), header = T, sep ='') # read in dataset
DEWIT_full<- read.delim(file.path(home_path,'DATA','deWit_2017.txt'), header = T, sep ='') # read in dataset
CECELI    <- read.delim(file.path(home_path,'DATA','Ceceli_inprep.txt'), header = T, sep ='') # read in dataset


#---------------------------- TRICOMI
# define factors in tricomi
TRICOMI$ID      <- factor(TRICOMI$ID)
TRICOMI$cue     <- factor(TRICOMI$cue)
TRICOMI$prepost <- factor(TRICOMI$prepost)
TRICOMI$group   <- factor(TRICOMI$group)
TRICOMI$cue     <- dplyr::recode(TRICOMI$cue, "val" = "Valued", "dev" = "Devalued" )
# normalize
TRICOMI$normPressFreq   <- scale(TRICOMI$PressFreq)


#---------------------------- CECELI
# get the last run of the last training session and all the runs after satiation
CECELI        <- subset(CECELI, cue == 'Valued' | cue == 'Devalued')
CECELI$normPressFreq   <- scale(CECELI$pressFreq)

CECELI.DAY1   <- subset(CECELI, group == '1-day')
CECELI.DAY3   <- subset(CECELI, group == '3-day')

CECELI.DAY1 <- ddply(CECELI.DAY1, .(ID), transform, averagePress  = mean(pressFreq[prepost=="pre"]))
CECELI.DAY3 <- ddply(CECELI.DAY3, .(ID), transform, averagePress  = mean(pressFreq[prepost=="pre"]))

CECELI.C.DAY1 <- subset(CECELI.DAY1, run == '2' | run == '3')
CECELI.DAY3   <- subset(CECELI.DAY3, session == '3') # we want the last day only
CECELI.C.DAY3 <- subset(CECELI.DAY3, run == '4' | run == '5')

CECELI.CHANGE <- rbind(CECELI.C.DAY1,CECELI.C.DAY3)

# get variable of interest
CECELI.CHANGE <- ddply(CECELI.CHANGE, .(ID), transform, normChangeBehav  = (mean(normPressFreq[prepost=="post" & cue=='Valued']) - mean(normPressFreq[prepost=="pre" & cue=='Valued'])) - (mean(normPressFreq[prepost=="post" & cue=='Devalued']) - mean(normPressFreq[prepost=="pre" & cue=='Devalued'])))
 
#average the data
CECELI <- CECELI.CHANGE

# define factors of interest in Ceceli
CECELI$ID        <- factor(CECELI$ID)
CECELI$cue       <- factor(CECELI$cue)
CECELI$prepost   <- factor(CECELI$prepost)
CECELI$group     <- factor(CECELI$group)


#----------------------------  DEWIT

# select variable of interest in dewit
myvars <- c("subj", "group", "endOfTrainingValued", "endOfTrainingDevalued", "extinctionValued", "extinctionDevalued")
DEWIT_wide <- DEWIT_full[myvars]

# convert into the long format
DEWIT <- gather(DEWIT_wide, prepostvaluedevalued, PressFreq, endOfTrainingValued:extinctionDevalued, factor_key=TRUE)
DEWIT$prepost <- dplyr::recode(DEWIT$prepostvaluedevalued, "endOfTrainingValued" = "pre", "endOfTrainingDevalued" = "pre","extinctionValued" = "post", "extinctionDevalued" = "post" )
DEWIT$cue     <- dplyr::recode(DEWIT$prepostvaluedevalued, "endOfTrainingValued" = "Valued", "extinctionValued" = "Valued","endOfTrainingDevalued" = "Devalued", "extinctionDevalued" = "Devalued" )
DEWIT$group   <- dplyr::recode(DEWIT$group, "1" = "1-day", "2" = "3-day" )
DEWIT$normPressFreq   <- scale(DEWIT$PressFreq)

DEWIT$subj    <- factor(DEWIT$subj)
DEWIT$cue     <- factor(DEWIT$cue, c("Devalued", "Valued"))
DEWIT$prepost <- factor(DEWIT$prepost)
DEWIT$group   <- factor(DEWIT$group)

# ---------------------- EFFECT SIZE AND FORST PLOT

# Get dataset formated for meta-analysis
CHANGE <- ddply(CHANGE, .(ID,group,cue,site), transform, metaBehav  = mean(normPressFreq[prepost=="post"]) - mean(normPressFreq[prepost=="pre"])) 
CHANGE.mean <- ddply(CHANGE, .(ID,group,cue,prepost,site), summarise, metaBehav= mean((metaBehav))) # we need the mean not to increase the N artifically
CHANGE.mean <- subset(CHANGE.mean, prepost == 'pre') # we need to subset not to have doubles

DEWIT <- ddply(DEWIT, .(subj,group,cue), transform, metaBehav  = normPressFreq[prepost=="post"] - normPressFreq[prepost=="pre"]) 
DEWIT.meta <- subset(DEWIT, prepost == 'pre')

TRICOMI <- ddply(TRICOMI, .(ID,group,cue), transform, metaBehav  = mean(normPressFreq[prepost=="post"]) - normPressFreq[prepost=="pre"]) 
TRICOMI.meta <- subset(TRICOMI, prepost == 'pre')

CECELI <- ddply(CECELI, .(ID,group,cue), transform, metaBehav  = mean(normPressFreq[prepost=="post"]) - mean(normPressFreq[prepost=="pre"])) 
CECELI.meta <- ddply(CECELI, .(ID,group,cue,prepost), summarise, metaBehav= mean((metaBehav))) # we need the mean not to increase the N artifically
CECELI.meta <- subset(CECELI.meta, prepost == 'pre') # we need to subset not to have doubles


# subset by site and crete the prepost index
C.CALTECH.day1 = subset(CHANGE.mean, site == 'Caltech1' &  group == "1-day")
C.CALTECH.day3 = subset(CHANGE.mean, site == 'Caltech1' &  group == "3-day")
C.CALTECH2.day1= subset(CHANGE.mean, site == 'Caltech2'&  group == "1-day")
C.CALTECH2.day3= subset(CHANGE.mean, site == 'Caltech2'&  group == "3-day")
C.HAMBURG.day1 = subset(CHANGE.mean, site == 'Hamburg'&  group == "1-day")
C.HAMBURG.day3 = subset(CHANGE.mean, site == 'Hamburg'&  group == "3-day")
C.SYDNEY.day1  = subset(CHANGE.mean, site == 'Sydney' &  group == "1-day")
C.SYDNEY.day3  = subset(CHANGE.mean, site == 'Sydney' &  group == "3-day")
C.TELAVIV.day1 = subset(CHANGE.mean, site == 'Tel_Aviv' &  group == "1-day")
C.TELAVIV.day3 = subset(CHANGE.mean, site == 'Tel_Aviv' &  group == "3-day")
TRICOMI.day1   = subset(TRICOMI.meta, group == "1-day")
TRICOMI.day3   = subset(TRICOMI.meta, group == "3-day")
DEWIT.day1     = subset(DEWIT.meta, group == "1-day")
DEWIT.day3     = subset(DEWIT.meta, group == "3-day")
CECELI.day1    = subset(CECELI.meta, group == "1-day")
CECELI.day3    = subset(CECELI.meta, group == "3-day")


# estimate mean standard deviation and correlation

# pasadena 2
estimate.caltech.day1 = summaryBy(metaBehav ~ cue, data = C.CALTECH.day1,
                                  FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.caltech.day1 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.CALTECH.day1)


estimate.caltech.day3 = summaryBy(metaBehav ~ cue, data = C.CALTECH.day3,
                                  FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.caltech.day3 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.CALTECH.day3)

# pasadena 2
estimate.caltech2.day1 = summaryBy(metaBehav ~ cue, data = C.CALTECH2.day1,
                                   FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.caltech2.day1 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.CALTECH2.day1)


estimate.caltech2.day3 = summaryBy(metaBehav ~ cue, data = C.CALTECH2.day3,
                                   FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.caltech2.day3 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.CALTECH2.day3)

# hamburg
estimate.hamburg.day1 = summaryBy(metaBehav ~ cue, data = C.HAMBURG.day1,
                                  FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.hamburg.day1 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.HAMBURG.day1)


estimate.hamburg.day3 = summaryBy(metaBehav ~ cue, data = C.HAMBURG.day3,
                                  FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.hamburg.day3 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.HAMBURG.day3)

# sydeny
estimate.sydney.day1  = summaryBy(metaBehav ~ cue, data = C.SYDNEY.day1,
                                  FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.sydney.day1 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.SYDNEY.day1)


estimate.sydney.day3  = summaryBy(metaBehav ~ cue, data = C.SYDNEY.day3,
                                  FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.sydney.day3 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.SYDNEY.day3)

# tel-aviv
estimate.telaviv.day1  = summaryBy(metaBehav ~ cue, data = C.TELAVIV.day1,
                                   FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.telaviv.day1 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.TELAVIV.day1)

estimate.telaviv.day3  = summaryBy(metaBehav ~ cue, data = C.TELAVIV.day3,
                                   FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.telaviv.day3 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = C.TELAVIV.day3)

# Tricomi
estimate.tricomi.day1  = summaryBy(metaBehav ~ cue, data = TRICOMI.day1,
                                   FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.tricomi.day1 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = TRICOMI.day1)

estimate.tricomi.day3  = summaryBy(metaBehav ~ cue, data = TRICOMI.day3,
                                   FUN = function(x) { c(m = mean(x), s = sd(x),n = length(x)) } )
corr.tricomi.day3 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = TRICOMI.day3)

# Dewit
estimate.dewit.day1  = summaryBy(metaBehav ~ cue, data = DEWIT.day1,
                                 FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.dewit.day1 = rmcorr(subj,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = DEWIT.day1)

estimate.dewit.day3  = summaryBy(metaBehav ~ cue, data = DEWIT.day3,
                                 FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.dewit.day3 = rmcorr(subj,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = DEWIT.day3)

# Ceceli
estimate.ceceli.day1  = summaryBy(metaBehav ~ cue, data = CECELI.day1,
                                 FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.ceceli.day1 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = CECELI.day1)

estimate.ceceli.day3  = summaryBy(metaBehav ~ cue, data = CECELI.day3,
                                 FUN = function(x) { c(m = mean(x), s = sd(x), n = length(x)) } )
corr.ceceli.day3 = rmcorr(ID,metaBehav[cue == 'Valued'],metaBehav[cue == 'Devalued'],dataset = CECELI.day3)

# build database for meta-analysis
site           = c ("ICHB: Pasadena1: 1-day "               , 
                    "ICHB: Pasadena1: 3-day "               , 
                    "ICHB: Hamburg: 1-day"                  ,
                    "ICHB: Hamburg: 3-day"                  ,
                    "ICHB: Pasadena2: 1-day"                ,
                    "ICHB: Pasadena2: 3-day"                ,
                    "ICHB: Sydeny: 1-day"                   ,
                    "ICHB: Sydeny: 3-day"                   ,
                    "ICHB: Tel-Aviv: 1-day"                 ,
                    "ICHB: Tel-Aviv: 3-day"                 ,
                    "De Wit (2018-jep:g): 1-day"             ,
                    "De Wit (2018-jep:g): 3-day"             ,
                    "Tricomi (2009-ejn): 1-day"              ,
                    "Tricomi (2009-ejn): 3-day"              ,
                    "Ceceli (in prep): 1-day"                ,
                    "Ceceli (in prep): 3-day")  

year           = c ("2017-sept"                                   ,
                    "2017-sept"                                   ,
                    "2018-jan"                                    ,
                    "2018-jan"                                    ,
                    "2018-may"                                    ,
                    "2018-may"                                    ,
                    "2018-may"                                    ,
                    "2018-may"                                    ,
                    "2018-june"                                   ,
                    "2018-june"                                   ,
                    "2018-jep:g"                                  ,
                    "2018-jep:g"                                  ,
                    "2009-ejn"                                    ,
                    "2009-ejn"                                    ,
                    "2017-???"                                    ,
                    "2017-???")

training       = c ("moderate-trainig"                           ,
                    "extensive-trainig"                          ,
                    "moderate-trainig"                           ,
                    "extensive-trainig"                          ,
                    "moderate-trainig"                           ,
                    "extensive-trainig"                          ,
                    "moderate-trainig"                           ,
                    "extensive-trainig"                          ,
                    "moderate-trainig"                           ,
                    "extensive-trainig"                          ,
                    "moderate-trainig"                           ,
                    "extensive-trainig"                          ,
                    "moderate-trainig"                           ,
                    "extensive-trainig"                          ,
                    "moderate-trainig"                           ,
                    "extensive-trainig")

mean_devalued   = c (estimate.caltech.day1$metaBehav.m[1]        , 
                     estimate.caltech.day3$metaBehav.m[1]          , 
                     estimate.hamburg.day1$metaBehav.m[1]          ,
                     estimate.hamburg.day3$metaBehav.m[1]          ,
                     estimate.caltech2.day1$metaBehav.m[1]         ,
                     estimate.caltech2.day3$metaBehav.m[1]         ,
                     estimate.sydney.day1$metaBehav.m[1]           , 
                     estimate.sydney.day3$metaBehav.m[1]           , 
                     estimate.telaviv.day1$metaBehav.m[1]          ,
                     estimate.telaviv.day3$metaBehav.m[1]          ,
                     estimate.dewit.day1$metaBehav.m[1]            ,
                     estimate.dewit.day3$metaBehav.m[1]            ,
                     estimate.tricomi.day1$metaBehav.m[1]          ,
                     estimate.tricomi.day3$metaBehav.m[1]          ,
                     estimate.ceceli.day1$metaBehav.m[1]           ,
                     estimate.ceceli.day3$metaBehav.m[1]) # mean difference prepost for devalued

mean_valued  = c (estimate.caltech.day1$metaBehav.m[2]         , 
                  estimate.caltech.day3$metaBehav.m[2]         , 
                  estimate.hamburg.day1$metaBehav.m[2]         ,
                  estimate.hamburg.day3$metaBehav.m[2]         ,
                  estimate.caltech2.day1$metaBehav.m[2]        ,
                  estimate.caltech2.day3$metaBehav.m[2]        ,
                  estimate.sydney.day1$metaBehav.m[2]          , 
                  estimate.sydney.day3$metaBehav.m[2]          , 
                  estimate.telaviv.day1$metaBehav.m[2]         ,
                  estimate.telaviv.day3$metaBehav.m[2]         ,
                  estimate.dewit.day1$metaBehav.m[2]            ,
                  estimate.dewit.day3$metaBehav.m[2]            ,
                  estimate.tricomi.day1$metaBehav.m[2]          ,
                  estimate.tricomi.day3$metaBehav.m[2]         ,
                  estimate.ceceli.day1$metaBehav.m[2]          ,
                  estimate.ceceli.day3$metaBehav.m[2]) # mean difference prepost for valued) # mean difference prepost for valued

std_devalued  = c (estimate.caltech.day1$metaBehav.s[1]       , 
                   estimate.caltech.day3$metaBehav.s[1]         , 
                   estimate.hamburg.day1$metaBehav.s[1]         ,
                   estimate.hamburg.day3$metaBehav.s[1]         ,
                   estimate.caltech2.day1$metaBehav.s[1]        ,
                   estimate.caltech2.day3$metaBehav.s[1]        ,
                   estimate.sydney.day1$metaBehav.s[1]          , 
                   estimate.sydney.day3$metaBehav.s[1]          , 
                   estimate.telaviv.day1$metaBehav.s[1]         ,
                   estimate.telaviv.day3$metaBehav.s[1]         ,
                   estimate.dewit.day1$metaBehav.s[1]           ,
                   estimate.dewit.day3$metaBehav.s[1]           ,
                   estimate.tricomi.day1$metaBehav.s[1]         ,
                   estimate.tricomi.day3$metaBehav.s[1]         ,
                   estimate.ceceli.day1$metaBehav.s[1]         ,
                   estimate.ceceli.day3$metaBehav.s[1]) # standard deviation  of the difference prepost for devalued)

std_valued  = c (estimate.caltech.day1$metaBehav.s[2]          , 
                 estimate.caltech.day3$metaBehav.s[2]         , 
                 estimate.hamburg.day1$metaBehav.s[2]         ,
                 estimate.hamburg.day3$metaBehav.s[2]         ,
                 estimate.caltech2.day1$metaBehav.s[2]        ,
                 estimate.caltech2.day3$metaBehav.s[2]        ,
                 estimate.sydney.day1$metaBehav.s[2]          , 
                 estimate.sydney.day3$metaBehav.s[2]          , 
                 estimate.telaviv.day1$metaBehav.s[2]         ,
                 estimate.telaviv.day3$metaBehav.s[2]         ,
                 estimate.dewit.day1$metaBehav.s[2]           ,
                 estimate.dewit.day3$metaBehav.s[2]           ,
                 estimate.tricomi.day1$metaBehav.s[2]         ,
                 estimate.tricomi.day3$metaBehav.s[2]         ,
                 estimate.ceceli.day1$metaBehav.s[2]         ,
                 estimate.ceceli.day3$metaBehav.s[2]) # standard deviation  of the difference prepost for valued)


n_devalued  = c (estimate.caltech.day1$metaBehav.n[1]           , 
                 estimate.caltech.day3$metaBehav.n[1]         , 
                 estimate.hamburg.day1$metaBehav.n[1]         ,
                 estimate.hamburg.day3$metaBehav.n[1]         ,
                 estimate.caltech2.day1$metaBehav.n[1]        ,
                 estimate.caltech2.day3$metaBehav.n[1]        ,
                 estimate.sydney.day1$metaBehav.n[1]          , 
                 estimate.sydney.day3$metaBehav.n[1]          , 
                 estimate.telaviv.day1$metaBehav.n[1]         ,
                 estimate.telaviv.day3$metaBehav.n[1]         ,
                 estimate.dewit.day1$metaBehav.n[1]            ,
                 estimate.dewit.day3$metaBehav.n[1]            ,
                 estimate.tricomi.day1$metaBehav.n[1]          ,
                 estimate.tricomi.day3$metaBehav.n[1]          ,
                 estimate.ceceli.day1$metaBehav.n[1]          ,
                 estimate.ceceli.day3$metaBehav.n[1]) # n participants  of the difference prepost for devalued)

n_valued  = c (estimate.caltech.day1$metaBehav.n[2]           , 
               estimate.caltech.day3$metaBehav.n[2]         , 
               estimate.hamburg.day1$metaBehav.n[2]         ,
               estimate.hamburg.day3$metaBehav.n[2]         ,
               estimate.caltech2.day1$metaBehav.n[2]        ,
               estimate.caltech2.day3$metaBehav.n[2]        ,
               estimate.sydney.day1$metaBehav.n[2]          , 
               estimate.sydney.day3$metaBehav.n[2]          , 
               estimate.telaviv.day1$metaBehav.n[2]         ,
               estimate.telaviv.day3$metaBehav.n[2]         ,
               estimate.dewit.day1$metaBehav.n[2]           ,
               estimate.dewit.day3$metaBehav.n[2]           ,
               estimate.tricomi.day1$metaBehav.n[2]         ,
               estimate.tricomi.day3$metaBehav.n[2]         ,
               estimate.ceceli.day1$metaBehav.n[2]         ,
               estimate.ceceli.day3$metaBehav.n[2]) # n participants of the difference prepost for valued)

ri  = c (corr.caltech.day1$r         , 
         corr.caltech.day3$r         , 
         corr.hamburg.day1$r         ,
         corr.hamburg.day3$r         ,
         corr.caltech2.day1$r        ,
         corr.caltech2.day3$r        ,
         corr.sydney.day1$r          , 
         corr.sydney.day3$r          , 
         corr.telaviv.day1$r         ,
         corr.telaviv.day3$r         ,
         corr.dewit.day1$r           ,
         corr.dewit.day3$r           ,
         corr.tricomi.day1$r         ,
         corr.tricomi.day3$r         ,
         corr.ceceli.day1$r         ,
         corr.ceceli.day3$r) # correlation


#---------------------------- RE META AND FOREST PLOT  -------------------

metadata = data.frame( site, year, training, mean_valued, mean_devalued, std_valued, std_devalued, n_valued, n_devalued,ri)

meta.data <- escalc(measure="MC", m1i=mean_valued, m2i=mean_devalued, sd1i=std_valued,sd2i=std_devalued, ni=n_valued,
                    ri=ri,  data=metadata)



#--------------------------- fit random-effect model for the moderator analysis
res <- rma.mv(yi, vi, mods = ~ training, random = ~ 1 | site, data=meta.data) # modulator analysis

res.all <- rma.mv(yi, vi, random = ~ 1 | site, data=meta.data) # for forest plot


### fit random-effects model in the three subgroups
res.day1 <- rma.mv(yi, vi, subset=(training=="moderate-trainig"), data=meta.data)
res.day3 <- rma.mv(yi, vi, subset=(training=="extensive-trainig"), data=meta.data)


# --------------------------------------- plot

par(mar=c(4,4,1,2)) # decrease margins so the full space is used
par(cex=0.8, font = 2)### switch to bold font


forest.plot <- forest(res.all,slab = (meta.data$site),xlim=c(-1.2,2),
                     ilab = cbind(meta.data$n_valued),
                     ilab.xpos=c(-0.3), cex=1,ylim=c(1.2,28),
                     order=order(meta.data$training),rows=c(4:11
                                                          ,17:24),
                     xlab="Mean Change", mlab="", psize=1)

### add summary polygons for the three subgroups
addpoly(res.day1, row=15.5, cex=0.75, mlab="")
addpoly(res.day3, row= 2.5, cex=0.75,  mlab="")

text(-1.2, 15.5, pos=4, cex=0.75, font=4, bquote(paste("RE Model for Moderate training")))
text(-1.2, 2.5, pos=4, cex=0.75, font=4, bquote(paste("RE Model for Extensive training")))
                                            
### add text for the subgroups
par(cex=0.8, font=4)
text(-1.2, c(30,12,25), pos=4, c("Moderate training", 
                             "Extensive training"))
# add column headings to the plot
par(cex=0.8, font=4)### switch to bold font
text(-1, 26.5, "STUDY",  pos=4)
text( 2, 26.5, "MC [95% CI]", pos=2)
par(cex=1, font=3)### switch to bold font
text(-0.3, 26.5, c("N"))

dev.print(pdf, file.path(figures_path,'Figure_forest.pdf'))
dev.off()







#---------------------------------------------------------------------------
#                  OUTCOME DEVALUATION CHANGES DISTRUBUTIONS ALL
#---------------------------------------------------------------------------

# --------------------------- FORMAT DATABASE ------------------------------

# first we need the aggregated data
CHANGE.means <- aggregate(CHANGE$normChangeBehav, by = list(CHANGE$ID, CHANGE$group, CHANGE$site), FUN='mean') # extract means
colnames(CHANGE.means) <- c('ID','group','site', 'normChangeBehav')


#--------------------------- FLEXMIX TO IDENTIFY CLUSTERS -----------------

#  what is the number of clusters that better explains the data
n_clusters <- stepFlexmix(normChangeBehav ~ group, data = CHANGE.means, control = list(verbose = 0), k = 1:5, nrep = 200)
getModel(n_clusters, "BIC")

# get cluster size
getModel(n_clusters, which = 1)
getModel(n_clusters, which = 2)
getModel(n_clusters, which = 3)
getModel(n_clusters, which = 4)
getModel(n_clusters, which = 5)

# the we do the analysis specifying the number of cluster we found with step flex
mixlm <- flexmix(normChangeBehav ~ group, data = CHANGE.means, k = 2)

print(table(clusters(mixlm), CHANGE.means$group))
CHANGE.means$Cluster = factor(clusters(mixlm)) # create a variable based on the clustering


#--------------------------- PLOT HISTOGRAM AND CLUSTERS -----------------

# rename variables for plot
CHANGE.means$group     <- dplyr::recode(CHANGE.means$group, "1-day" = "Moderate training", "3-day" = "Extensive training" )
CHANGE.means$Cluster   <- dplyr::recode(CHANGE.means$Cluster, "2" = "Outcome-insensitive", "1" = "Outcome-sensitive" )


pp <-  ggplot(CHANGE.means, aes(normChangeBehav, fill = Cluster)) +
  geom_histogram(aes(y=..density..),alpha=0.2,binwidth=0.2)+
  geom_density(alpha = 0.5)+
  xlab('Behavioral adaptation index')+
  ylab('Density')+
  facet_grid(~group)+
  scale_fill_manual(values=c("#F5793A", "#C9C9DB")) +
  theme_bw()

ppp <-  pp + theme_bw(base_size = 17, base_family = "Helvetica")+
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        strip.background = element_rect(color="white", fill="white", linetype="solid"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22))

pdf(file.path(figures_path,'Figure_histograms_clusters.pdf'))
print(ppp)
dev.off()









#---------------------------------------------------------------------------
#                       INDIVIDUAL DIFFERENCES 
#---------------------------------------------------------------------------




#------------------------ DATA REDUCTION  ----------------------------------

# prepare database for the FA
Q_EFA.means.ID <- aggregate(ANXIETY ~ ID * TICS_SOOV * TICS_PREPE * TICS_WODI * TICS_EXWO * TICS_LACK * TICS_SOTE * TICS_SOIS * TICS_WORY * TICS_WOOV * BIS_motor * BIS_attentional * BIS_nonplanning,
                            data = CHANGE, FUN = mean, na.action = na.pass) # we do not include the total scales
Q_EFA.means <- Q_EFA.means.ID
Q_EFA.means$ID <- NULL

# quick look at the covarivance structure
r.subscale = cor(Q_EFA.means, use = "pairwise.complete.obs")
cor.plot(Q_EFA.means,numbers=TRUE,main="correlation matrix")
names(Q_EFA.means)[names(Q_EFA.means) == 'V1'] <- 'STAI'

# check distributions before proceeding with FA
describe (Q_EFA.means)
pairs.panels(na.omit(Q_EFA.means))

# determine the number of factors
nFactor  <- fa.parallel(Q_EFA.means, fm = "ml")


# apply EFA with oblimin
quest.1.efa <- fa(r = Q_EFA.means, nfactors = 4, rotate = "oblimin", fm = "ml")

print(quest.1.efa$loadings,cutoff = 0.0)

# create figure with EFA solution
fa.diagram(quest.1.efa)

# save the plot in the figures folder
dev.print(pdf, file.path(figures_path,'Figure_EFA_oblimin.pdf'))
dev.off()

# calculate the factors loadings
s = factor.scores (Q_EFA.means, quest.1.efa) # 


#------------------------ USE FACTOR AS AS MODERATOR IN THE MAIN ANALYSIS ----------

# merge with the FULL database
axes <- s$scores

# combine it with the participants ID
dat <- cbind(Q_EFA.means.ID, axes)
EFA_CHANGE <- join (CHANGE,dat, type = "full")

# run full model for each factor individually

# stress work
inter.work = lmer(normPressFreq~ group*cue*prepost*ML1 + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE, control = lmerControl(optimizer ="bobyqa"))
summary(inter.work)
Confint(inter.work, level = 0.95) 

# ----- assumptions check
plot(fitted(inter.work),residuals(inter.work)) 
qqnorm(residuals(inter.work))
hist(residuals(inter.work))

# stress social
inter.social = lmer(normPressFreq~ group*cue*prepost*ML2 + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE, control = lmerControl(optimizer ="bobyqa"))
summary(inter.social)
Confint(inter.social, level = 0.95) 

# ----- assumptions check
plot(fitted(inter.social),residuals(inter.social)) 
qqnorm(residuals(inter.social))
hist(residuals(inter.social))

# stress affect
inter.affect = lmer(normPressFreq~ group*cue*prepost*ML3 + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE, control = lmerControl(optimizer ="bobyqa"))
summary(inter.affect)
Confint(inter.affect, level = 0.95) 

# ----- assumptions check
plot(fitted(inter.affect),residuals(inter.affect)) 
qqnorm(residuals(inter.affect))
hist(residuals(inter.affect))

# implusivity
inter.implusivity = lmer(normPressFreq~ group*cue*prepost*ML4 + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(inter.implusivity)
Confint(inter.implusivity, level = 0.95) 

# ----- assumptions check
plot(fitted(inter.implusivity),residuals(inter.implusivity)) 
qqnorm(residuals(inter.implusivity))
hist(residuals(inter.implusivity))

# test and different points of the model to understand interaction

# Stress affective -1 SD people low on affectiv stress have effect of overtraining
EFA_CHANGE$AFF_pSD <- scale(EFA_CHANGE$ML3, scale = T) + 1 # here I'm going to test at - 1SD (so people that are low in anxiety)
sslop.pSD = lmer(normPressFreq~ group*cue*prepost*AFF_pSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(sslop.pSD)
Confint(sslop.pSD, level = 0.95) 

# Stress Affective +1 SD people high on affective stress have effect of overtraining
EFA_CHANGE$AFF_mSD <- scale(EFA_CHANGE$ML3, scale = T) - 1 # here I'm going to test at + 1SD (so people that are high in anxiety)
sslop.mSD = lmer(normPressFreq ~ group*cue*prepost*AFF_mSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE, control = lmerControl(optimizer ="bobyqa"))
summary(sslop.mSD)
Confint(sslop.mSD, level = 0.95) 

#----------------------------- PLOT EXPLORATORY MODEL ---------------------------

# this tests the model predictions as we do in lmer but does not allow to display distributions
AFF.means <- aggregate(EFA_CHANGE$normChangeBehav, by = list(EFA_CHANGE$ID, EFA_CHANGE$group, EFA_CHANGE$site, EFA_CHANGE$AFF_pSD, EFA_CHANGE$AFF_mSD, EFA_CHANGE$ML3), FUN='mean', na.rm = T) # extract means
colnames(AFF.means) <- c('ID','group','site', 'AFF_pSD', 'AFF_mSD','AFF', 'normChangeBehav')


# figure for AFF: Streess Affect
AFF.means$StressAffect<- ntile(AFF.means$AFF, 2)
AFF.means$StressAffect<- factor(AFF.means$StressAffect)

# low stress affect
lowAff.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(AFF.means, StressAffect == '1'),
                          factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(3.98), df = c(1), df_error = c(95))


# Bayes factors 
lowAff.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(AFF.means, StressAffect  == '1'), 
                     whichRandom = "ID", iterations = 50000)
lowAff.BF <- recompute(lowAnx.BF, iterations = 50000)
lowAff.BF[1]

# high stress affect
highAff.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(AFF.means, StressAffect == '2'),
                           factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(0.35), df = c(1), df_error = c(94))

# Bayes factors
highAff.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(AFF.means,  StressAffect == '2'), 
                      whichRandom = "ID", iterations = 50000)
highAff.BF <- recompute(highAff.BF, iterations = 50000)
highAff.BF[1]

# rename variables for plot
AFF.means$StressAffect    <- dplyr::recode(AFF.means$StressAffect, "1" = "Lower Stress Affect", "2" = "Higher Stress Affect" )
AFF.means$group           <- dplyr::recode(AFF.means$group, "1-day" = "Moderate", "3-day" = "Extensive" )


pp <- ggplot(AFF.means, aes(x = group, y = normChangeBehav, fill = group, color = group)) +
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .1, aes(x = group, y = normChangeBehav, fill = factor(group, levels = c("Moderate","Extensive" ))), color = NA)+
  geom_point(alpha = .3, position = position_jitterdodge(jitter.width = .5, jitter.height = 0)) +
  stat_summary(fun.data = mean_se, geom = "crossbar",width = 0.85 , alpha = 0.1) +
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

pdf(file.path(figures_path,'Figure_IndividualDifferences.pdf'))
print(ppp)
dev.off()






#---------------------------------------------------------------------------
#  SUPPLEMENTARY STARTEGY 1: INDIVIDUAL DIFFERENCES APPROACH 
#---------------------------------------------------------------------------

# non collinar factor for ancova-like approach

#------------------------ DATA REDUCTION TO EXTRACT ORTHOGONAL FACTORS ------

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


#------------------------ USE FACTOR AS AS MODERATOR IN THE MAIN ANALYSIS ----------

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
EFA_CHANGE$AFF_pSD <- scale(EFA_CHANGE$ML3, scale = T) + 1 # here I'm going to test at - 1SD (so people that are low in anxiety)
sslop.pSD = lmer(normPressFreq~ group*cue*prepost*AFF_pSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(sslop.pSD)
Confint(sslop.pSD, level = 0.95) 

# Social Isolation +1 SD people high in axiety/stress have effect of overtraining
EFA_CHANGE$AFF_mSD <- scale(EFA_CHANGE$ML3, scale = T) - 1 # here I'm going to test at + 1SD (so people that are high in anxiety)
sslop.mSD = lmer(normPressFreq ~ group*cue*prepost*AFF_mSD + itemxcondition + site + (1+cue*prepost+itemxcondition|ID), data = EFA_CHANGE, REML=FALSE,control = lmerControl(optimizer ="bobyqa"))
summary(sslop.mSD)
Confint(sslop.mSD, level = 0.95) 

#----------------------------- PLOT EXPLORATORY MODEL ---------------------------

# this tests the model predictions as we do in lmer but does not allow to display distributions
AFF.means <- aggregate(EFA_CHANGE$normChangeBehav, by = list(EFA_CHANGE$ID, EFA_CHANGE$group, EFA_CHANGE$site, EFA_CHANGE$AFF_pSD, EFA_CHANGE$AFF_mSD, EFA_CHANGE$ML3), FUN='mean', na.rm = T) # extract means
colnames(AFF.means) <- c('ID','group','site', 'AFF_pSD', 'AFF_mSD','AFF', 'normChangeBehav')

# figure for AFF: Streess Affect
AFF.means$StressAffect<- ntile(AFF.means$AFF, 2)
AFF.means$StressAffect<- factor(AFF.means$StressAffect)

# low stress affect
lowAff.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(AFF.means, StressAffect == '1'),
                           factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(4.84), df = c(1), df_error = c(95))


# Bayes factors 
lowAnx.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(AFF.means, StressAffect  == '1'), 
                     whichRandom = "ID", iterations = 50000)
lowAnx.BF <- recompute(lowAnx.BF, iterations = 50000)
lowAnx.BF[1]

# high anxiety
highAnx.stat    <- aov_car(normChangeBehav  ~ group + site + Error(ID), data = subset(AFF.means, StressAffect == '2'),
                            factorize = F, anova_table = list(correction = "GG",es = "pes"))
# effect sizes (90%CI)
F_to_eta2(f = c(0.34), df = c(1), df_error = c(94))


# Bayes factors
highAnx.BF <- anovaBF(normChangeBehav  ~ group + site, data = subset(AFF.means,  StressAffect == '2'), 
                      whichRandom = "ID", iterations = 50000)
highAnx.BF <- recompute(highAnx.BF, iterations = 50000)
highAnx.BF[1]

# rename variables for plot
AFF.means$StressAffect    <- dplyr::recode(AFF.means$StressAffect, "1" = "Lower Stress Affect", "2" = "Higher Stress Affect" )
AFF.means$group           <- dplyr::recode(AFF.means$group, "1-day" = "Moderate", "3-day" = "Extensive" )


pp <- ggplot(AFF.means, aes(x = group, y = normChangeBehav, fill = group, color = group)) +
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .1, aes(x = group, y = normChangeBehav, fill = factor(group, levels = c("Moderate","Extensive" ))), color = NA)+
  geom_point(alpha = .3, position = position_jitterdodge(jitter.width = .5, jitter.height = 0)) +
  stat_summary(fun.data = mean_se, geom = "crossbar",width = 0.85 , alpha = 0.1) +
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

pdf(file.path(figures_path,'Figure_IndividualDifferences_S1.pdf'))
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



# -------------------- PLOTS ----------------------------------------



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
  geom_flat_violin(scale = "count", trim = FALSE, alpha = .1, aes(x = group, y = normChangeBehav, fill = factor(group, levels = c("Moderate","Extensive" ))), color = NA)+
  geom_point(alpha = .3, position = position_jitterdodge(jitter.width = .5, jitter.height = 0)) +
  stat_summary(fun.data = mean_se, geom = "crossbar",width = 0.85 , alpha = 0.1) +
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
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

pdf(file.path(figures_path,'Figure_S2_Anxiety_Worries.pdf'),width=7,height=8)
print(ppp)
dev.off()











