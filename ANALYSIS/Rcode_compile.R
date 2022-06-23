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
# Started  by E.R.P on NOVEMBER 2018                                                               #
# Verified by R.G on DECEMBER 2018                                                                 #
# Modified by  E.R.P on JUNE 2022                                                                 #
####################################################################################################


#####################################################################################################
# ----------------------------------- PRELIMIMINARY STUFF ------------------------------------------


# load libraries
library (dplyr)
library (plyr)
library (pastecs)
library (reshape)
library (reshape2)

# Set path
full_path       <- dirname(rstudioapi::getActiveDocumentContext()$path)
pos             <- regexpr("MULTILAB_HABIT", full_path)
home_path       <- substr(full_path, 1, pos+13)
utilities_path  <- file.path(home_path,'ANALYSIS','R')
setwd (home_path)

source (file.path(utilities_path, 'normalizeVariablesBehavior.R'))
source (file.path(utilities_path, 'normalizeVariablesQuestionnaires.R'))

#####################################################################################################
# ----------------------------------------- QUESTIONNAIRES ------------------------------------------

# get questionnaiores databases
Q.CALTECH1 <- read.delim(file.path(home_path,'DATA','CALTECH_V1_QUESTIONNARIES.txt'), header = T, sep ='') # read in dataset
Q.CALTECH1 <- normalizeVariablesQuestionnaires(Q.CALTECH1) # we need to normalize for each center individually
Q.HAMBURG  <- read.delim(file.path(home_path,'DATA','HAMBURG_QUESTIONNARIES.txt'), header = T, sep ='')    # read in dataset
Q.HAMBURG  <- normalizeVariablesQuestionnaires(Q.HAMBURG) # we need to normalize for each center individually
Q.TELAVIV  <- read.delim(file.path(home_path,'DATA','TELAVIV_QUESTIONNARIES.txt'), header = T, sep ='')    # read in dataset
Q.TELAVIV  <- normalizeVariablesQuestionnaires(Q.TELAVIV) # we need to normalize for each center individually
Q.CALTECH2 <- read.delim(file.path(home_path,'DATA','CALTECH_V2_QUESTIONNARIES.txt'), header = T, sep ='') # read in dataset
Q.CALTECH2 <- normalizeVariablesQuestionnaires(Q.CALTECH2) # we need to normalize for each center individually

tmp1 = join (Q.CALTECH1, Q.HAMBURG, type = "full")
tmp2 = join (tmp1, Q.TELAVIV, type = "full")
QUESTIONNAIRES = join (tmp2, Q.CALTECH2, type = "full")
# remove participant that have more than one missing data in the questionnaire of interest
QUESTIONNAIRES <- subset (QUESTIONNAIRES, !ID == '330' & !ID == '200' & !ID == '168' & !ID == '222')


#####################################################################################################
# ----------------------------------------- FREE OPERANT TASK ---------------------------------------

P.CALTECH1 <- read.delim(file.path(home_path,'DATA','CALTECH_V1.txt'), header = T, sep ='') # read in dataset
P.CALTECH1 <- normalizeVariablesBehavior(P.CALTECH1) # we need to normalize for each center individually
P.HAMBURG  <- read.delim(file.path(home_path,'DATA','HAMBURG.txt'), header = T, sep ='') # read in dataset
P.HAMBURG  <- normalizeVariablesBehavior(P.HAMBURG) # we need to normalize for each center individually
P.TELAVIV  <- read.delim(file.path(home_path,'DATA','TELAVIV.txt'), header = T, sep ='') # read in dataset
P.TELAVIV  <- normalizeVariablesBehavior(P.TELAVIV) # we need to normalize for each center individually
P.CALTECH2 <- read.delim(file.path(home_path,'DATA','CALTECH_V2.txt'), header = T, sep ='') # read in dataset
P.CALTECH2 <- normalizeVariablesBehavior(P.CALTECH2) # we need to normalize for each center individually
P.SYDNEY   <- read.delim(file.path(home_path,'DATA','SYDNEY.txt'), header = T, sep ='') # read in dataset
P.SYDNEY   <- normalizeVariablesBehavior(P.SYDNEY ) # we need to normalize for each center individually

tmp1 = join (P.CALTECH1, P.HAMBURG, type = "full")
tmp2 = join (tmp1, P.CALTECH2, type = "full")
tmp3 = join (tmp2, P.TELAVIV, type = 'full')
FREEOPERANT =  join (tmp3, P.SYDNEY, type = "full")
# remove participant based on pre-reg criteria
FREEOPERANT <- subset (FREEOPERANT,!ID == '234') # caltech 2 extream
FREEOPERANT <- subset (FREEOPERANT,!ID == '299'  & !ID == '334' & !ID == '341' & !ID == '310' & !ID == '304' & !ID == '322' & !ID == '326' & !ID == '352' & !ID == '356' & !ID == '360' & !ID == '301') # automated exclusions in Telaviv

# merge
FULL   <- join (QUESTIONNAIRES, FREEOPERANT, type = "full")

# print database
write.table(FULL,file.path(home_path,'DATA','FULL_DATABASE.txt'),sep="\t",row.names=FALSE)


#------------------------------------------------------------ FOR REVISIONS GET THE NON_NORMALIZED DATABASE OF THE QUESTIONNAIRES  ----------------------------------------


# get questionnaiores databases
Q.CALTECH1 <- read.delim(file.path(home_path,'DATA','CALTECH_V1_QUESTIONNARIES.txt'), header = T, sep ='') # read in dataset
Q.HAMBURG  <- read.delim(file.path(home_path,'DATA','HAMBURG_QUESTIONNARIES.txt'), header = T, sep ='')    # read in dataset
Q.TELAVIV  <- read.delim(file.path(home_path,'DATA','TELAVIV_QUESTIONNARIES.txt'), header = T, sep ='')    # read in dataset
Q.CALTECH2 <- read.delim(file.path(home_path,'DATA','CALTECH_V2_QUESTIONNARIES.txt'), header = T, sep ='') # read in dataset

tmp1 = join (Q.CALTECH1, Q.HAMBURG, type = "full")
tmp2 = join (tmp1, Q.TELAVIV, type = "full")
QUESTIONNAIRES_NS = join (tmp2, Q.CALTECH2, type = "full")
# remove participant that have more than one missing data in the questionnaire of interest
QUESTIONNAIRES_NS <- subset (QUESTIONNAIRES_NS, !ID == '330' & !ID == '200' & !ID == '168' & !ID == '222')
# remove participant based on pre-reg criteria
QUESTIONNAIRES_NS <- subset (QUESTIONNAIRES_NS,!ID == '234') # caltech 2 extream
QUESTIONNAIRES_NS <- subset (QUESTIONNAIRES_NS,!ID == '299'  & !ID == '334' & !ID == '341' & !ID == '310' & !ID == '304' & !ID == '322' & !ID == '326' & !ID == '352' & !ID == '356' & !ID == '360' & !ID == '301') # automated exclusions in Telaviv


# print the databse
write.table(QUESTIONNAIRES_NS,file.path(home_path,'DATA','FULL_QUESTIONNAIRES.txt'),sep="\t",row.names=FALSE)


#####################################################################################################
# ----------------------------------------- DETAILS DEVALUATION PROCEDIRE---------------------------------------

D.CALTECH1 <- read.delim(file.path(home_path,'DATA','CALTECH_V1_DEVALUATION_DETAILS.txt'), header = T, sep ='') # read in dataset
D.HAMBURG  <- read.delim(file.path(home_path,'DATA','HAMBURG_DEVALUATION_DETAILS.txt'), header = T, sep ='') # read in dataset
D.TELAVIV  <- read.delim(file.path(home_path,'DATA','TELAVIV_DEVALUATION_DETAILS.txt'), header = T, sep ='') # read in dataset
D.CALTECH2 <- read.delim(file.path(home_path,'DATA','CALTECH_V2_DEVALUATION_DETAILS.txt'), header = T, sep ='') # read in dataset
D.SYDNEY   <- read.delim(file.path(home_path,'DATA','SYDNEY_DEVALUATION_DETAILS.txt'), header = T, sep ='') # read in dataset

tmp1 = join (D.CALTECH1, D.HAMBURG, type = "full")
tmp2 = join (tmp1, D.CALTECH2, type = "full")
tmp3 = join (tmp2, D.TELAVIV, type = 'full')
DEVALUATIONDETAILS =  join (tmp3, D.SYDNEY, type = "full")
# remove participant based on pre-reg criteria
DEVALUATIONDETAILS  <- subset (DEVALUATIONDETAILS ,!ID == '234') # caltech 2 extream
DEVALUATIONDETAILS  <- subset (DEVALUATIONDETAILS ,!ID == '299'  & !ID == '334' & !ID == '341' & !ID == '310' & !ID == '304' & !ID == '322' & !ID == '326' & !ID == '352' & !ID == '356' & !ID == '360' & !ID == '301') # automated exclusions in Telaviv


# print database
write.table(FULL,file.path(home_path,'DATA','DEVALUATIONDETAILS_DATABASE.txt'),sep="\t",row.names=FALSE)


