#4/27 FI Model Accuracy
#Aishat Sadiq

setwd("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/30 LHL - PARA/31 Ongoing Projects/Food Insecurity")

#Libraries
library(tidyverse) #pretty df
library(psych)
library(psychTools)
library(dplyr) #pretty df
library(ggplot2) #graphs
library(Gmisc) #summary stats
library(magrittr)
library(gtsummary)
library(scales)
library(readr)
library(polycor)
library(interactions)
library(sandwich)

#Full Dataframe #620ptps
Descriptive_Library <- read_csv("31.02 Food Insecurity/final_scores_2021_3-22.csv")
View(Descriptive_Library)
colnames(Descriptive_Library)

#n=620
Full_Set <- select(Descriptive_Library, "BART_adj_total_pumps", "BART_points", "BART_balloons_popped","BIS_score", "BAS_score", "reward_sensitivity", "punishment_sensitivity", "AK_insight_plan_control", "Radimer_home_food_insecure", "Radimer_cornell_food_insecure", "FIS_food_insecurity_score", "FIS_food_insecurity_status", "Demographics_Age", "Demographics_Race", "Demographics_Year", "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", "Demographics_Live", "life_history_past", "life_history_present") %>%
  na.omit
describe(Full_Set)
View(Full_Set)

#FIS >= 2 Subset Dataframe #81ptps
FI <- subset(Full_Set, Full_Set$FIS_food_insecurity_score >= 2)
View(FI)
describe(FI)

#Demographics Dataframe
Demographics <- select(Full_Set, "Demographics_Age", "Demographics_Race", "Demographics_Year", "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", "Demographics_Live", "life_history_past", "life_history_present")
View(Demographics)
describe(Demographics)

FIS_food_insecurity_score <- as.factor(Full_Set$FIS_food_insecurity_score)
levels(FIS_food_insecurity_score) <- c("['Food Secure']",
                                       "['Food Secure']",
                                       "['Food Secure, at risk']",
                                       "['Food Insecure']",
                                       "['Food Insecure']",
                                       "['Food Insecure']",
                                       "['Food Insecure']")

Radimer_home_food_insecure <- as.factor(Full_Set$Radimer_home_food_insecure)                                       
levels(Radimer_home_food_insecure) <- c("['Food Secure']",
                                        "['Food Secure']",
                                        "['Food Secure']",
                                        "['Food Secure']",
                                        "['Food Insecure']",
                                        "['Food Insecure']",
                                        "['Food Insecure']",
                                        "['Food Insecure']")

Radimer_cornell_food_insecure <- as.factor(Full_Set$Radimer_cornell_food_insecure)
levels(Radimer_cornell_food_insecure) <- c("['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Insecure']",
                                           "['Food Insecure']",
                                           "['Food Insecure']")

ItemLevel<- read_csv("31.02 Food Insecurity/itemlevel_2021_cleaned (2).csv")
View(ItemLevel)
colnames(ItemLevel)

mean(Full_Set$BART_adj_total_pumps)
var(Full_Set$BART_adj_total_pumps)

mean(Full_Set$BART_points)
var(Full_Set$BART_points)

mean(Full_Set$BART_balloons_popped)
var(Full_Set$BART_balloons_popped)

mean(Full_Set$BIS_score)
var(Full_Set$BIS_score)

mean(Full_Set$BAS_score) 
var(Full_Set$BAS_score) 

mean(Full_Set$reward_sensitivity) 
var(Full_Set$reward_sensitivity)

mean(Full_Set$punishment_sensitivity)
var(Full_Set$punishment_sensitivity)


"Radimer_HomeEatSameThing"                      
"Radimer_HomeFoodDidntLast"                    
"Radimer_HomeFoodForMeal"                       

"Radimer_CornellCantAffordFood"                
"Radimer_CornellOftenHungry"                    
"Radimer_CornellEatLess"                       

"FIS_FoodDidntLast"                                                         
"FIS_ReduceMealSize"                            
"FIS_ReduceHowOften"                           
"FIS_EatLess"
"FIS_Hungry" 
"FIS_BalancedMeals"

################################################################################
#MANOVAs for Full Set
RR_vars <- cbind(Full_Set$BART_adj_total_pumps, Full_Set$BART_points, Full_Set$BART_balloons_popped, Full_Set$BIS_score, Full_Set$BAS_score, Full_Set$reward_sensitivity, Full_Set$punishment_sensitivity)
BART_RR_vars <- cbind(Full_Set$BART_adj_total_pumps, Full_Set$BART_points, Full_Set$BART_balloons_popped)
BIS_BAS <- cbind(Full_Set$BIS_score, Full_Set$BAS_score)
reward_punishment <- cbind(Full_Set$reward_sensitivity, Full_Set$punishment_sensitivity)

#AK
#[Pillai’s Trace = 0.22273, F(7, 612) = 25.053, p < 2.2e-16]
AK.RR.MANOVA <- manova(RR_vars ~ AK_insight_plan_control, data = Full_Set)
summary(AK.RR.MANOVA)

#insig, direction?
AK.BARTRR.MANOVA <- manova(BART_RR_vars ~ AK_insight_plan_control, data = Full_Set)
summary(AK.BARTRR.MANOVA)

#[Pillai’s Trace = 0.13778, F(2, 617) = 49.298, p < 2.2e-16]
AK.BIS_BAS.MANOVA <- manova(BIS_BAS ~ AK_insight_plan_control, data = Full_Set)
summary(AK.BIS_BAS.MANOVA)

#[Pillai’s Trace = 0.15401, F(2, 617) = 56.162, p < 2.2e-16]
AK.reward_punish.ANOVA <- manova(reward_punishment ~ AK_insight_plan_control, data = Full_Set)
summary(AK.reward_punish.ANOVA)

#FIS
#[Pillai’s Trace = 4.6385, F(7, 612) = 49.298, p = 4.488e-05]
FIS.RR.MANOVA <- manova(RR_vars ~ FIS_food_insecurity_score, data = Full_Set)
summary(FIS.RR.MANOVA)

#insig, direction?
FIS.BARTRR.MANOVA <- manova(BART_RR_vars ~ FIS_food_insecurity_score, data = Full_Set)
summary(FIS.BARTRR.MANOVA)

#insig, direction?
FIS.BIS_BAS.MANOVA <- manova(BIS_BAS ~ FIS_food_insecurity_score, data = Full_Set)
summary(FIS.BIS_BAS.MANOVA)

#[Pillai’s Trace = 0.038178, F(2, 617) = 12.245, p = 6.092e-06]
FIS.reward_punish.MANOVA <- manova(reward_punishment ~ FIS_food_insecurity_score, data = Full_Set)
summary(FIS.reward_punish.MANOVA)

#interaction trending
FIS.AK_RR.MANOVA <- manova(RR_vars ~ FIS_food_insecurity_score*AK_insight_plan_control, data = Full_Set)
summary(FIS.AK_RR.MANOVA)

#Interaction - [Pillai’s Trace = 0.0153906, F(3, 614) = 3.1992, p = 0.0230]
FIS.AK_BRR.MANOVA <- manova(BART_RR_vars ~ FIS_food_insecurity_score*AK_insight_plan_control, data = Full_Set)
summary(FIS.AK_BRR.MANOVA)

#RCH
#[Pillai’s Trace = 0.05139, F(7,612) = 4.7364, p = 3.396e-05]
RCH.RR.MANOVA <- manova(RR_vars ~ Radimer_home_food_insecure, data = Full_Set)
summary(RCH.RR.MANOVA)

#insig, direction?
RCH.AK_RR.MANOVA <- manova(BART_RR_vars ~ Radimer_home_food_insecure, data = Full_Set)
summary(RCH.AK_RR.MANOVA)

#[Pillai’s Trace = 0.012334, F(2,617) = 3.8524, p = 0.02174]
RCH.BIS_BAS.MANOVA <- manova(BIS_BAS ~ Radimer_home_food_insecure, data = Full_Set)
summary(RCH.BIS_BAS.MANOVA)

#[Pillai’s Trace = 4.4425, F(2, 617) = 4.4425, p < 0.01215]
RCH.reward_punish.MANOVA <- manova(reward_punishment ~ Radimer_home_food_insecure, data = Full_Set)
summary(RCH.reward_punish.MANOVA)

#Interaction - [Pillai’s Trace = 0.023034, F(7, 610) = 2.0546, p = 0.04653]
RCH.AK_RR.MANOVA <- manova(RR_vars ~ Radimer_home_food_insecure*AK_insight_plan_control, data = Full_Set)
summary(RCH.AK_RR.MANOVA)

#Interaction - [Pillai’s Trace = 0.0149357, F(3, 614) = 3.10319, p = 0.02618]
RCH.AK_RR.MANOVA <- manova(BART_RR_vars ~ Radimer_home_food_insecure*AK_insight_plan_control, data = Full_Set)
summary(RCH.AK_RR.MANOVA)

#RCC
#[Pillai’s Trace = 0.069025, F(7, 612) = 6.4822, p = 2.186e-07]
RCC.RR.MANOVA <- manova(RR_vars ~ Radimer_cornell_food_insecure, data = Full_Set)
summary(RCC.RR.MANOVA)

#Insig, direction?
RCC.AK_RR.MANOVA <- manova(BART_RR_vars ~ Radimer_cornell_food_insecure, data = Full_Set)
summary(RCC.AK_RR.MANOVA)

#Insig, direction?
RCC.BIS_BAS.MANOVA <- manova(BIS_BAS ~ Radimer_cornell_food_insecure, data = Full_Set)
summary(RCC.BIS_BAS.MANOVA)

#[Pillai’s Trace = 0.050244, F(2, 617) = 16.32, p < 1.24e-07]
RCC.reward_punish.MANOVA <- manova(reward_punishment ~ Radimer_cornell_food_insecure, data = Full_Set)
summary(RCC.reward_punish.MANOVA)

#Interaction insig
RCC.AK_RR.MANOVA <- manova(RR_vars ~ Radimer_cornell_food_insecure*AK_insight_plan_control, data = Full_Set)
summary(RCC.AK_RR.MANOVA)

#Interaction insig
RCC.AK_RR.MANOVA <- manova(BART_RR_vars ~ Radimer_cornell_food_insecure*AK_insight_plan_control, data = Full_Set)
summary(RCC.AK_RR.MANOVA)


#FIS STATUS
#[Pillai’s Trace = 0.052287, F(14, 1224) = 2.3471, p = 0.003306] - WHY DO I HAVE 2 DF?
FStatus.RR.MANOVA <- manova(RR_vars ~ FIS_food_insecurity_status, data = Full_Set)
summary(FStatus.RR.MANOVA)

#Insig, direction? (2 DF - FIS + FIS*AK; 1 DF - AK)?
FStatus.AK_RR.MANOVA <- manova(BART_RR_vars ~ FIS_food_insecurity_status, data = Full_Set)
summary(FStatus.AK_RR.MANOVA)

#Insig, direction? - WHY DO I HAVE 2 DF?
FStatus.BIS_BAS.MANOVA <- manova(BIS_BAS ~ FIS_food_insecurity_status, data = Full_Set)
summary(FStatus.BIS_BAS.MANOVA)

#Insig, direction? - WHY DO I HAVE 2 DF?
FStatus.AK_RR.MANOVA <- manova(RR_vars ~ FIS_food_insecurity_status*AK_insight_plan_control, data = Full_Set)
summary(FStatus.AK_RR.MANOVA)

#Insig, direction? - WHY DO I HAVE 2 DF?
FStatus.AK_RR.MANOVA <- manova(BART_RR_vars ~ FIS_food_insecurity_status*AK_insight_plan_control, data = Full_Set)
summary(FStatus.AK_RR.MANOVA)

#MANOVAs for FI Set
FI.FIS_food_insecurity_score <- as.factor(FI$FIS_food_insecurity_score)
levels(FI.FIS_food_insecurity_score) <- c("['Food Secure']",
                                       "['Food Secure']",
                                       "['Food Secure, at risk']",
                                       "['Food Insecure']",
                                       "['Food Insecure']",
                                       "['Food Insecure']",
                                       "['Food Insecure']")

FIS.Radimer_home_food_insecure <- as.factor(FI$Radimer_home_food_insecure)                                       
levels(FIS.Radimer_home_food_insecure) <- c("['Food Secure']",
                                        "['Food Secure']",
                                        "['Food Secure']",
                                        "['Food Secure']",
                                        "['Food Insecure']",
                                        "['Food Insecure']",
                                        "['Food Insecure']",
                                        "['Food Insecure']")

FIS.Radimer_cornell_food_insecure <- as.factor(FI$Radimer_cornell_food_insecure)
levels(FIS.Radimer_cornell_food_insecure) <- c("['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Insecure']",
                                           "['Food Insecure']",
                                           "['Food Insecure']")

FI.RR_vars <- cbind(FI$BART_adj_total_pumps, FI$BART_points, FI$BART_balloons_popped, FI$BIS_score, FI$BAS_score, FI$reward_sensitivity, FI$punishment_sensitivity)
FI.BART_RR_vars <- cbind(FI$BART_adj_total_pumps, FI$BART_points, FI$BART_balloons_popped)
FI.BIS_BAS <- cbind(FI$BIS_score, FI$BAS_score)
FI.reward_punishment <- cbind(FI$reward_sensitivity, FI$punishment_sensitivity)

#FI-AK
#[Pillai’s Trace = 0.27705, F(7, 73) = 3.9964, p = 0.0009443]
FI.AK.RR.MANOVA <- manova(FI.RR_vars ~ AK_insight_plan_control, data = FI)
summary(FI.AK.RR.MANOVA) 

#Insig, direction?
FI.AK.BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ AK_insight_plan_control, data = FI)
summary(AK.BARTRR.MANOVA)

#[Pillai’s Trace = 0.13778, F(2, 617) = 49.298, p < 2.2e-16]
FI.AK.BIS_BAS.MANOVA <- manova(FI.BIS_BAS ~ AK_insight_plan_control, data = FI)
summary(AK.BIS_BAS.MANOVA)

#[Pillai’s Trace = 0.11522, F(2, 78) = 5.079, p = 0.008443]
FI.AK.reward_punishment.MANOVA <- manova(FI.reward_punishment ~ AK_insight_plan_control, data = FI)
summary(FI.AK.reward_punishment.MANOVA)

#FI-FIS
#Insig, direction?
FI.FIS.RR.MANOVA <- manova(FI.RR_vars ~ FIS_food_insecurity_score, data = FI)
summary(FI.FIS.RR.MANOVA)

#Interaction insig
FI.FIS.AK_RR.MANOVA <- manova(FI.RR_vars ~ FIS_food_insecurity_score*AK_insight_plan_control, data = FI)
summary(FI.FIS.AK_RR.MANOVA)

#Insig, direction?
FI.BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ FIS_food_insecurity_score, data = FI)
summary(FI.BARTRR.MANOVA)

#Interaction insig
FI.AK.BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ FIS_food_insecurity_score*AK_insight_plan_control, data = FI)
summary(FI.AK.BARTRR.MANOVA)

#Insig, direction?
FI.BIS_BAS.MANOVA <- manova(FI.BIS_BAS ~ FIS_food_insecurity_score, data = FI)
summary(FI.BIS_BAS.MANOVA)

#[Pillai’s Trace = 0.088771, F(2, 78 = 3.7994, p = 0.02664]
FI.reward_punishment.MANOVA <- manova(FI.reward_punishment ~ FIS_food_insecurity_score, data = FI)
summary(FI.reward_punishment.MANOVA)

#FI-RCH
#Insig, direction?
FI.RCH.RR.MANOVA <- manova(FI.RR_vars ~ Radimer_home_food_insecure, data = FI)
summary(FI.RCH.RR.MANOVA)

#Interaction insig
FI.RCH.AK_RR.MANOVA <- manova(FI.RR_vars ~ Radimer_home_food_insecure*AK_insight_plan_control, data = FI)
summary(FI.RCH.AK_RR.MANOVA)

#Insig, direction?
FI.RCH.BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ Radimer_home_food_insecure, data = FI)
summary(FI.RCH.BARTRR.MANOVA)

#Interaction trending
FI.RCH.AK_BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ Radimer_home_food_insecure*AK_insight_plan_control, data = FI)
summary(FI.RCH.AK_BARTRR.MANOVA)

#trending
FI.RCH.BIS_BAS.MANOVA <- manova(FI.BIS_BAS ~ Radimer_home_food_insecure, data = FI)
summary(FI.RCH.BIS_BAS.MANOVA)

#insig, direction?
FI.RCH.reward_punishment.MANOVA <- manova(FI.reward_punishment ~ Radimer_home_food_insecure, data = FI)
summary(FI.RCH.reward_punishment.MANOVA)

#FI-RCC
#[Pillai’s Trace = 0.20095, F(7, 73) = 2.6226, p = 0.01792]
FI.RCC.RR.MANOVA <- manova(FI.RR_vars ~ Radimer_cornell_food_insecure, data = FI)
summary(FI.RCC.RR.MANOVA)

#Interaction insig
FI.RCC.AK_RR.MANOVA <- manova(FI.RR_vars ~ Radimer_cornell_food_insecure*AK_insight_plan_control, data = FI)
summary(FI.RCC.AK_RR.MANOVA)

#Insig, direction?
FI.RCC.BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ Radimer_cornell_food_insecure, data = FI)
summary(FI.RCC.BARTRR.MANOVA)

#Interaction insig
FI.RCC.AK_BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ Radimer_cornell_food_insecure*AK_insight_plan_control, data = FI)
summary(FI.RCC.AK_BARTRR.MANOVA)

#Insig, direction?
FI.RCC.BIS_BAS.MANOVA <- manova(FI.BIS_BAS ~ Radimer_cornell_food_insecure, data = FI)
summary(FI.RCC.BIS_BAS.MANOVA)

#[Pillai’s Trace = 0.13678, F(2, 78) = 6.1798, p = 0.003226]
FI.RCC.reward_punishment.MANOVA <- manova(FI.reward_punishment ~ Radimer_cornell_food_insecure, data = FI)
summary(FI.RCC.reward_punishment.MANOVA)

#FI-FIS Status
#Insig, direction?
FI.FStatus.RR.MANOVA <- manova(FI.RR_vars ~ FIS_food_insecurity_status, data = FI)
summary(FI.FStatus.RR.MANOVA)

#Interaction insig
FI.FStatus.AK_RR.MANOVA <- manova(FI.RR_vars ~ FIS_food_insecurity_status*AK_insight_plan_control, data = FI)
summary(FI.FStatus.AK_RR.MANOVA)

#Insig, direction?
FI.FStatus.BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ FIS_food_insecurity_status, data = FI)
summary(FI.FStatus.BARTRR.MANOVA)

#Interaction insig
FI.FStatus.AK_BARTRR.MANOVA <- manova(FI.BART_RR_vars ~ FIS_food_insecurity_status*AK_insight_plan_control, data = FI)
summary(FI.FStatus.AK_BARTRR.MANOVA)

#Insig, direction?
FI.FStatus.BIS_BAS.MANOVA <- manova(FI.BIS_BAS ~ FIS_food_insecurity_status, data = FI)
summary(FI.FStatus.BIS_BAS.MANOVA)

#Insig, direction?
FI.reward_punishment.MANOVA <- manova(FI.reward_punishment ~ FIS_food_insecurity_status, data = FI)
summary(FI.reward_punishment.MANOVA)


################################################################################
#To-Do
#PERMANOVA 
#PERMANOVA model accuracy tests for sig results

################################################################################

