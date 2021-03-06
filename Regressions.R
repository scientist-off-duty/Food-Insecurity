#Aishat Sadiq
#FI Regressions
#Date of Last Update: 6/07/22
setwd("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/Code/31.02 Food Insecurity")
#Run 'Data Prep (Pre-Analysis).R

#To-Do ----

#Libraries ----
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
library(brms)

#SES ----
SES_combined <- cbind(Full_Set$Demographics_ParentalIncome, Full_Set$Demographics_MotherEdu, Full_Set$Demographics_FatherEdu, Full_Set$life_history_past, Full_Set$life_history_present)
FIS <- cbind(Full_Set$FIS_food_insecurity_score, Full_Set$Radimer_home_food_insecure, Full_Set$Radimer_cornell_food_insecure)
RC_Full <- cbind(Full_Set$Radimer_cornell_food_insecure, Full_Set$Radimer_home_food_insecure)

as.factor(SES_combined)
summary(SES_combined)

SES_combined.fit <- manova(FIS_food_insecurity_score ~ SES_combined, family="poisson", data=Full_Set)

SES_Full.fit <-glm(FIS_food_insecurity_score ~ Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu + life_history_past + life_history_present, family="poisson", data=Full_Set)
summary(SES_Full.fit)

SES_FI.fit <-glm(FIS_food_insecurity_score ~ Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu + life_history_past + life_history_present, family="poisson", data=FI)
summary(SES_FI.fit)

SES_RCH_Full.fit <-glm(Radimer_home_food_insecure ~ Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu + life_history_past + life_history_present, family="poisson", data=Full_Set)
summary(SES_RCH_Full.fit)

SES_RCH_FI.fit <-glm(Radimer_home_food_insecure ~ Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu + life_history_past + life_history_present, family="poisson", data=FI)
summary(SES_RCH_FI.fit)

SES_RCC_Full.fit <-glm(Radimer_cornell_food_insecure ~ Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu + life_history_past + life_history_present, family="poisson", data=Full_Set)
summary(SES_RCC_Full.fit)

SES_RCC_FI.fit <-glm(Radimer_cornell_food_insecure ~ Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu + life_history_past + life_history_present, family="poisson", data=FI)
summary(SES_RCC_FI.fit)


#FI_Full
FI_Full.fit <- glm(FIS_food_insecurity_score ~ Radimer_home_food_insecure + Radimer_cornell_food_insecure, family="poisson", data=Full_Set)
summary(FI_Full.fit)
#All strongly correlated

#R/R_Full
Risk_Full.fit <- glm(BART_adj_total_pumps ~ BIS_score + BAS_score + reward_sensitivity + punishment_sensitivity, data = Full_Set, family="poisson")
summary(Risk_Full.fit)
#BIS(p=0.0485)

Risk_Bpts.Full.fit <- glm(BART_points ~ BIS_score + BAS_score + reward_sensitivity + punishment_sensitivity, data = Full_Set, family="poisson")
summary(Risk_Bpts.Full.fit)
#BIS(p=0.00798)  

Risk_Bpopped.Full.fit <- glm(BART_balloons_popped ~ BIS_score + BAS_score + reward_sensitivity + punishment_sensitivity, data = Full_Set, family="poisson")
summary(Risk_Bpopped.Full.fit)
#reward(p=0.0122); punishment (p=0.0202)

#FI_FI
FI_FI.fit <- glm(FIS_food_insecurity_score ~ Radimer_home_food_insecure + Radimer_cornell_food_insecure, family="poisson", data=FI)
summary(FI_FI.fit)
#Cornell(p=7.35e-05)

#R/R_FI
Risk_FI.fit <- glm(BART_adj_total_pumps ~ BIS_score + BAS_score + reward_sensitivity + punishment_sensitivity, data = FI, family="poisson")
summary(Risk_FI.fit)
#BIS(p=0.0101); reward(p=0.0491)

Risk_Bpts.FI.fit <- glm(BART_points ~ BIS_score + BAS_score + reward_sensitivity + punishment_sensitivity, data = FI, family="poisson")
summary(Risk_Bpts.FI.fit)
#BIS(p=0.000152); BAS(p=0.002687); reward(p=2.13e-06)  

Risk_Bpopped.FI.fit <- glm(BART_balloons_popped ~ BIS_score + BAS_score + reward_sensitivity + punishment_sensitivity, data = FI, family="poisson")
summary(Risk_Bpopped.FI.fit)
#no sig

#Main Analyses w/Full Set (FI -> R/R scales)
BART_Full.main <- glm(BART_adj_total_pumps ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(BART_Full.main)
#insig

BIS_Full.main <- glm(BIS_score ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(BIS_Full.main)
#Home(p= 0.00962)

BAS_Full.main <- glm(BAS_score ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(BAS_Full.main)
#no interaction

reward_Full.main <- glm(reward_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(reward_Full.main)
#no interaction

punishment_Full.main <- glm(punishment_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(punishment_Full.main)
#FIS(p=0.0414); Cornell(p=5.17e-06)

points_Full.main <- glm(BART_points ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(points_Full.main)
#Home(p=0.0023)

popped_Full.main <- glm(BART_balloons_popped ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(popped_Full.main)
#Cornell(0.046)

#Main Analyses w/FI Set (FI -> R/R scales)
BART_FI.main <- glm(BART_adj_total_pumps ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(BART_FI.main)
#Insig

BIS_FI.main <- glm(BIS_score ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(BIS_FI.main)
#Insig

BAS_FI.main <- glm(BAS_score ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(BAS_FI.main)
#Insig

reward_FI.main <- glm(reward_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(reward_FI.main)
#Insig

punishment_FI.main <- glm(punishment_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(punishment_FI.main)
#Cornell(p=0.00123)

points_FI.main <- glm(BART_points ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(points_FI.main)
#Home(p=0.0.0101)

popped_FI.main <- glm(BART_balloons_popped ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(popped_Full.main)
#Cornell(p=0.046)

####################### WTF DUDE, only sig for FI pop!!! ####################### ----
AK_BART_Full <-glm(BART_adj_total_pumps  ~ AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_BART_Full)
#insig

AK_BART_FI <-glm(BART_adj_total_pumps  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_BART_FI)
#p=0.00285

AK_points_Full <- glm(BART_points  ~ AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_Full)
#insig

AK_point_FI <- glm(BART_points  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_point_FI)
#p=<2e-16

AK_popped_Full <-glm(BART_balloons_popped  ~ AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_popped_Full)
#insig

AK_popped_FI <-glm(BART_balloons_popped  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_popped_FI)
#p=0.00485

#Interaction Analyses + Plots

#CI for models - confint(model_name)

#BART_adj_total_pumps x FI x AK - Full & FI ----
AK_Full.INT <- glm(BART_adj_total_pumps ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full.INT)
#p=0.01759
interact_plot(model=AK_Full.INT, pred = FIS_food_insecurity_score, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)

AK_FI.INT <- glm(BART_adj_total_pumps ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_FI.INT)
#no interaction, WHYYYYY????
interact_plot(model=AK_FI.INT, pred = FIS_food_insecurity_score, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)

AK_Full_HomeFI.INT <- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full_HomeFI.INT)
#p=0.000274
interact_plot(model=AK_Full_HomeFI.INT, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)

AK_FI_HomeFI.INT<- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_FI_HomeFI.INT)
#p=0.0142
interact_plot(model=AK_FI_HomeFI.INT, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, point.alpha = .5)

AK_Full_CornellFI.INT<- glm(BART_adj_total_pumps ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full_CornellFI.INT)
#p=0.01458
interact_plot(model=AK_Full_CornellFI.INT, pred = Radimer_cornell_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)

AK_FI_CornellFI.INT <- glm(BART_adj_total_pumps ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_FI_CornellFI.INT)
#no interaction
interact_plot(model=AK_FI_CornellFI.INT, pred = Radimer_cornell_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)

#BIS_score x FI x AK - Full & FI ----
AK_BIS_FIS.Full <- glm(BIS_score  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_BIS_FIS.Full)
#no interaction

AK_BIS_FIS.FI <- glm(BIS_score  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_BIS_FIS.FI)
#no interaction

AK_BIS_Home_Full <- glm(BIS_score  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_BIS_Home_Full)
#no interaction

AK_BIS_Home_FI <- glm(BIS_score  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_BIS_Home_FI)
#no interaction

AK_BIS_Cornell_Full <- glm(BIS_score  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_BIS_Cornell_Full)
#no interaction

AK_BIS_Cornell_FI <- glm(BIS_score  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_BIS_Cornell_FI)
#no interaction


#punishment x FI x AK - Full & FI ----
AK_punish_FIS.Full <- glm(punishment_sensitivity  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_punish_FIS.Full)
#no interaction

AK_punish_FIS.FI <- glm(punishment_sensitivity  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_punish_FIS.FI)
#no interaction

AK_punishment_Home_Full <- glm(punishment_sensitivity  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_punishment_Home_Full)
#p=0.0138
interact_plot(model=AK_punishment_Home_Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)

AK_punishment_Home_FI <- glm(punishment_sensitivity  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_punishment_Home_FI)
#no interaction 

AK_punishment_Cornell_Full <- glm(punishment_sensitivity  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_punishment_Cornell_Full)
#no interaction

AK_punishment_Cornell_FI <- glm(punishment_sensitivity  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_punishment_Cornell_FI)
#no interaction


#BART popped x FI x AK - Full & FI ----
AK_popped_FIS.Full <- glm(BART_balloons_popped  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_popped_FIS.Full)
#no interaction

AK_popped_FIS.FI <- glm(BART_balloons_popped  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_popped_FIS.FI)
#no interaction

AK_popped_Home.Full <- glm(BART_balloons_popped ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_popped_Home.Full)
#p=0.000124
interact_plot(model=AK_popped_Home.Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)

AK_popped_Home.FI <- glm(BART_balloons_popped  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_popped_Home.FI)
#p=8.44e-05
interact_plot(model=AK_popped_Home.FI, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)

AK_popped_Cornell.Full <- glm(BART_balloons_popped  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_popped_Cornell.Full)
#no interaction

AK_popped_Cornell.FI <- glm(BART_balloons_popped  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_popped_Cornell.FI)
#no interaction

#BART points x FI x AK - Full & FI ----
AK_points_FIS.Full <- glm(BART_points  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_FIS.Full)
#p=< 2e-16
interact_plot(model=AK_points_FIS.Full, pred = FIS_food_insecurity_score, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)

AK_points_FIS.FI <- glm(BART_points  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_points_FIS.FI)
#no interaction
interact_plot(model=AK_points_FIS.FI, pred = FIS_food_insecurity_score, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)

AK_points_Home.Full <- glm(BART_points ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_Home.Full)
#p=2.55e-16
interact_plot(model=AK_points_Home.Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)

AK_points_Home.FI <- glm(BART_points  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_points_Home.FI)
#no interaction
interact_plot(model=AK_points_Home.FI, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)

AK_points_Cornell.Full <- glm(BART_points  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_Cornell.Full)
#<2e-16
interact_plot(model=AK_points_Cornell.Full, pred =  Radimer_cornell_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)

AK_points_Cornell.FI <- glm(BART_points  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_points_Cornell.FI)
#no interaction
interact_plot(model=AK_points_Cornell.FI, pred =  Radimer_cornell_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)





