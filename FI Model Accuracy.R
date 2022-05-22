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
Demographics <- select(Full_Set, "Demographics_Age", "Demographics_Race", "Demographics_Year", "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", "Demographics_Live")
View(Demographics)
describe(Demographics)


################################################################################
#Sig Main Analyses 
BIS_Full.main <- glm(BIS_score ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(BIS_Full.main)
summary(BIS_Full.main)$dispersion
#Home(B= -0.112073, p= 0.000325, SE= 0.031001, AIC= 3329) 

#Heteroscedasticity consistent standard errors
BIS.vcov <- vcovHC(BIS_Full.main, type = "HC") #"HC" gives White's estimator = does not assume constant variance and is suitable in case of heteroscedasticity
  coeftest(BIS_Full.main, vcov = BIS.vcov)

pchisq(BIS_Full.main$deviance, BIS_Full.main$df.residual, lower.tail=FALSE) #6.849754e-23
par(mfrow=c(2,2))
plot(BIS_Full.main)


BIS_Full.main2 <- glm(BIS_score ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="quasipoisson")
summary(BIS_Full.main2)
#Home(B= -0.112073, p= 0.000325, SE= 0.031001, AIC = NA) better error rate than poisson and nb
pchisq(deviance(BIS_Full.main2), df.residual(BIS_Full.main2)) #6.849754e-23
par(mfrow=c(2,2))
plot(BIS_Full.main2)

punishment_Full.main <- glm(punishment_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(punishment_Full.main)
#FIS(B=0.011866, p=0.0414, se= 0.005817); Cornell(B= 0.080812, p=5.17e-06, se= 0.017730) 
pchisq(deviance(punishment_Full.main), df.residual(punishment_Full.main)) #1
par(mfrow=c(2,2))
plot(punishment_Full.main)

punishment_Full.main2 <- glm(punishment_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="quasipoisson")
summary(punishment_Full.main2)
pchisq(deviance(punishment_Full.main2), df.residual(punishment_Full.main2)) #1


points_Full.main <- glm(BART_points ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(points_Full.main)
#Home(B= -0.045450, SE= 0.014912, p=0.0023)
pchisq(deviance(points_Full.main), df.residual(points_Full.main)) #1
par(mfrow=c(2,2))
plot(points_Full.main)

popped_Full.main <- glm(BART_balloons_popped ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Full_Set, family="poisson")
summary(popped_Full.main)
#Cornell(B= -0.09541, SE= 0.04781, p= 0.046)
pchisq(deviance(popped_Full.main), df.residual(popped_Full.main)) #1
par(mfrow=c(2,2))
plot(popped_Full.main)

################################################################################
BIS_FI.main <- glm(BIS_score ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(BIS_FI.main)

BAS_FI.main <- glm(BAS_score ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(BAS_FI.main)


punishment_FI.main <- glm(punishment_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(punishment_FI.main)
#Cornell(B= 0.08509, p=0.00123, se= 0.02633)
pchisq(deviance(punishment_FI.main), df.residual(punishment_FI.main)) #0.9999208

punishment_FI.main2 <- glm.nb(punishment_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI)
summary(punishment_FI.main2)
#Cornell(B=0.08615, SE= 0.03385, p=0.0109)
pchisq(deviance(punishment_FI.main2), df.residual(punishment_FI.main2)) #0.6790231
par(mfrow=c(2,2))
plot(punishment_FI.main2)

points_FI.main <- glm(BART_points ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(points_FI.main)
#Home(B=  -0.04526, SE= 0.01760, p=0.0.0101)
1 - pchisq(deviance(points_FI.main), df.residual(points_FI.main)) #0
par(mfrow=c(2,2))
plot(points_FI.main)

popped_FI.main <- glm(BART_balloons_popped ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(popped_Full.main)
#Cornell(B= -0.09541, SE= 0.04781, p=0.046)
1 - pchisq(deviance(popped_FI.main), df.residual(popped_FI.main)) #1.261196e-10
#quasi-poisson

popped_FI.main2 <- glm.nb(BART_balloons_popped ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI)
summary(popped_FI.main2)
#nb insig
1 - pchisq(deviance(popped_FI.main2), df.residual(popped_FI.main2)) #0.2283872
#better fit
par(mfrow=c(2,2))
plot(popped_FI.main2)

points_FI.main <- glm(BART_points ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(points_FI.main)



################################################################################
#Sig Main Analyses - FI Set (AK -> R/R)
AK_BART_FI <-glm(BART_adj_total_pumps  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_BART_FI)
#(B= 0.14778, SE= 0.04952, p=0.00285)
1 - pchisq(deviance(AK_BART_FI), df.residual(AK_BART_FI)) #2.918559e-07
#quasi-poisson insig
par(mfrow=c(2,2))
plot(AK_BART_FI)

AK_BART_FI2 <-glm.nb(BART_adj_total_pumps  ~ AK_insight_plan_control, data=FI)
summary(AK_BART_FI2)
#(B= 0.14586, SE= 0.07151, p=0.0365)
1 - pchisq(deviance(AK_BART_FI2), df.residual(AK_BART_FI2)) #0.3408204 More error, better fit
par(mfrow=c(2,2))
plot(AK_BART_FI2)

AK_point_FI <- glm(BART_points  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_point_FI)
#(B= 0.11397, SE=0.01158, p=<2e-16)
1 - pchisq(deviance(AK_point_FI), df.residual(AK_point_FI)) #0
par(mfrow=c(2,2))
plot(AK_point_FI)

AK_popped_FI <-glm(BART_balloons_popped  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_popped_FI)
#(B=0.13216, SE=0.04692, p=0.00485)
1 - pchisq(deviance(AK_popped_FI), df.residual(AK_popped_FI)) #1.014102e-10
#quasipoisson insig

AK_popped_FI2 <-glm.nb(BART_balloons_popped  ~ AK_insight_plan_control, data=FI)
summary(AK_popped_FI2)
#insig
1 - pchisq(deviance(AK_popped_FI2), df.residual(AK_popped_FI2)) #0.2813581
par(mfrow=c(2,2))
plot(AK_popped_FI2)

################################################################################
#Sig Interactions - Full Set (AK -> R/R)
AK_Full.INT <- glm(BART_adj_total_pumps ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full.INT)
#(B= 0.036548, SE= 0.015394, p= 0.01759)
interact_plot(model=AK_Full.INT, pred = FIS_food_insecurity_score, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
pchisq(deviance(AK_Full.INT), df.residual(AK_Full.INT)) 
par(mfrow=c(2,2))
plot(AK_Full.INT)

FIS_BART <- glm(BART_adj_total_pumps ~ FIS_food_insecurity_score, family="poisson", data=Full_Set)
anova(FIS_BART, AK_Full.INT, test = "Chisq") #FIS*AK interaction is sig better than FIS alone


AK_Full_HomeFI.INT <- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full_HomeFI.INT)
#(B= 0.210075, SE=0.057728, p=0.000274)
interact_plot(model=AK_Full_HomeFI.INT, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
pchisq(deviance(AK_Full_HomeFI.INT), df.residual(AK_Full_HomeFI.INT)) 
par(mfrow=c(2,2))
plot(AK_Full_HomeFI.INT)

FIS_BART <- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure, family="poisson", data=Full_Set)
anova(FIS_BART, AK_Full.INT, test = "Chisq") #FIS*AK interaction is sig better than FIS alone

RCH_BART <- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure, family="poisson", data=Full_Set)
anova(RCH_BART, AK_Full_HomeFI.INT, test = "Chisq") #RCH*AK interaction is sig better than FIS alone

AK_FI_HomeFI.INT<- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_FI_HomeFI.INT)
#Home*AK (B=0.21265, SE= 0.08672, p= 0.0142)
interact_plot(model=AK_FI_HomeFI.INT, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, point.alpha = .5)
pchisq(deviance(AK_FI_HomeFI.INT), df.residual(AK_FI_HomeFI.INT)) 
#quasi insig

FI.AK_BART <- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure, family="poisson", data=FI)
anova(FI.AK_BART, AK_FI_HomeFI.INT, test = "Chisq") #RCH*AK interaction is sig better than FIS alone for FI pop

AK_Full_CornellFI.INT<- glm(BART_adj_total_pumps ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full_CornellFI.INT)
#(B= 0.111914, SE=0.045818, p=0.01458)
interact_plot(model=AK_Full_CornellFI.INT, pred = Radimer_cornell_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
pchisq(deviance(AK_Full_CornellFI.INT), df.residual(AK_Full_CornellFI.INT)) 
par(mfrow=c(2,2))
plot(AK_Full_CornellFI.INT)

RCC_BART <- glm(BART_adj_total_pumps ~ Radimer_cornell_food_insecure, family="poisson", data=Full_Set)
anova(RCC_BART, AK_Full_CornellFI.INT, test = "Chisq") #RCC*AK interaction is sig better than FIS alone

AK_punishment_Home_Full <- glm(punishment_sensitivity  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_punishment_Home_Full)
#(B=0.051382, SE= 0.020871, p=0.0138)
interact_plot(model=AK_punishment_Home_Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
pchisq(deviance(AK_punishment_Home_Full), df.residual(AK_punishment_Home_Full)) 
par(mfrow=c(2,2))
plot(AK_punishment_Home_Full)

punishment_RCH <- glm(punishment_sensitivity  ~ Radimer_home_food_insecure, family="poisson", data=Full_Set)
anova(RCH_BART, AK_punishment_Home_Full, test = "Chisq") #AK interaction insig better

AK_popped_Home.Full <- glm(BART_balloons_popped ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_popped_Home.Full)
#(B=0.211195, SE=0.055036, p=0.000124)
interact_plot(model=AK_popped_Home.Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
pchisq(deviance(AK_popped_Home.Full), df.residual(AK_popped_Home.Full)) 
par(mfrow=c(2,2))
plot(AK_popped_Home.Full)

BART_RCH <- glm(BART_balloons_popped ~ Radimer_home_food_insecure, family="poisson", data=Full_Set)
anova(BART_RCH, AK_popped_Home.Full, test = "Chisq") #RCH*AK sig - Full

AK_popped_Home.FI <- glm(BART_balloons_popped  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_popped_Home.FI)
#Home*AK (B=0.32408, SE= 0.08243, p=8.44e-05)
interact_plot(model=AK_popped_Home.FI, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
pchisq(deviance(AK_popped_Home.FI), df.residual(AK_popped_Home.FI))

FI.BART_RCH <- glm(BART_balloons_popped ~ Radimer_home_food_insecure, family="poisson", data=FI)
anova(FI.BART_RCH, AK_popped_Home.FI, test = "Chisq") #RCH*AK interaction sig better for FI

AK_popped_Home.FI2 <- glm.nb(BART_balloons_popped  ~ Radimer_home_food_insecure*AK_insight_plan_control, data=FI)
summary(AK_popped_Home.FI2)
#Home*AK (B=0.33293, SE= 0.12069, p=0.00580)
interact_plot(model=AK_popped_Home.FI2, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
pchisq(deviance(AK_popped_Home.FI2), df.residual(AK_popped_Home.FI2))
#nb has higer SE but better chi-squared fit
par(mfrow=c(2,2))
plot(AK_popped_Home.FI2)

AK_points_FIS.Full <- glm(BART_points  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_FIS.Full)
#(B=0.035120, SE= 0.003560, p=< 2e-16)
interact_plot(model=AK_points_FIS.Full, pred = FIS_food_insecurity_score, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)
pchisq(deviance(AK_points_FIS.Full), df.residual(AK_points_FIS.Full)) 
par(mfrow=c(2,2))
plot(AK_points_FIS.Full)

points_FIS <- glm(BART_points  ~ FIS_food_insecurity_score, family="poisson", data=Full_Set)
anova(points_FIS, AK_points_FIS.Full, test = "Chisq") #FIS*AK interaction sig better - full

AK_points_Home.Full <- glm(BART_points ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_Home.Full)
#(B=0.109851, SE= 0.013408, p=2.55e-16)
interact_plot(model=AK_points_Home.Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)
pchisq(deviance(AK_points_Home.Full), df.residual(AK_points_Home.Full)) 
par(mfrow=c(2,2))
plot(AK_points_Home.Full)

points_RCH <- glm(BART_points ~ Radimer_home_food_insecure, family="poisson", data=Full_Set)
AK_Resid <- anova(points_RCH, AK_points_Home.Full, test = "Chisq") #RCH*AK interaction sig better
AK_Resid$`Resid. Dev`[1] - AK_Resid$`Resid. Dev`[2] #Drop in Deviance*
pchisq(AK_Resid$`Resid. Dev`[1] - AK_Resid$`Resid. Dev`[2], 2, lower.tail=FALSE) #p-value of deviance 
anova(points_RCH, AK_points_Home.Full, test = "Chisq")



AK_points_Cornell.Full <- glm(BART_points  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_Cornell.Full)
#(B= 0.087335, SE= 0.010493, p=<2e-16)
interact_plot(model=AK_points_Cornell.Full, pred =  Radimer_cornell_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)
pchisq(deviance(AK_points_Cornell.Full), df.residual(AK_points_Cornell.Full)) 
par(mfrow=c(2,2))
plot(AK_points_Cornell.Full)

points_RCC <- glm(BART_points  ~ Radimer_cornell_food_insecure, family="poisson", data=Full_Set)
anova(points_RCC, AK_points_Cornell.Full, test = "Chisq") #RCC*AK interaction sig better

RCC vs RCH 
severity and interaction w/ context


