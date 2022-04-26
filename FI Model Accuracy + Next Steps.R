#4/25 FI Model Accuracy and Next Steps
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
#1 Chi squared goodness of fit test w/ residuals 
   1 - pchisq(deviance(df), df.residual(df))
#2
   drop1(df, test = "F")
#3
   residuals(modObj, type = "resType") #returns a vector of the residuals
#4 
   predict(modObj, type = "fitType") #returns a vector of fitted values
#5 likelihood ratio test (LRT)
#6 For quasi family models an F-test is used for nested model tests
#7 Look to see if a negative binomial model might be a better fit.
   
################################################################################
#Sig Main Analyses - FI Set (FI -> R/R scales)
punishment_FI.main <- glm(punishment_sensitivity ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(punishment_FI.main)
#Cornell(p=0.00123)
1 - pchisq(deviance(punishment_FI.main), df.residual(punishment_FI.main)) #x^2 = 7.922698e-05
#Correct functional form of the expected means - Linearity of the predicted response variable
scatter.smooth(fitted(punishment_FI.main), residuals(punishment_FI.main, type = "pearson"),
               mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted on green in regulation proportion")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")




points_FI.main <- glm(BART_points ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(points_FI.main)


popped_FI.main <- glm(BART_balloons_popped ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = FI, family="poisson")
summary(popped_Full.main)
#Cornell(p=0.046)
1 - pchisq(deviance(popped_FI.main), df.residual(popped_FI.main)) #1.261196e-10
#Correct functional form of the expected means - Linearity of the predicted response variable
scatter.smooth(fitted(popped_FI.main), residuals(popped_FI.main, type = "pearson"),
               mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted on green in regulation proportion")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")


#Sig Main Analyses - FI Set (AK -> R/R)
AK_BART_FI <-glm(BART_adj_total_pumps  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_BART_FI)
#p=0.00285
1 - pchisq(deviance(AK_BART_FI), df.residual(AK_BART_FI)) #2.918559e-07
#Correct functional form of the expected means - Linearity of the predicted response variable
scatter.smooth(fitted(AK_BART_FI), residuals(AK_BART_FI, type = "pearson"),
               mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted on green in regulation proportion")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")


AK_point_FI <- glm(BART_points  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_point_FI)
#p=<2e-16
1 - pchisq(deviance(AK_point_FI), df.residual(AK_point_FI))
drop1(AK_point_FI, test = "F")

AK_popped_FI <-glm(BART_balloons_popped  ~ AK_insight_plan_control, family="poisson", data=FI)
summary(AK_popped_FI)
#p=0.00485
1 - pchisq(deviance(AK_popped_FI), df.residual(AK_popped_FI)) #1.014102e-10
#Correct functional form of the expected means - Linearity of the predicted response variable
scatter.smooth(fitted(AK_popped_FI), residuals(AK_popped_FI, type = "pearson"),
               mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted on green in regulation proportion")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")


#Sig Interactions - FI Set (AK -> R/R)
AK_Full.INT <- glm(BART_adj_total_pumps ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full.INT)
#p=0.01759
interact_plot(model=AK_Full.INT, pred = FIS_food_insecurity_score, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
1 - pchisq(deviance(AK_Full.INT), df.residual(AK_Full.INT))
drop1(AK_Full.INT, test = "F") #insig

AK_Full_HomeFI.INT <- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full_HomeFI.INT)
#p=0.000274
interact_plot(model=AK_Full_HomeFI.INT, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
1 - pchisq(deviance(AK_Full_HomeFI.INT), df.residual(AK_Full_HomeFI.INT))
drop1(AK_Full_HomeFI.INT, test = "F")

AK_FI_HomeFI.INT<- glm(BART_adj_total_pumps ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_FI_HomeFI.INT)
#p=0.0142
interact_plot(model=AK_FI_HomeFI.INT, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, point.alpha = .5)
1 - pchisq(deviance(AK_FI_HomeFI.INT), df.residual(AK_FI_HomeFI.INT)) #9.794177e-07
#Correct functional form of the expected means - Linearity of the predicted response variable
scatter.smooth(fitted(AK_FI_HomeFI.INT), residuals(AK_FI_HomeFI.INT, type = "pearson"),
               mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted on green in regulation proportion")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")


AK_Full_CornellFI.INT<- glm(BART_adj_total_pumps ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_Full_CornellFI.INT)
#p=0.01458
interact_plot(model=AK_Full_CornellFI.INT, pred = Radimer_cornell_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
1 - pchisq(deviance(AK_Full_CornellFI.INT), df.residual(AK_Full_CornellFI.INT))
drop1(AK_Full_CornellFI.INT, test = "F") #insig

AK_punishment_Home_Full <- glm(punishment_sensitivity  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_punishment_Home_Full)
#p=0.0138
interact_plot(model=AK_punishment_Home_Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
1 - pchisq(deviance(AK_punishment_Home_Full), df.residual(AK_punishment_Home_Full))
drop1(AK_punishment_Home_Full, test = "F") #insig

AK_popped_Home.Full <- glm(BART_balloons_popped ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_popped_Home.Full)
#p=0.000124
interact_plot(model=AK_popped_Home.Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
1 - pchisq(deviance(AK_popped_Home.Full), df.residual(AK_popped_Home.Full))
drop1(AK_popped_Home.Full, test = "F")

AK_popped_Home.FI <- glm(BART_balloons_popped  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=FI)
summary(AK_popped_Home.FI)
#p=8.44e-05
interact_plot(model=AK_popped_Home.FI, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, interval= TRUE, jitter = 1, point.alpha = .5)
1 - pchisq(deviance(AK_popped_Home.FI), df.residual(AK_popped_Home.FI)) #1.351646e-08
#Correct functional form of the expected means - Linearity of the predicted response variable
scatter.smooth(fitted(AK_popped_Home.FI), residuals(AK_popped_Home.FI, type = "pearson"),
               mgp = c(2.2, 1, 0),
               ylab = "Residuals (Pearson)",
               xlab = "Predicted on green in regulation proportion")
title("Residual plot", line = 0.7)
abline(h = 0, col="blue")

AK_points_FIS.Full <- glm(BART_points  ~ FIS_food_insecurity_score*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_FIS.Full)
#p=< 2e-16
interact_plot(model=AK_points_FIS.Full, pred = FIS_food_insecurity_score, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)
1 - pchisq(deviance(AK_points_FIS.Full), df.residual(AK_points_FIS.Full))
drop1(AK_points_FIS.Full, test = "F")

AK_points_Home.Full <- glm(BART_points ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_Home.Full)
#p=2.55e-16
interact_plot(model=AK_points_Home.Full, pred = Radimer_home_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)
1 - pchisq(deviance(AK_points_Home.Full), df.residual(AK_points_Home.Full))
drop1(AK_points_Home.Full, test = "F") #insig

AK_points_Cornell.Full <- glm(BART_points  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Full_Set)
summary(AK_points_Cornell.Full)
#<2e-16
interact_plot(model=AK_points_Cornell.Full, pred =  Radimer_cornell_food_insecure, modx=AK_insight_plan_control, set.offset = 1, plot.points = TRUE, jitter = 1, interval= TRUE, point.alpha = .5)
1 - pchisq(deviance(AK_points_Cornell.Full), df.residual(AK_points_Cornell.Full))
drop1(AK_points_Cornell.Full, test = "F")

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################
To Do

Cameron and Trivedi (2009) recommended using robust standard errors for the parameter estimates to control for mild violation of the distribution 
assumption that the variance equals the mean. We use R package sandwich below to obtain the robust standard errors and calculated the p-values 
accordingly. Together with the p-values, we have also calculated the 95% confidence interval using the parameter estimates and their robust 
standard errors.

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)
r.est

Now let’s look at the output of function glm more closely.

The output begins with echoing the function call. The information on deviance residuals is displayed next. 
Deviance residuals are approximately normally distributed if the model is specified correctly.In our example, it shows a little bit of skeweness 
since median is not quite zero.

Next come the Poisson regression coefficients for each of the variables along with the standard errors, z-scores, p-values and 95% confidence 
intervals for the coefficients. The coefficient for math is .07. This means that the expected log count for a one-unit increase in math is .07. 
The indicator variable progAcademic compares between prog = “Academic” and prog = “General”, the expected log count for prog = “Academic” 
increases by about 1.1. The indicator variable prog.Vocational is the expected difference in log count ((approx .37)) between prog = “Vocational” 
and the reference group (prog = “General”).

The information on deviance is also provided. We can use the residual deviance to perform a goodness of fit test for the overall model. 
The residual deviance is the difference between the deviance of the current model and the maximum deviance of the ideal model where the 
predicted values are identical to the observed. Therefore, if the residual difference is small enough, the goodness of fit test will not be 
significant, indicating that the model fits the data. We conclude that the model fits reasonably well because the goodness-of-fit chi-squared 
test is not statistically significant. If the test had been statistically significant, it would indicate that the data do not fit the model well. 
In that situation, we may try to determine if there are omitted predictor variables, if our linearity assumption holds and/or if there is an 
issue of over-dispersion.











