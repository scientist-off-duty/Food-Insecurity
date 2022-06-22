#Aishat Sadiq
#Food Insecurity and Decision Making in Uncertain Environments
#Date of Last Update: 4/9/22

setwd("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/30 LHL - PARA/31 Ongoing Projects/Food Insecurity")
#############################################################################################################
########################################## Libraries + DF Set Up ##############################################################
#############################################################################################################
library(tidyverse) #pretty df
library(psych)
library(psychTools)
library(dplyr) #pretty df
library(ggplot2) #graphs
library(kableExtra)
library(Gmisc) #summary stats
library(magrittr)
library(dplyr)
library(htmlTable)
library(gtsummary)


Descriptive_Library <- final_scores_2021_3_22 
Full_Set <- na.omit(Descriptive_Library)


#Full Sample Profile
Demographics_ <- select(Descriptive_Library, "Demographics_Age", "Demographics_Race", "Demographics_Year", "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", "Demographics_Live", "FIS_food_insecurity_score")
View(Demographics)
describe(Demographics)

#Correcting data types
Age <- Demographics$Demographics_Age
Race <- as.factor(Demographics$Demographics_Race)
Year <- as.factor(Demographics$Demographics_Year)
Transfer <- as.factor(Demographics$Demographics_Transfer)
CorellCollege <- as.factor(Demographics$Demographics_CornellCollege)
ParentalIncome <- as.factor(Demographics$Demographics_ParentalIncome)
MotherEdu <- as.factor(Demographics$Demographics_MotherEdu)
FatherEdu <- as.factor(Demographics$Demographics_FatherEdu)
GenderID <- as.factor(Demographics$Demographics_GenderID)
Sex <- as.factor(Demographics$Demographics_Sex)
Live <- as.factor(Demographics$Demographics_Live)

#df of all ptp demographics w/ FI score > 2
FI <- subset(Demographics, Demographics$FIS_food_insecurity_score >= 2, na.rm=TRUE)
View(FI)
describe(FI)

#FI dist w/in full pop vs FI dist w/in FI>=2 group
hist(Demographics$FIS_food_insecurity_score, probability = TRUE)
hist(FI$Demographics_Age, probability = TRUE)


as.factor(FI$FIS_food_insecurity_score)
FI %>%
  ggplot(aes(x=FIS_food_insecurity_score)) + ## When adding ggplot layers, you use the + operator
  geom_bar()

FI %>%
  ggplot(aes(x=Demographics_Age)) +
  geom_bar() 


FI %>%
  mutate(Demographics_Age = as.character(Demographics_Age)) %>%
  ggplot(aes(x=Demographics_Age)) +
  geom_bar() 

FI %>%
  ggplot(aes(x=Demographics_Sex)) +
  geom_bar()

FI %>%
  ggplot(aes(x=Demographics_ParentalIncome)) +
  geom_bar()

FI %>%
  ggplot(aes(x=Demographics_MotherEdu)) +
  geom_bar()

FI %>%
  ggplot(aes(x=Demographics_FatherEdu)) +
  geom_bar()


FI %>%
  ggplot(aes(FIS_food_insecurity_score) +
  geom_bar()


summary(Year,  na.rm=TRUE)
summary(Age)
summary(Transfer)
summary(CorellCollege)
summary(ParentalIncome na.rm=TRUE)
as.factor

(as.factor(FI$Demographics_ParentalIncome), na.rm = TRUE)

summary(MotherEdu)
summary(FatherEdu)
summary(GenderID)
summary(Sex)
summary(Live)


FI.Risk2 <-Descriptive_Library %>%
  ggplot(aes(x=FIS_food_insecurity_score)) +
  geom_bar(aes(fill=BART_adj_total_pumps), position = "dodge")
FI.Risk2


Cornell_FI <- Descriptive_Library$Radimer_cornell_food_insecure
hist(Cornell_FI)
Home_FI <- Descriptive_Library$Radimer_home_food_insecure
Overall_FI_Score <- Descriptive_Library$FIS_food_insecurity_score 
Demographics_Age <- Descriptive_Library$Demographics_Age

#how identity and SES is related to these factors both at home and cornell
sum(Overall_FI_Score == '6', na.rm=TRUE) #18
sum(Overall_FI_Score == '5', na.rm=TRUE) #10
sum(Overall_FI_Score == '4', na.rm=TRUE) #18
sum(Overall_FI_Score == '3', na.rm=TRUE) #22
sum(Overall_FI_Score == '2', na.rm=TRUE) #41
sum(Overall_FI_Score == '1', na.rm=TRUE) #59

#############################################################################################################
########################################## Regressions ##############################################################
#############################################################################################################
#To-Do
##Run descriptives for moderator/ mediators and outcomes
##SES measures: life_history_past + life_history_present + life_history_future + life_history_total + Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu

FI.Risk.fit <- glm(BART_adj_total_pumps ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, data = Descriptive_Library, family="poisson")
summary(FI.Risk.fit)
exp(coef(FI.Risk.fit))
exp(confint(FI.Risk.fit))

#BIS -> Behavioral Inhibition System(sensitive to punishment) 
#BAS -> Behavioral Activation System(sensitive to reward)
suppressWarnings(BART.Risk.fit <- glm(BART_adj_total_pumps ~ BIS_score + BAS_score + reward_sensitivity + punishment_sensitivity, data = Descriptive_Library, family="poisson"))
summary(BART.Risk.fit)

#AK_insight_plan_control

Food.Insec.fit <- glm(FIS_food_insecurity_score ~ Radimer_home_food_insecure + Radimer_cornell_food_insecure, family="poisson", data=Descriptive_Library)
summary(Food.Insec.fit)

    
Food.Insec.subset.fit <- glm(FIS_food_insecurity_score ~ Radimer_home_food_insecure + Radimer_cornell_food_insecure, family="poisson", data=subset(Descriptive_Library, FIS_food_insecurity_score >= 2))
summary(Food.Insec.subset.fit)

FI_subset <- subset(Descriptive_Library, FIS_food_insecurity_score > 1)
na.omit(FI_subset)
hist(FI_subset$FIS_food_insecurity_score, probability=TRUE)
    
subset.fit <-glm(BART_adj_total_pumps ~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure, family="poisson", data=subset(Descriptive_Library, FIS_food_insecurity_score >= 2))
#FIS > 2 = Low food security + Very low food security = food insecure
summary(subset.fit) 

subset.fit2 <-glm(BART_adj_total_pumps ~ FIS_food_insecurity_score + Radimer_cornell_food_insecure, family="poisson", data=subset(Descriptive_Library, FIS_food_insecurity_score >= 2))
#FIS > 2 = Low food security + Very low food security = food insecure
summary(subset.fit2) 


insight.fit <-glm(BART_adj_total_pumps  ~ AK_insight_plan_control, family="poisson", data=subset(Descriptive_Library, FIS_food_insecurity_score >= 2))
summary(insight.fit)


insight.fit.cornell <-glm(BART_adj_total_pumps  ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=Descriptive_Library)
summary(insight.fit.cornell)

insight.fit.cornell.subset <-glm(BART_adj_total_pumps ~ Radimer_cornell_food_insecure*AK_insight_plan_control, family="poisson", data=subset(Descriptive_Library, FIS_food_insecurity_score >= 2))
summary(insight.fit.cornell)

#Hi Ak, low Ak
#Hi FI, low FI 
#GG PLOT, relationship between AK and Radimer cornell 

insight.fit.home <-glm(BART_adj_total_pumps  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=Descriptive_Library)
summary(insight.fit.home)

insight.fit.home2 <-glm(BART_adj_total_pumps  ~ Radimer_home_food_insecure*AK_insight_plan_control, family="poisson", data=subset(Descriptive_Library, FIS_food_insecurity_score >= 2))
summary(insight.fit.home2)
    
#GIMME analysis - FI -> BART rounds ("learning"), need to use non-finalized data set
        
#############################################################################################################
########################################## PLOTS ##############################################################
#############################################################################################################

FI.BART <- ggplot(aes(x=Descriptive_Library$FIS_food_insecurity_score)) + 
      geom_bar(aes(fill=Descriptive_Library$BART_adj_total_pumps), position = "dodge")
FI.BART       
#how many people are doing worse at cornell vs at home and vise versa

hist(Cornell_FI, probability = TRUE) #relative frequencies
hist(Home_FI, probability = TRUE)
hist(Overall_FI_Score, probability = TRUE) 
        
FI.Risk2 <-Descriptive_Library %>%
      ggplot(aes(x=FIS_food_insecurity_score)) +
      geom_bar(aes(fill=BART_adj_total_pumps), position = "dodge")
FI.Risk2

        