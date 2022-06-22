#4/27 FI Demographics
#Aishat Sadiq

setwd("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/30 LHL - PARA/31 Ongoing Projects/Food Insecurity")

#Libraries
library(tidyverse) #pretty data cleaning
library(psych) 
library(psychTools)
library(dplyr) #pretty data handling
library(ggplot2) #pretty graphs
library(Gmisc) #summary stats
library(scales) 
library(readr) #reading in csv files
library(interactions) #interaction plots
library(sandwich) #robust covariance matrix - estimates, std errors, z-values, and p-values

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
FI.Demographics <- subset(Demographics, Full_Set$FIS_food_insecurity_score >= 2)

Full_Set$Demographics_Race <- as.factor(Full_Set$Demographics_Race)
levels(Full_Set$Demographics_Race) <- c("['Mixed Race']",             
                  "['Mixed Race']",
                  "['Mixed Race']",         
                  "['Mixed Race']",                                          
                  "['American Indian or Alaskan Native']",                                                                         
                  "['Mixed Race']",                                                                                   
                  "['Mixed Race']",                                                                                
                  "['Asian']",                                                                                                     
                  "['Mixed Race']",                                                                                           
                  "['Mixed Race']",                                       
                  "['Mixed Race']",                                                                                  
                  "['Mixed Race']",                                                                                      
                  "['Mixed Race']",                                                                                        
                  "['Black']",                                                                                                     
                  "['Mixed Race']",                                                                   
                  "['Mixed Race']",                                                                                 
                  "['Mixed Race']",                                                          
                  "['Mixed Race']",                                                                      
                  "['Mixed Race']",                                                                            
                  "['Hispanic/Latinx']",                                                                                           
                  "['Middle Eastern']",                                                                                            
                  "['Mixed Race']",                                                                                                
                  "['Unknown']",                                                                                                   
                  "['Mixed Race']",                                                        
                  "['Mixed Race']",                                    
                  "['Mixed Race']",                                                                      
                  "['Mixed Race']",                                                        
                  "['Mixed Race']",                                                                      
                  "['Mixed Race']",                                                            
                  "['Mixed Race']",                                                  
                  "['Mixed Race']",                                                             
                  "['Mixed Race']",                                                                 
                  "['Mixed Race']",                                                           
                  "['Mixed Race']",                                                                    
                  "['White (of European descent)']")  
summary(Full_Set$Demographics_Race)
                     
FI$Demographics_Race <- as.factor(FI$Demographics_Race)  
levels(FI$Demographics_Race) <- c("['Mixed Race']", 
                      "['Mixed Race']",                                           
                      "['Mixed Race']",                                                                                 
                      "['Asian']",                                                                                                     
                      "['Mixed Race']",                                                                                            
                      "['Mixed Race']",                                                                                  
                      "['Black']",                                                                                                     
                      "['Mixed Race']",                                                                             
                      "['Hispanic/Latinx']",                                                                                           
                      "['Middle Eastern']",                                                                                                   
                      "['Unknown']",  
                      "['Mixed Race']",                                                           
                      "['Mixed Race']",                                                           
                      "['Mixed Race']", 
                      "['White (of European descent)']",
                      "['White (of European descent)']")
summary(FI$Demographics_Race)

################################################################################
Demographics %>%
  ggplot(aes(x=Race)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Race - Full Set",
       y = "Percent of Sample") +
  coord_flip()

FI.Demographics %>%
  ggplot(aes(x=Race.FI)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Race for FIS > 2",
       y = "Percent of Sample") +
  coord_flip()


summary(Race) 
#n=620
#Mixed Race - 93
#American Indian or Alaskan Native - 1
#Asian - 212
#Black - 37
#Hispanic/Latinx - 37
#Middle Eastern - 16
#Unknown - 7
#White (of European descent) - 217

summary(Race.FI)
#n=81
#Mixed Race - 12
#Asian - 19
#Black - 13
#Hispanic/Latinx - 12
#Middle Eastern - 2
#Unknown - 2
#White (of European descent) - 21

#What is the prevalence of food insecurity by race
Full_Set %>%
  ggplot(aes(x=Race)) +
  geom_col() +


Full_Set %>%
  ggplot(aes(x=Race, y=Radimer_home_food_insecure)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_linedraw()

Full_Set %>%
  ggplot(aes(x=Race, y=Radimer_cornell_food_insecure)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  theme_linedraw()

Overall_FI_Score <- Full_Set$FIS_food_insecurity_score 
Home_FI <- Full_Set$Radimer_home_food_insecure
Cornell_FI <- Full_Set$Radimer_cornell_food_insecure

Gender <- as.factor(Demographics$Demographics_GenderID)
Sex <- as.factor(Demographics$Demographics_Sex)
Gender_Sex <- glm(Gender ~ Sex, data = Demographics, family = "binomial")
summary(Gender_Sex)

logit1 <- glm(f.trump ~ ideol + gender + education + income, data = ds.sub,
              family = binomial(link = logit), x = TRUE)
summary(logit1)


################################################################################
Full_Set %>%
  ggplot(aes(x=FIS_food_insecurity_score)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of FIS - Full Set",
       x = "Food Insecurity Score",
       y = "Percent of Sample")

Full_Set %>%
  ggplot(aes(x=Radimer_home_food_insecure)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Home Food Insecurity - Full Set",
       x = "Radimer-Cornell Home",
       y = "Percent of Sample")

Full_Set %>%
  ggplot(aes(x=Radimer_cornell_food_insecure)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Cornell Food Insecurity - Full Set",
       x = "Radimer-Cornell School",
       y = "Percent of Sample")

#R/R.Full Set - Distribution Scatterplots of points won by FI measure
Full_Set %>%
  ggplot(aes(x=FIS_food_insecurity_score, y=BART_points)) + 
  geom_point(alpha=.5) +
  geom_jitter() +
  geom_smooth(method=glm, , se = F, 
              method.args = list(family = "poisson")) +
  labs(title = "Distribution of BART points by FIS Score",
       x = "Food Insecurity Score",
       y = "Winnings")

Full_Set %>%
  ggplot(aes(x=Radimer_cornell_food_insecure, y=BART_points)) + 
  geom_point(alpha=.5) +
  geom_jitter() +
  geom_smooth(method=glm, , se = F, 
              method.args = list(family = "poisson")) +
  labs(title = "Distribution of BART points by FIS Score",
       x = "Food Insecurity @ Cornell",
       y = "Winnings")

Full_Set %>%
  ggplot(aes(x=Radimer_home_food_insecure, y=BART_points)) + 
  geom_point(alpha=.5) +
  geom_jitter() +
  geom_smooth(method=glm, , se = F, 
              method.args = list(family = "poisson")) +
  labs(title = "Distribution of BART points by FIS Score",
       x = "Food Insecurity @ Home",
       y = "Winnings")

#FI.FI Set - Distribution of FIS, RCC, RCH
FI %>%
  ggplot(aes(x=FIS_food_insecurity_score)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of FIS - Food Insecure Subset",
       x = "Food Insecurity Score",
       y = "Percent of Sample")

FI %>%
  ggplot(aes(x=Radimer_home_food_insecure)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Home Food Insecurity - Food Insecure Subset",
       x = "Radimer-Cornell Home",
       y = "Percent of Sample")

FI %>%
  ggplot(aes(x=Radimer_cornell_food_insecure)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Cornell Food Insecurity - Food Insecure Subset",
       x = "Radimer-Cornell Home",
       y = "Percent of Sample")

#R/R.FI - Distribution of points won by FI measure
FI %>%
  ggplot(aes(x=FIS_food_insecurity_score, y=BART_points)) + 
  geom_point() +
  geom_smooth(method=glm, , se = F, 
              method.args = list(family = "poisson")) +
  labs(title = "Distribution of BART points by FIS Score for FIs >=2",
       x = "Food Insecurity Score >=2",
       y = "Winnings")

FI %>%
  ggplot(aes(x=Radimer_cornell_food_insecure, y=BART_points)) + 
  geom_point() +
  geom_smooth(method=glm, , se = F, 
              method.args = list(family = "poisson")) +
  labs(title = "Distribution of BART points by FIS Score for FIs >=2",
       x = "Food Insecurity @ Cornell",
       y = "Winnings")

FI %>%
  ggplot(aes(x=Radimer_home_food_insecure, y=BART_points)) + 
  geom_point() +
  geom_smooth(method=glm, , se = F, 
              method.args = list(family = "poisson")) +
  labs(title = "Distribution of BART points by FIS Score for FIs >=2",
       x = "Food Insecurity @ Home",
       y = "Winnings")



################################################################################

To-Do
#MANOVA using FIS status
#macarthur scales
################################################################################







