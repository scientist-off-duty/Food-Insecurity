#Food Insecurity Data Prep
#Aishat Sadiq
#Last Update: 6/13/22
setwd("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/Code/31.02 Food Insecurity")
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
library(sandwich)

#Full Dataframe #620ptps ----
Descriptive_Library <- read_csv("final_scores_2021_3-22.csv")
View(Descriptive_Library)
colnames(Descriptive_Library)
#Subset of necessary (variables) columns for analysis; n=601 ----
Full_Set <- select(Descriptive_Library, "BART_adj_total_pumps", "BART_points", "BART_balloons_popped","BIS_score", 
                   "BAS_score", "reward_sensitivity", "punishment_sensitivity", "AK_insight_plan_control", 
                   "Radimer_home_food_insecure", "Radimer_cornell_food_insecure", "FIS_food_insecurity_score", 
                   "FIS_food_insecurity_status", "childhood_residential_moves", "Demographics_Age", "Demographics_Race", "Demographics_Year", 
                   "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", 
                   "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", 
                   "Demographics_Live", "life_history_past", "life_history_present", "life_history_future", 
                   "life_history_total") %>%
  na.omit
describe(Full_Set)
View(Full_Set)

#FIS >= 2 Subset Dataframe #78ptps ----
FI <- subset(Full_Set, Full_Set$FIS_food_insecurity_score >= 2)
View(FI)
describe(FI)

##setting Race as cat/factor variable ----
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

##setting FIS measures in Full_Set DF as categorical and specifying the new levels ----
Full_Set$FIS_food_insecurity_score <- as.factor(Full_Set$FIS_food_insecurity_score)
levels(Full_Set$FIS_food_insecurity_score) <- c("['Food Secure']",
                                                "['Food Secure']",
                                                "['Food Secure, at risk']",
                                                "['Food Insecure']",
                                                "['Food Insecure']",
                                                "['Food Insecure']",
                                                "['Food Insecure']")

Full_Set$Radimer_home_food_insecure <- as.factor(Full_Set$Radimer_home_food_insecure)                                       
levels(Full_Set$Radimer_home_food_insecure) <- c("['Food Secure']",
                                                 "['Food Secure']",
                                                 "['Food Secure']",
                                                 "['Food Secure']",
                                                 "['Food Insecure']",
                                                 "['Food Insecure']",
                                                 "['Food Insecure']",
                                                 "['Food Insecure']")

Full_Set$Radimer_cornell_food_insecure <- as.factor(Full_Set$Radimer_cornell_food_insecure)
levels(Full_Set$Radimer_cornell_food_insecure) <- c("['Food Secure']",
                                                    "['Food Secure']",
                                                    "['Food Secure']",
                                                    "['Food Secure']",
                                                    "['Food Insecure']",
                                                    "['Food Insecure']",
                                                    "['Food Insecure']")

##setting FIS measures in FI DF as categorical and specifying the new levels ----
FI$FIS_food_insecurity_score <- as.factor(FI$FIS_food_insecurity_score)
levels(FI$FIS_food_insecurity_score) <- c("['Food Secure']",
                                          "['Food Secure']",
                                          "['Food Secure, at risk']",
                                          "['Food Insecure']",
                                          "['Food Insecure']",
                                          "['Food Insecure']",
                                          "['Food Insecure']")

FI$Radimer_home_food_insecure <- as.factor(FI$Radimer_home_food_insecure)                                       
levels(FI$Radimer_home_food_insecure) <- c("['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Secure']",
                                           "['Food Insecure']",
                                           "['Food Insecure']",
                                           "['Food Insecure']",
                                           "['Food Insecure']")

FI$Radimer_cornell_food_insecure <- as.factor(FI$Radimer_cornell_food_insecure)
levels(FI$Radimer_cornell_food_insecure) <- c("['Food Secure']",
                                              "['Food Secure']",
                                              "['Food Secure']",
                                              "['Food Secure']",
                                              "['Food Insecure']",
                                              "['Food Insecure']",
                                              "['Food Insecure']")
View(Full_Set)

##dealing with ordinal predictor variables in Full_Set ----
Full_Set$Demographics_MotherEdu <- factor(Full_Set$Demographics_MotherEdu, ordered = TRUE, 
                                          levels = c("I don't know", "Less than high school", "High School Diploma or GED", "Some college", "Bachelor's Degree", "Master's Degree", "PhD, MD, DOS, OD, JD"))
summary(as.factor(Full_Set$Demographics_MotherEdu))

Full_Set$Demographics_FatherEdu <- factor(Full_Set$Demographics_FatherEdu, ordered = TRUE, 
                                          levels = c("I don't know", "Less than high school", "High School Diploma or GED", "Some college", "Bachelor's Degree", "Master's Degree", "PhD, MD, DOS, OD, JD"))
summary(as.factor(Full_Set$Demographics_FatherEdu))

Full_Set$Demographics_ParentalIncome <- factor(Full_Set$Demographics_ParentalIncome, ordered = TRUE, 
                                               levels = c("Below $40,000", "$40,000 - $59,999", "$60,000 - $99,999", "$100,000 - $174,999", "$175,000 - $299,000", "$300,000 - $499,999", "$500,000 - $749,999", "More than $750,000"))
summary(as.factor(Full_Set$Demographics_ParentalIncome))

Full_Set$FIS_food_insecurity_status <- factor(Full_Set$FIS_food_insecurity_status, ordered = TRUE)
summary(Full_Set$FIS_food_insecurity_status)

Full_Set$Demographics_Year <- factor(Full_Set$Demographics_Year, ordered = TRUE)
summary(Full_Set$Demographics_Year)


Full_Set$Demographics_Transfer <- factor(Full_Set$Demographics_Transfer)
summary(Full_Set$Demographics_Transfer)

Full_Set$Demographics_CornellCollege <- factor(Full_Set$Demographics_CornellCollege)
summary(Full_Set$Demographics_CornellCollege)

Full_Set$Demographics_GenderID <- factor(Full_Set$Demographics_GenderID)
summary(Full_Set$Demographics_GenderID)

Full_Set$Demographics_Sex <- factor(Full_Set$Demographics_Sex)
summary(Full_Set$Demographics_Sex)

Full_Set$Demographics_Live <- factor(Full_Set$Demographics_Live)
summary(Full_Set$Demographics_Live)

##dealing with ordinal predictor variables in FI ----
FI$Demographics_MotherEdu <- factor(FI$Demographics_MotherEdu, ordered = TRUE, 
                                    levels = c("I don't know", "Less than high school", "High School Diploma or GED", "Some college", "Bachelor's Degree", "Master's Degree", "PhD, MD, DOS, OD, JD"))
summary(as.factor(FI$Demographics_MotherEdu))

FI$Demographics_FatherEdu <- factor(FI$Demographics_FatherEdu, ordered = TRUE, 
                                    levels = c("I don't know", "Less than high school", "High School Diploma or GED", "Some college", "Bachelor's Degree", "Master's Degree", "PhD, MD, DOS, OD, JD"))
summary(as.factor(FI$Demographics_FatherEdu))

FI$Demographics_ParentalIncome <- factor(FI$Demographics_ParentalIncome, ordered = TRUE, 
                                         levels = c("Below $40,000", "$40,000 - $59,999", "$60,000 - $99,999", "$100,000 - $174,999", "$175,000 - $299,000", "$300,000 - $499,999", "$500,000 - $749,999", "More than $750,000"))
summary(as.factor(FI$Demographics_ParentalIncome))

FI$FIS_food_insecurity_status <- factor(FI$FIS_food_insecurity_status, ordered = TRUE)
summary(FI$FIS_food_insecurity_status)

FI$Demographics_Year <- factor(FI$Demographics_Year, ordered = TRUE)
summary(FI$Demographics_Year)

FI$Demographics_Transfer <- factor(FI$Demographics_Transfer)
summary(FI$Demographics_Transfer)

FI$Demographics_CornellCollege <- factor(FI$Demographics_CornellCollege)
summary(FI$Demographics_CornellCollege)

FI$Demographics_GenderID <- factor(FI$Demographics_GenderID)
summary(FI$Demographics_GenderID)

FI$Demographics_Sex <- factor(FI$Demographics_Sex)
summary(FI$Demographics_Sex)

FI$Demographics_Live <- factor(FI$Demographics_Live)
summary(FI$Demographics_Live)

#dealing with high variance variables in Full Set -> scaled to standard deviation ----
Full_Set$BART_points <- scale(Full_Set$BART_points)

#dealing with high variance variables in FI -> scaled to standard deviation ----
FI$BART_points <- scale(FI$BART_points)


