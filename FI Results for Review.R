#7/21/22
#Food Insecurity Results
setwd("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/Code/31.02 Food Insecurity")
#Data Prep ----

#Libraries
library(tidyverse) #pretty df
library(gridExtra) #necessary for grid.arrange() function to have multiple ggplots on 1 page
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
library(devtools)
library(mediation)
library(countES)
library(nlme)
library(brms)

#Read in Dataframes 
itemlevel_2021 <- read_csv("itemlevel_2021_cleaned (2).csv")
Descriptive_Library <- read_csv("final_scores_2021_3-22.csv")

#Filter out graduate students 
Undergrad <- Descriptive_Library %>%
  filter(Demographics_Year != "Graduate student") 

#Subset variables of interest and remove participants w/ missing values to create final datafrmae
Full_Set <- dplyr::select(Undergrad, "BART_adj_total_pumps", "BART_points", "BART_balloons_popped",
                          "BIS_score", "BAS_score", "reward_sensitivity", "punishment_sensitivity", 
                          "AK_insight_plan_control", 
                          "Radimer_home_food_insecure", "Radimer_cornell_food_insecure", "FIS_food_insecurity_score","FIS_food_insecurity_status", 
                          "childhood_residential_moves", "Demographics_Age", "Demographics_Race", "Demographics_Year", 
                          "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", 
                          "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", 
                          "Demographics_Live", "life_history_past", "life_history_present", "life_history_future", "life_history_total") %>% 
  na.omit()

#Create a new DF with participants w/ FI score 2+
FI <- subset(Full_Set, Full_Set$FIS_food_insecurity_score >= 2)

#Set Race as categorical/factor variable for Full and FI subset
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

#Encode ordered factor variables for Full
Full_Set$Demographics_MotherEdu <- factor(Full_Set$Demographics_MotherEdu, ordered = TRUE, 
                                          levels = c("I don't know", "Less than high school", "High School Diploma or GED", "Some college", "Bachelor's Degree", "Master's Degree", "PhD, MD, DOS, OD, JD"))
Full_Set$Demographics_FatherEdu <- factor(Full_Set$Demographics_FatherEdu, ordered = TRUE, 
                                          levels = c("I don't know", "Less than high school", "High School Diploma or GED", "Some college", "Bachelor's Degree", "Master's Degree", "PhD, MD, DOS, OD, JD"))
Full_Set$Demographics_ParentalIncome <- factor(Full_Set$Demographics_ParentalIncome, ordered = TRUE, 
                                               levels = c("Below $40,000", "$40,000 - $59,999", "$60,000 - $99,999", "$100,000 - $174,999", "$175,000 - $299,000", "$300,000 - $499,999", "$500,000 - $749,999", "More than $750,000"))
Full_Set$FIS_food_insecurity_status <- factor(Full_Set$FIS_food_insecurity_status, ordered = TRUE)
Full_Set$Demographics_Year <- factor(Full_Set$Demographics_Year, ordered = TRUE)
Full_Set$Demographics_Transfer <- factor(Full_Set$Demographics_Transfer)
Full_Set$Demographics_CornellCollege <- factor(Full_Set$Demographics_CornellCollege)
Full_Set$Demographics_GenderID <- factor(Full_Set$Demographics_GenderID)
Full_Set$Demographics_Sex <- factor(Full_Set$Demographics_Sex)
Full_Set$Demographics_Live <- factor(Full_Set$Demographics_Live)

#Encode ordered factor variables for FI subset
FI$Demographics_MotherEdu <- factor(FI$Demographics_MotherEdu, ordered = TRUE, 
                                    levels = c("I don't know", "Less than high school", "High School Diploma or GED", "Some college", "Bachelor's Degree", "Master's Degree", "PhD, MD, DOS, OD, JD"))
FI$Demographics_FatherEdu <- factor(FI$Demographics_FatherEdu, ordered = TRUE, 
                                    levels = c("I don't know", "Less than high school", "High School Diploma or GED", "Some college", "Bachelor's Degree", "Master's Degree", "PhD, MD, DOS, OD, JD"))

FI$Demographics_ParentalIncome <- factor(FI$Demographics_ParentalIncome, ordered = TRUE, 
                                         levels = c("Below $40,000", "$40,000 - $59,999", "$60,000 - $99,999", "$100,000 - $174,999", "$175,000 - $299,000", "$300,000 - $499,999", "$500,000 - $749,999", "More than $750,000"))

FI$FIS_food_insecurity_status <- factor(FI$FIS_food_insecurity_status, ordered = TRUE)
FI$Demographics_Year <- factor(FI$Demographics_Year, ordered = TRUE)
FI$Demographics_Transfer <- factor(FI$Demographics_Transfer)
FI$Demographics_CornellCollege <- factor(FI$Demographics_CornellCollege)
FI$Demographics_GenderID <- factor(FI$Demographics_GenderID)
FI$Demographics_Sex <- factor(FI$Demographics_Sex)
FI$Demographics_Live <- factor(FI$Demographics_Live)


#Descriptive findings of sociodemographic characteristics ----
#Proportions (Race, Year, Transfer status, gender, and living on/off campus)         #TODO: Compile into one table with percentages
summary(Full_Set$Demographics_Age)
summary(Full_Set$Demographics_Transfer)
summary(Full_Set$Demographics_GenderID)
summary(Full_Set$Demographics_Sex)
summary(Full_Set$Demographics_Live)

summary(FI$Demographics_Age)
summary(FI$Demographics_Transfer)
summary(FI$Demographics_GenderID)
summary(FI$Demographics_Sex)
summary(FI$Demographics_Live)

##TODO: Scatterplot of "childhood_residential_moves"

#Distribution of racial groups within full sample 
Full_Set %>%                                              #wrap in layer_data() for raw data 
  ggplot(aes(x=Demographics_Race)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Complete Distribution of Racial Groups",
       x = " ",
       y = "Percent of Sample")    

#Distribution of racial group within FI subset            #Note: The proportion of PoC increases from Full Set to FI subset.
FI %>%  #specifically for FI individuals
  ggplot(aes(x=Demographics_Race)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Distribution of FIS - FI",
       x = " ",
       y = "Percent of Sample")

#Distribution of Academic Year within full and FI sample
Full_Set %>%                                                                       #wrap in layer_data() for raw data 
  ggplot(aes(x=Demographics_Year)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Complete Distribution of Academic Year",
       x = "Academic Year",
       y = "Percent of Sample")  

FI %>%                                              #wrap in layer_data() for raw data 
  ggplot(aes(x=Demographics_Year)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Academic Year for FI Subset",
       x = "Academic Year",
       y = "Percent of Sample")


#Proportions of Cornell Colleges represented in the data 
Full_Set %>%  
  ggplot(aes(x=Demographics_CornellCollege)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Complete Distribution of Cornell College",
       x = "   ",
       y = "Percent of Sample")

#Distribution of Parental Income within Full and FI subset    
Full_Set %>%  
  ggplot(aes(x=Demographics_ParentalIncome)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Complete Distribution of Parental Income",
       x = "   ",
       y = "Percent of Sample")
FI %>%  
  ggplot(aes(x=Demographics_ParentalIncome)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Distribution of Parental Income for FI Subset",
       x = "   ",
       y = "Percent of Sample")

#Distribution of Mother's Education Level within full sample and FI subset
#Visible difference 
Full_Set %>%  
  ggplot(aes(x=Demographics_MotherEdu)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Complete Distribution of Mother's Highest Education Level",
       x = "   ",
       y = "Percent of Sample")

FI %>%  
  ggplot(aes(x=Demographics_MotherEdu)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Distribution of Mother's Highest Education Level for Food Insecure Subset",
       x = "   ",
       y = "Percent of Sample")

#Distribution of Father's Education Level within full sample and FI subset
#Visible difference
Full_Set %>%  
  ggplot(aes(x=Demographics_FatherEdu)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Complete Distribution of Father's Highest Education Level",
       x = "   ",
       y = "Percent of Sample")

FI %>%  
  ggplot(aes(x=Demographics_FatherEdu)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  coord_flip() +
  labs(title = "Distribution of Father's Highest Education Level for Food Insecure Subset",
       x = "   ",
       y = "Percent of Sample")

# Prevalence and level of food insecurity ----
#Percentage of food insecure undergraduates
#A: 78 people; 12.72%
sum(Full_Set$FIS_food_insecurity_score > 1)                 #number of people
((sum(Full_Set$FIS_food_insecurity_score > 1))/613)*100     #percentage of complete sample

#Distribution of Food Insecurity 
Full_Set %>%
  ggplot(aes(x=FIS_food_insecurity_score)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Complete Distribution of Food Insecurity",
       x = "The Subset of the Core US Household Food Insecurity Scale",
       y = "Percent of Sample")

# Group differences between food insecure and food secure (FIS scale) on behavioral risk and reward task? ----
#MANOVA -> BART_RR ~FIS*IPC

# Risk and Reward Outcomes ----
#Q: Is food insecurity score associated with risk taking? 
#A: As food insecurity increases self reported sensitivity to punishment increases. As food insecurity decreases, self reported behavioral inhibition decreases
#                         Estimate  Std. Error  z value  Pr(>|z|)    
#BIS_score              -0.063429   0.021231  -2.988  0.00281 ** 
#punishment_sensitivity  0.057243   0.006182   9.259  < 2e-16 ***

fit <- glm(FIS_food_insecurity_score ~ 
               BART_adj_total_pumps+
               BART_points+
               BART_balloons_popped +                                           
               BIS_score+
               BAS_score+
               reward_sensitivity+
               punishment_sensitivity,                                          
               family="poisson", data=Full_Set)

# Insight, Planning, and Control Mediation Analyses ----
#Q: Is executive control a mediator between Food Insecurity score and risk taking for the full sample?          A: Data does not support

#Step 1: Is there an association between Food Insecurity score and executive control? 
                            #Estimate   Std. Error t value  Pr(>|t|)    
#FIS_food_insecurity_score -0.06415    0.02413    -2.659    0.00804 ** 
summary(glm(AK_insight_plan_control ~ FIS_food_insecurity_score, data= Full_Set))           #sig                     

#Step 2: Is there an association between executive control and risk taking?
summary(glm(BART_adj_total_pumps ~ AK_insight_plan_control, data= Full_Set))
summary(glm(BART_points ~ AK_insight_plan_control,  data= Full_Set))
summary(glm(BART_balloons_popped ~ AK_insight_plan_control, data= Full_Set))
summary(glm(BIS_score  ~ AK_insight_plan_control, data= Full_Set))
summary(glm(BAS_score  ~ AK_insight_plan_control, data= Full_Set))                           #sig
summary(glm(reward_sensitivity  ~ AK_insight_plan_control, data= Full_Set))                  #sig
summary(glm(punishment_sensitivity  ~ AK_insight_plan_control, data= Full_Set))              #sig

#Step 3: Is there an association between food insecurity score and risk taking?
summary(glm(BART_adj_total_pumps ~ FIS_food_insecurity_score, data= Full_Set))
summary(glm(BART_points ~ FIS_food_insecurity_score,  data= Full_Set))
summary(glm(BART_balloons_popped ~ FIS_food_insecurity_score, data= Full_Set))
summary(glm(BIS_score ~ FIS_food_insecurity_score, data= Full_Set))
summary(glm(BAS_score ~ FIS_food_insecurity_score, data= Full_Set))
summary(glm(reward_sensitivity ~ FIS_food_insecurity_score, data= Full_Set))
summary(glm(punishment_sensitivity ~ FIS_food_insecurity_score, data= Full_Set))              #sig

mediate_model = glm(AK_insight_plan_control ~ FIS_food_insecurity_score, data= Full_Set)
full_model = glm(BART_adj_total_pumps  ~ AK_insight_plan_control + FIS_food_insecurity_score, data= Full_Set)

summary(mediate(mediate_model, full_model, treat= 'FIS_food_insecurity_score',                #cannot perform robust SE on nonparametric bootstrap 
                mediator = 'AK_insight_plan_control', boot=TRUE, sims=5000))

# Q: Is executive control a mediator between 
# food insecurity score and risk taking for the food insecure subset?                         A: Data does not support

#Step 1: Is there an association between Food Insecurity score and executive control?       A: No
summary(glm(AK_insight_plan_control ~ FIS_food_insecurity_score, data= FI))                                

#Step 2: Is there an association between executive control and risk taking?
summary(glm(BART_adj_total_pumps ~ AK_insight_plan_control, data= FI))              #sig                   
summary(glm(BART_points ~ AK_insight_plan_control,  data= FI))                      #sig
summary(glm(BART_balloons_popped ~ AK_insight_plan_control, data= FI))              #sig
summary(glm(BIS_score  ~ AK_insight_plan_control, data= FI))
summary(glm(BAS_score  ~ AK_insight_plan_control, data= FI))                        #sig                           
summary(glm(reward_sensitivity  ~ AK_insight_plan_control, data= FI))                  
summary(glm(punishment_sensitivity  ~ AK_insight_plan_control, data= FI))           #sig              

#Step 3: Is there an association between food insecurity score and risk taking?
summary(glm(BART_adj_total_pumps ~ FIS_food_insecurity_score, data= FI))
summary(glm(BART_points ~ FIS_food_insecurity_score,  data= FI))
summary(glm(BART_balloons_popped ~ FIS_food_insecurity_score, data= FI))
summary(glm(BIS_score ~ FIS_food_insecurity_score, data= FI))
summary(glm(BAS_score ~ FIS_food_insecurity_score, data= FI))
summary(glm(reward_sensitivity ~ FIS_food_insecurity_score, data= FI))
summary(glm(punishment_sensitivity ~ FIS_food_insecurity_score, data= FI))           #sig              

mediate_model2 = glm(AK_insight_plan_control ~ FIS_food_insecurity_score, data= FI)
full_model2 = glm(BART_adj_total_pumps  ~ AK_insight_plan_control + FIS_food_insecurity_score, data= FI)

summary(mediate(mediate_model2, full_model2, treat= 'FIS_food_insecurity_score',                #cannot perform robust SE on nonparametric bootstrap 
                mediator = 'AK_insight_plan_control', boot=TRUE, sims=5000))





