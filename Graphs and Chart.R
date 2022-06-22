#Aishat Sadiq
#FI Regressions
#Date of Last Update: 6/07/22
setwd("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/Code/31.02 Food Insecurity")

#To-Do ----
#Run 'Data Prep (Pre-Analysis).R
#Change scatterplots to bar graphs
#Decide whether I want to create kernel density plots for raw or standardized values of BART points, balloons popped, and adjusted total pumps

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

#Datasets ----
#Unfinalized scores DF
ItemLevel<- read_csv("itemlevel_2021_cleaned (2).csv")
View(ItemLevel)
colnames(ItemLevel)

#2021 Finalized Score DF
Descriptive_Library <- read_csv("final_scores_2021_3-22.csv")
View(Descriptive_Library)
colnames(Descriptive_Library)

#Cleaned Full DF; n=620
Full_Set <- select(Descriptive_Library, "BART_adj_total_pumps", "BART_points", "BART_balloons_popped","BIS_score", 
                   "BAS_score", "reward_sensitivity", "punishment_sensitivity", "AK_insight_plan_control", 
                   "Radimer_home_food_insecure", "Radimer_cornell_food_insecure", "FIS_food_insecurity_score", 
                   "FIS_food_insecurity_status", "childhood_residential_moves", "Demographics_Age", "Demographics_Race", "Demographics_Year", 
                   "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", 
                   "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", 
                   "Demographics_Live", "life_history_past", "life_history_present", "life_history_future", 
                   "life_history_total", "STROOP_total_rts_that_fit_criteria_average", 
                   "STROOP_congruent_rts_that_fit_critera_average", 
                   "STROOP_incongruent_rts_that_fit_critera_average", "STROOP_speed_interference",                        
                   "STROOP_congruent_trials_all_count", "STROOP_incongruent_trials_all_count",              
                   "STROOP_congruent_trials_that_fit_criteria_count", "STROOP_incongruent_trials_that_fit_criteria_count",
                   "STROOP_total_num_trials") %>%
  na.omit
describe(Full_Set)
View(Full_Set)

#FIS >= 2 Subset DF n=81
FI <- Full_Set[Full_Set$FIS_food_insecurity_score == "Food Secure", ]
Summary(FI)



#Full_Set %>% Percent of Sample Experiencing Food Insecurity - FIS ----
Full_Set %>%
  ggplot(aes(x=FIS_food_insecurity_score)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of FIS - Full Set",
     x = "The Subset of the Core US Household Food Insecurity Scale",
     y = "Percent of Sample")
#Full_Set %>% Percent of Sample Experiencing Food Insecurity - RCH ----
Full_Set %>%
  ggplot(aes(x=Radimer_home_food_insecure)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Home Food Insecurity - Full Set",
       x = "Radimer/Cornell  Measure  of  Hunger  and  Food  Insecurity - Home",
       y = "Percent of Sample")
#Full_Set %>% Percent of Sample Experiencing Food Insecurity - RCC ----
Full_Set %>%
  ggplot(aes(x=Radimer_cornell_food_insecure)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  labs(title = "Distribution of Cornell Food Insecurity - Full Set",
       x = "Radimer/Cornell  Measure  of  Hunger  and  Food  Insecurity - School",
       y = "Percent of Sample")

#Full_Set %>% Bar Graph of points won by FIS ----
ggplot(Full_Set, aes(FIS_food_insecurity_score)) +
       geom_bar(aes(fill=BART_points)) +
  labs(title = "Points x Food Insecurity Score",
       x = "The Subset of the Core US Household Food Insecurity Scale",
       y = "Points Won")

#Full_Set %>% Bar Graph of points won by RCH ----
ggplot(Full_Set, aes(Radimer_home_food_insecure)) +
  geom_bar(aes(fill=BART_points)) +
  labs(title = "Points x Food Insecurity Score",
       x = "Radimer/Cornell  Measure  of  Hunger  and  Food  Insecurity - Home",
       y = "Points Won")


#Full_Set %>% Bar Graph of points won by RCC ----
ggplot(Full_Set, aes(Radimer_cornell_food_insecure)) +
  geom_bar(aes(fill=BART_points)) +
  labs(title = "Points x Food Insecurity Score",
       x = "Radimer/Cornell  Measure  of  Hunger  and  Food  Insecurity - School",
       y = "Points Won")

#Full_Set %>% Bar Graph of balloons popped by FIS ----
ggplot(Full_Set, aes(FIS_food_insecurity_score)) +
  geom_bar(aes(fill=BART_points)) +
  labs(title = "Points x Food Insecurity Score",
       x = "The Subset of the Core US Household Food Insecurity Scale",
       y = "Points Won")
#Full_Set %>% Bar Graph of balloons popped by RCH ----
ggplot(Full_Set, aes(Radimer_home_food_insecure)) +
  geom_bar(aes(fill=BART_points)) +
  labs(title = "Points x Food Insecurity Score",
       x = "Radimer/Cornell  Measure  of  Hunger  and  Food  Insecurity - Home",
       y = "Points Won")
#Full_Set %>% Bar Graph of balloons popped by RCC ----
ggplot(Full_Set, aes(Radimer_cornell_food_insecure)) +
  geom_bar(aes(fill=BART_points)) +
  labs(title = "Points x Food Insecurity Score",
       x = "Radimer/Cornell  Measure  of  Hunger  and  Food  Insecurity - School",
       y = "Points Won")

#Full_Set %>% Bar Graph of BART adjusted total points by FIS ----
ggplot(Full_Set, aes(FIS_food_insecurity_score)) +
  geom_bar(aes(fill=BART_adj_total_pumps)) +
  labs(title = "BART adjusted total pumps x Food Insecurity Score",
       x = "The Subset of the Core US Household Food Insecurity Scale",
       y = "BART adjusted total pumps")

ggplot(Full_Set, aes(FIS_food_insecurity_score, BART_adj_total_pumps)) +
  geom_col()

#Full_Set %>% Bar Graph of BART adjusted total points by RCH ----
ggplot(Full_Set, aes(Radimer_home_food_insecure)) +
  geom_bar(aes(fill=BART_adj_total_pumps, stat = "summary", fun.y = "mean")) +
  labs(title = "BART adjusted total pumps x Food Insecurity Score",
       x = "Radimer/Cornell  Measure  of  Hunger  and  Food  Insecurity - Home",
       y = "BART adjusted total pumps")

#Full_Set %>% Bar Graph of BART adjusted total points by RCC ----
ggplot(Full_Set, aes(Radimer_cornell_food_insecure)) +
  geom_bar(aes(fill=BART_adj_total_pumps)) +
  labs(title = "BART adjusted total pumps x Food Insecurity Score",
       x = "Radimer/Cornell  Measure  of  Hunger  and  Food  Insecurity - School",
       y = "BART adjusted total pumps")

#FI %>% Kernel Density Plot of Points  ----
hist(FI$BART_points, freq = FALSE)

ggplot(FI, aes(BART_points)) +
  geom_density()

#FI %>% Kernel Density Plot of Balloons Popped ----
hist(FI$BART_balloons_popped, freq = FALSE)
ggplot(FI, aes(BART_balloons_popped)) +
  geom_density()
#FI %>% Kernel Density Plot of Adjusted Total Pumps ----
hist(FI$BART_adj_total_pumps, freq = FALSE)
ggplot(FI, aes(BART_adj_total_pumps)) +
  geom_density()







