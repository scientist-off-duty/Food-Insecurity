#Aishat Sadiq
#Structural Equation Model - FI.2021
#Date of Last Update: 6/07/22
setwd("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/Code")

#TO-DO
#Troubleshoot Models 1 + 3
#Create models 2 + 4
#check assumptions for SEM

#SEM Code format ----
#myModel <- ' # regressions
             #y1 + y2 ~ f1 + f2 + x1 + x2
             #     f1 ~ f2 + f3
             #     f2 ~ f3 + x1 + x2

             # latent variable definitions 
             #  f1 =~ y1 + y2 + y3 
             #  f2 =~ y4 + y5 + y6 
             #  f3 =~ y7 + y8 + y9 + y10

             # variances and covariances 
             #  y1 ~~ y1 
             #  y1 ~~ y2 
             #  f1 ~~ f2

             # intercepts 
             #  y1 ~ 1 
             #  f1 ~ 1
#          '
#Load Packages ----
library(readr) #parsing a flat file into a tibble
library(tidyverse) #pretty data
library(sandwich) #HC and HAC covariances matrices
library(lavaan)
library(lavaanPlot)
library(semTools)
library(semPlot)
library(knitr)
library(mvnormalTest) #univariate (Shapiro-Wilk’s W) and multivariate normality (Mardia’s Multivariate Skewness and Kurtosis tests)
library(fastDummies)

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
                   "life_history_total","STROOP_congruent_rts_that_fit_critera_average", 
                   "STROOP_incongruent_rts_that_fit_critera_average", "STROOP_speed_interference",                        
                   "STROOP_congruent_trials_all_count", "STROOP_incongruent_trials_all_count",              
                   "STROOP_congruent_trials_that_fit_criteria_count", "STROOP_incongruent_trials_that_fit_criteria_count",
                   "STROOP_total_num_trials") %>%
  na.omit
describe(Full_Set)
View(Full_Set)


#FIS >= 2 Subset DF n=81
FI <- subset(Full_Set, Full_Set$FIS_food_insecurity_score >= 2)
View(FI)
summary(FI)

#Demographics Dataframe
Demographics <- select(Full_Set, "Demographics_Age", "Demographics_Race", "Demographics_Year", "Demographics_Transfer", "Demographics_CornellCollege","Demographics_ParentalIncome", "Demographics_MotherEdu", "Demographics_FatherEdu", "Demographics_GenderID", "Demographics_Sex", "Demographics_Live", "life_history_past", "life_history_present")
View(Demographics)
describe(Demographics)


#Model 1: As simple as it gets ----
#Simple model
model1 <- '
# latent variable definitions 
FI =~ FIS_food_insecurity_score
RC =~ Radimer_home_food_insecure + Radimer_cornell_food_insecure           
BART =~ BART_points + BART_balloons_popped + BART_adj_total_pumps
BIS_BAS =~ BIS_score + BAS_score
SPRQ =~  punishment_sensitivity + reward_sensitivity
Executive_Control =~ AK_insight_plan_control

# regressions
BART ~ FI
BIS_BAS ~ FI
SPRQ ~ FI

#residual correlations
'
fit1 <- sem(model1, data = Full_Set, ordered = c("FIS_food_insecurity_score", "Radimer_home_food_insecure", "Radimer_cornell_food_insecure"))

summary(fit1, standardized = TRUE, fit.measures = TRUE)
semPaths(fit1, title=FALSE, "std",edge.label.cex=0.5) ##green = positive, red = negative, lightness = strength of contribution of latent variable

#Model 2: Full DF w/ all variables ----
##removed FIS status or score bc: lavaan WARNING: correlation between variables FIS_food_insecurity_status and FIS_food_insecurity_score is (nearly) 1.0 
model2 <- '
# latent variable definitions 
SES =~ Demographics_ParentalIncome + Demographics_MotherEdu + Demographics_FatherEdu + life_history_past + life_history_present 
Food_Insecurity =~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure           
BART =~ BART_points + BART_balloons_popped + BART_adj_total_pumps
Risk_Taking =~ BIS_score + BAS_score + punishment_sensitivity
Reward_Seeking =~ reward_sensitivity
Executive_Control =~ AK_insight_plan_control

# regressions
Food_Insecurity ~ SES 
Food_Insecurity ~ Executive_Control * Risk_Taking + Reward_Seeking

#residual correlations
'

#fit the model and see the results
fit2 <- sem(model2, data = Full_Set, check.gradient = FALSE, ordered = c("Demographics_ParentalIncome", 
            "Demographics_MotherEdu", "Demographics_FatherEdu", "FIS_food_insecurity_score", 
            "Radimer_home_food_insecure", "Radimer_cornell_food_insecure"))
                   
summary(fit2, standardized = TRUE, fit.measures = TRUE)
semPaths(fit2, title=FALSE, "std",edge.label.cex=0.5) ##green = positive, red = negative, lightness = strength of contribution of latent variable
varTable(fit2)




#Model 3: Composite Radimer Conrell score ----
#Radimer-Cornell Home and School added together for composite R-C score
#Model 4: piecewiseSEM ----
#(non-parametric test) of best SEM model?

#Fit statistics ----

selected_fit_stats <-   
  c("chisq.scaled",
    "df.scaled", ## must be >0 to test G.O.F.
    "pvalue.scaled", ## ideally n.s.
    "cfi.scaled", ## ideally ≥ 0.95
    "rmsea.scaled", ## ideally ≤ 0.05
    "rmsea.pvalue.scaled", ## ideally n.s.
    "srmr" ## ideally < 0.08
  )


###########################################################
#                Marlen's Questions                        #
#                                                          #
 #########################################################

# Can we create a latent Risk tolerance?

risk_tolerance_latent_CFA <- '
# latent variable definitions 

Risk_Taking =~ reward_sensitivity + BAS_score  + BART_adj_total_pumps
SES =~ Demographics_ParentalIncome + Demographics_MotherEdu + life_history_past + life_history_present 
Food_Insecurity =~ FIS_food_insecurity_score + Radimer_home_food_insecure + Radimer_cornell_food_insecure 

# regressions
Risk_Taking ~ Food_Insecurity + SES



#residual correlations
'
risktolerance<-sem(risk_tolerance_latent_CFA, data = Full_Set)
summary(risktolerance, fit.measures = T)
semPaths(risktolerance, title=FALSE, "std",edge.label.cex=0.5) 
