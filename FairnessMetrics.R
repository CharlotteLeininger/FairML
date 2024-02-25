##### Requirements:

## 1. Run ProPublica_Analysis.R to get the Compas dataframe "df"
## 2. Run this code

#####

## In this code, frequently used values such as the prevalences of African-Americans and Whites are calculated
## and different subsets of the Compas data used in my anaysis are generated;
## the following fairness metrics are calculated:
## Balance for positive class 
## Balance for negative class
## Error rate balance (false positive rate balance & false negative rate balance)
## Predicitve parity (Positive predictive values)
## Statistical Parity
## Equal Accuracy


# Subsets of data
# African-American:
af <- df %>% 
  filter(race == "African-American")
#3175 observations

#White:
wh <- df %>% 
  filter(race == "Caucasian")
#2103 observations


########################################################################################################
######################### PREVALENCES
########################################################################################################

#Prevalences:

#African Americans: 
recid_af <- df %>% 
  filter(race == "African-American") %>% 
  select(two_year_recid) %>% 
  as.vector()

sum(recid_af[[1]])/length(recid_af[[1]])
#-> 0.5231496 recidivism prevalence for African-Americans

#White Americans: 
recid_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  select(two_year_recid) %>% 
  as.vector()

sum(recid_wh[[1]])/length(recid_wh[[1]])
#-> 0.3908702 recidivism prevalence for Whites

########################################################################################################
######################### BALANCE FOR POSITVE & NEGATIVE CLASS
########################################################################################################

## Balance for Positive class: 

##African-Americans: 

score_african_1 <- df %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 1) %>% 
  select(decile_score) %>% 
  as.vector() 


avp_af <- lapply(score_african_1, mean)
##6.236002 average score for positive (=recidivating) African-Americans

##Median of scores for positive class: (important for group-specific threshold adjustment in Section 5.3)
median_af <- lapply(score_african_1, median)
#Score 7 is 0.5-quantile of distribution of scores for positive class for African-Americans


##White Americans: 

score_white_1 <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 1) %>% 
  select(decile_score) %>% 
  as.vector() 

avp_wh <- lapply(score_white_1, mean)
#4.715328 average score for positive (=recidivating) Whites 

##Median of scores for positive class:
median_wh <- lapply(score_white_1, median)
#Score 5 is 0.5-quantile of distribution of scores for positive class for Whites 


## Balance for Negative class: 

##African-Americans: 
score_african_0 <- df %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 0) %>% 
  select(decile_score) %>% 
  as.vector() 


avn_af <- lapply(score_african_0, mean)
#4.224571 average score for negative (=not recidivating) African-Americans


##White Americans: 
score_white_0 <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 0) %>% 
  select(decile_score) %>% 
  as.vector() 

avn_wh <- lapply(score_white_0, mean)
#2.942233 average score for negative (=not recidivating) Whites

########################################################################################################
######################### FALSE POSITIVE & FALSE NEGATIVE RATE
########################################################################################################

#######
#False positve:  
#African Americans: 
FP_af <- df %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 0) %>% 
  select(score_factor)

length(subset(FP_af[[1]],FP_af[[1]] == "HighScore"))/length(FP_af[[1]])
##0.4233818 

#False positve: 
#white Americans: 
FP_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 0) %>% 
  select(score_factor)

length(subset(FP_wh[[1]],FP_wh[[1]] == "HighScore"))/length(FP_wh[[1]])
##0.2201405



#########
#False negatives: 
#African Americans: 
FN_af <- df %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 1) %>% 
  select(score_factor)

length(subset(FN_af[[1]],FN_af[[1]] == "LowScore"))/length(FN_af[[1]])
##0.2847682

#False negatives: 
#White Americans: 
FN_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 1) %>% 
  select(score_factor)

length(subset(FN_wh[[1]],FN_wh[[1]] == "LowScore"))/length(FN_wh[[1]])
#0.4963504


########################################################################################################
######################### PPV
########################################################################################################

#African Americans: 
PV_af <- df %>% 
  filter(race == "African-American") %>% 
  filter(score_factor == "HighScore") %>% 
  select(two_year_recid)

length(subset(PV_af[[1]],PV_af[[1]] == 1))/length(PV_af[[1]])
#[1] 0.6495353

#White Americans: 
PV_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(score_factor == "HighScore") %>% 
  select(two_year_recid)

length(subset(PV_wh[[1]],PV_wh[[1]] == 1))/length(PV_wh[[1]])
# [1] 0.5948276


########################################################################################################
######################### Statistical Parity 
########################################################################################################

# African-Americans: 
pred_af <- df %>% 
  filter(race == "African-American") %>% 
  select(score_factor) %>% 
  filter(score_factor == "HighScore")

length(pred_af[[1]])/length(subset(df$race, df$race == "African-American"))
# [1] 0.576063

# Whites:
pred_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  select(score_factor) %>% 
  filter(score_factor == "HighScore")

length(pred_wh[[1]])/length(subset(df$race, df$race == "Caucasian"))
#[1] 0.3309558

##-> no statistical parity


########################################################################################################
######################### TOTAL ACCURACY: 
########################################################################################################

# True positives: 
TP <- length(subset(FN_af[[1]],FN_af[[1]] == "HighScore")) + length(subset(FN_wh[[1]],FN_wh[[1]] == "HighScore"))
# True negatives:
TN <- length(subset(FP_af[[1]],FP_af[[1]] == "LowScore")) + length(subset(FP_wh[[1]],FP_wh[[1]] == "LowScore"))

# Total accuracy: 
ACC <- (TP + TN)/5278 # n = 5278
# 0.6582039

########################################################################################################
######################### GROUP ACCURACY: 
########################################################################################################

# African-Americans:
ACC_af <- (length(subset(FP_af[[1]],FP_af[[1]] == "LowScore"))+length(subset(FN_af[[1]],FN_af[[1]] == "HighScore")))/(length(FN_af[[1]])+length(FP_af[[1]]))
# 0.6491339

# Whites:
ACC_wh <- (length(subset(FP_wh[[1]],FP_wh[[1]] == "LowScore"))+length(subset(FN_wh[[1]],FN_wh[[1]] == "HighScore")))/(length(FN_wh[[1]])+length(FP_wh[[1]]))
# 0.6718973

