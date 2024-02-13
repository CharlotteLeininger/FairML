#####

## 1. Run ProPublica_Analysis.R to get the Compas dataframe "df"
## 2. Run this code

#####

## In this code, frequently used values such as the base rates of the African-Americans and Whites are calculated
## and different subsets of the Compas data used in my anaysis are generated;
## the following fairenss metrics are calculated:
## Balance for positive class 
## Balance for negative class
## Error Rate Balance (False Positive Rate Balance & False Negative Rate Balance)
## Predicitve Parity (Positive Predictive Values)
## Calibration
## Statistical Parity

########################################################################################################
######################### BASE RATES 
########################################################################################################

#Base rates: 
af <- df %>% 
  filter(race == "African-American")
#3175

wh <- df %>% 
  filter(race == "Caucasian")
#2103

#African Americans: 
recid_af <- df %>% 
  filter(race == "African-American") %>% 
  select(two_year_recid) %>% 
  as.vector()

sum(recid_af[[1]])/length(recid_af[[1]])
#->0.5231496

#White Americans: 
recid_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  select(two_year_recid) %>% 
  as.vector()

sum(recid_wh[[1]])/length(recid_wh[[1]])
#-> 0.3908702

########################################################################################################
######################### BALANCE FOR POSITVE & NEGATIVE CLASS
########################################################################################################


## Balance for Positive class: 

##African-Americans: 

score_african <- df %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 1) %>% 
  select(decile_score) %>% 
  as.vector() 


avp_af <- lapply(score_african, mean)
##6.236002

##White Americans: 

score_white <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 1) %>% 
  select(decile_score) %>% 
  as.vector() 

avp_wh <- lapply(score_white, mean)
#4.715328

## Balance for Negative class: 

##African-Americans: 

score_african_0 <- df %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 0) %>% 
  select(decile_score) %>% 
  as.vector() 


avn_af <- lapply(score_african_0, mean)
#4.224571


##White Americans: 

score_white_0 <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 0) %>% 
  select(decile_score) %>% 
  as.vector() 

avn_wh <- lapply(score_white_0, mean)
#2.942233

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
#white Americans: 
FN_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 1) %>% 
  select(score_factor)

length(subset(FN_wh[[1]],FN_wh[[1]] == "LowScore"))/length(FN_wh[[1]])
#0.4963504


########################################################################################################
######################### PPV
########################################################################################################

PV_af <- df %>% 
  filter(race == "African-American") %>% 
  filter(score_factor == "HighScore") %>% 
  select(two_year_recid)

length(subset(PV_af[[1]],PV_af[[1]] == 1))/length(PV_af[[1]])
#[1] 0.6495353

PV_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  filter(score_factor == "HighScore") %>% 
  select(two_year_recid)

length(subset(PV_wh[[1]],PV_wh[[1]] == 1))/length(PV_wh[[1]])
# [1] 0.5948276


########################################################################################################
######################### CALIBRATION
########################################################################################################

# Check Calibration: 
##Total:
Score_x <- list()
Score_total <- vector()

for (i in 1:10) {
  Score_x[[i]] <- df %>% 
    filter(decile_score == i) %>% 
    count(two_year_recid == 1)
  Score_total[[i]] <- Score_x[[i]][2,2]/(Score_x[[i]][2,2] + Score_x[[i]][1,2])
  
}

Score_total
# [1] 0.2153966 0.3211679 0.3771252 0.4369369 0.4810997 0.5822306 0.6008065 0.7190476 0.7142857 0.8059211

# Calibration per group: 

#African-American: 
Score_x_af <- list()
Score_af <- vector()

for (i in 1:10) {
  Score_x_af[[i]] <- df %>% 
    filter(decile_score == i) %>% 
    filter(race == "African-American") %>% 
    count(two_year_recid == 1)
  Score_af[[i]] <- Score_x_af[[i]][2,2]/(Score_x_af[[i]][2,2] + Score_x_af[[i]][1,2])
  
}

Score_af
# [1] 0.2328767 0.3034682 0.4194631 0.4688427 0.4891641 0.5880503 0.6093294 0.7142857 0.7223975 0.8370044

#########African American Positiv Class: 
Score_x_af_n <- list()
Score_af_n <- vector()

for (i in 1:10) {
  Score_x_af_n[[i]] <- df %>% 
    filter(decile_score == i) %>% 
    filter(race == "African-American") %>% 
    count(two_year_recid == 0)
  Score_af_n[[i]] <- Score_x_af_n[[i]][2,2]/(Score_x_af_n[[i]][2,2] + Score_x_af_n[[i]][1,2])
  
}

Score_af_n
avn_af <- mean(Score_af_n)



#Score White Americans: 
Score_x_wh <- list()
Score_wh <- vector()

for (i in 1:10) {
  Score_x_wh[[i]] <- df %>% 
    filter(decile_score == i) %>% 
    filter(race == "Caucasian") %>% 
    count(two_year_recid == 1)
  Score_wh[[i]] <- Score_x_wh[[i]][2,2]/(Score_x_wh[[i]][2,2] + Score_x_wh[[i]][1,2])
  
}

Score_wh
# [1] 0.2115702 0.3115265 0.3445378 0.4032922 0.4550000 0.5812500 0.6017699 0.7500000 0.7142857 0.7000000


########################################################################################################
######################### Statistical Parity 
########################################################################################################

pred_af <- df %>% 
  filter(race == "African-American") %>% 
  select(score_factor) %>% 
  filter(score_factor == "HighScore")

length(pred_af[[1]])/length(subset(df$race, df$race == "African-American"))

pred_wh <- df %>% 
  filter(race == "Caucasian") %>% 
  select(score_factor) %>% 
  filter(score_factor == "HighScore")

length(pred_wh[[1]])/length(subset(df$race, df$race == "Caucasian"))


##-> keine statistical parity



