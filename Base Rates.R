#####Change Base Rate

set.seed(123)
##African Americans: 
baserate_af_pos <- df %>%
  filter(race == "African-American") %>% 
  filter(two_year_recid == 1) %>% 
  sample_n(size = 822)

baserate_af_neg <- df %>%
  filter(race == "African-American") %>% 
  filter(two_year_recid == 0) %>% 
  sample_n(size = 1281)

baserate_af <- rbind(baserate_af_pos, baserate_af_neg)

##White Americans (stays the same):
baserate_wh <- df %>%
  filter(race == "Caucasian")


####Join: 
#############
# Final Dataset with equal baserates: 
df_baserate <- rbind(baserate_af, baserate_wh)

# Check:

#African Americans: 
recid_af_base <- df_baserate %>% 
  filter(race == "African-American") %>% 
  select(two_year_recid) %>% 
  as.vector()

sum(recid_af_base[[1]])/length(recid_af_base[[1]])
# Base Rate: 0.3908702

##White Americans: 

recid_wh_base <- df_baserate %>% 
  filter(race == "Caucasian") %>% 
  select(two_year_recid) %>% 
  as.vector()

sum(recid_wh_base[[1]])/length(recid_wh_base[[1]])
# Base Rate: 0.3908702

#######################################################################################
# Check the impossibility theorem for equal baserates: 
#######################################################################################


# 1. Calibration: 

Base_Score_x <- list()
Base_Score_total <- vector()

for (i in 1:10) {
  Base_Score_x[[i]] <- df %>% 
    filter(decile_score == i) %>% 
    count(two_year_recid == 1)
  Base_Score_total[[i]] <- Base_Score_x[[i]][2,2]/(Base_Score_x[[i]][2,2] + Base_Score_x[[i]][1,2])
  
}

Base_Score_total
# [1] 0.2153966 0.3211679 0.3771252 0.4369369 0.4810997 0.5822306 0.6008065 0.7190476 0.7142857 0.8059211

# Calibration per group: 

#African-American: 
Base_Score_x_af <- list()
Base_Score_af <- vector()

for (i in 1:10) {
  Base_Score_x_af[[i]] <- df_baserate %>% 
    filter(decile_score == i) %>% 
    filter(race == "African-American") %>% 
    count(two_year_recid == 1)
  Base_Score_af[[i]] <- Base_Score_x_af[[i]][2,2]/(Base_Score_x_af[[i]][2,2] + Base_Score_x_af[[i]][1,2])
  
}

Base_Score_af
# [1] 0.1378092 0.2490119 0.2850000 0.3375527 0.3603604 0.4714286 0.4520548 0.6011905 0.6217617 0.7118644

vector_score_pos_br <- vector()
for (i in 1:10) {
  vector_score_pos_br[[i]] <- Base_Score_x_af[[i]][2,2]*Base_Score_af[[i]]
}
avp_af_br <- sum(vector_score_pos_br)/822


vector_score_neg_br <- vector()
for (i in 1:10) {
  vector_score_neg_br[[i]] <- Base_Score_x_af[[i]][1,2]*Base_Score_af[[i]]
}
avn_af_br <- sum(vector_score_neg_br)/1281


# Calibration Score for Whites:

# [1] 0.2115702 0.3115265 0.3445378 0.4032922 0.4550000 0.5812500 0.6017699 0.7500000 0.7142857 0.7000000
# [1] 0.1378092 0.2490119 0.2850000 0.3375527 0.3603604 0.4714286 0.4520548 0.6011905 0.6217617 0.7118644

########## => Calibration fullfilled 

vector_s_pos_br <- vector()
for (i in 1:10) {
  vector_s_pos_br[[i]] <- Base_Score_x_af[[i]][2,2]*i
}
avp_af_br_s <- sum(vector_s_pos_br)/822


###########################################################################################################

# 2. Balance for positve Class: 

##African-Americans: 

score_african_base <- df_baserate %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 1) %>% 
  select(decile_score) %>% 
  as.vector() 


lapply(score_african_base, mean)
##6.1691

##White Americans: 

score_white_base <- df_baserate %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 1) %>% 
  select(decile_score) %>% 
  as.vector() 

lapply(score_white_base, mean)
#4.715328

###Kleinberg: 



###Alternative: 
f_1_br <- function(x) (0.3908702)/(1-(0.3908702))*(1-x)
f_2_br <- function(x) (0.3908702)/(1-(0.3908702))*(1-x)

ggplot() +
  geom_function(aes(color = "f_1"), fun = f_1_br, size = 1.5) +
  geom_function(aes(color = "f_2"), fun = f_2_br, size = 1) +
  geom_point(aes(x=avp_af_br, y=avn_af_br), colour="black", size = 4) +
  geom_point(aes(x=avp_wh, y=avn_wh), colour="red", size = 2) +
  scale_color_manual(values = c("f_1" = "black", "f_2" = "red"),
                     name = NULL,
                     labels = c("f_1" = "Group 1", "f_2" = "Group 2")) +
  theme_bw(base_size = 12) +
  xlab(expression(paste("Average Score For Positive Class = ", gamma))) +
  ylab("Average Score For Negative Class") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = "bottom")


###################
##Chouldechova: 

########################################################################################################
######################### PPV
########################################################################################################

PV_af_br <- df_baserate %>% 
  filter(race == "African-American") %>% 
  filter(score_factor == "HighScore") %>% 
  select(two_year_recid)

PPV_af_br <- length(subset(PV_af_br[[1]],PV_af_br[[1]] == 1))/length(PV_af_br[[1]])
# [1] 0.5159292






FP_af_br <- df_baserate %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 0) %>% 
  select(score_factor)

FPR_af_br <- length(subset(FP_af_br[[1]],FP_af_br[[1]] == "HighScore"))/length(FP_af_br[[1]])
# [1] 0.4270101

FN_af_br <- df_baserate %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 1) %>% 
  select(score_factor)

FNR_af_br <- length(subset(FN_af_br[[1]],FN_af_br[[1]] == "LowScore"))/length(FN_af_br[[1]])
# [1] 0.2907543


f_c1_br <-  function(FNR) (p2/(1-p2))*((1-PPV_af_br)/PPV_af_br)*(1-FNR)
f_c2 <-  function(FNR) (p2/(1-p2))*((1-PPV_wh)/PPV_wh)*(1-FNR)

ggplot() +
  geom_function(aes(color = "f_c1_br"), fun = f_c1_br, size = 1.5) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5) +
  scale_color_manual(values = c("f_c1_br" = "black", "f_c2" = "red"),
                     name = NULL,
                     labels = c("f_c1_br" = "Group 1", "f_c2" = "Group 2")) +
  geom_point(aes(x=FNR_af_br, y=FPR_af_br), colour="black", size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="red", size = 4) +
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = "bottom")

###################################
#Calibration BASE RATES: 
Calibration_br <- as.data.frame(cbind("Score"= 1:10, Base_Score_af, Score_wh))
Calibration_long_br <- pivot_longer(Calibration_br, cols = c(Base_Score_af, Score_wh),
                                 names_to = "Group", values_to = "v_b")

ggplot(data = Calibration_long_br, aes(x = factor(Score), y = v_b, fill = factor(Group))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("black", "red"), labels = c("African-American", "White")) +  # Farben anpassen
  labs(x = "Score", y = "Positive Cases", fill = "Group") +
  theme_bw(12)
