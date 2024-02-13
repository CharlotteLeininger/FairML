#####
##Adjsuting the thresholds for the groups: 

##Idea: Ajust thresholds of the groups to their median score for positive class, since they are not equal: 
## Median score for positive class: 
## African-American: 7   -> set threshold to 7
##            White: 5   -> threshold stays the same 



df_thresh <- df %>% 
  filter(race %in% c("African-American", "Caucasian")) %>% 
  mutate(threshold = ifelse(race == "African-American" & decile_score > 6 | race == "Caucasian" & decile_score > 4, "high", "low"))

##Check PPV: 
PV_af_t <- df_thresh %>% 
  filter(race == "African-American") %>% 
  filter(threshold == "high") %>% 
  select(two_year_recid)

PPV_af_t <- length(subset(PV_af_t[[1]],PV_af_t[[1]] == 1))/length(PV_af_t[[1]])
#[1] 0.709596 -> higher PPV then before

PV_wh_t <- df_thresh %>% 
  filter(race == "Caucasian") %>% 
  filter(threshold == "high") %>% 
  select(two_year_recid)

PPV_wh_t <- length(subset(PV_wh_t[[1]],PV_wh_t[[1]] == 1))/length(PV_wh_t[[1]])
#[1] 0.5948276


#False positve:  
#African Americans: 
FP_af_t <- df_thresh %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 0) %>% 
  select(threshold)

#FPR of African-Americans:
FPR_af_t <- length(subset(FP_af_t[[1]],FP_af_t[[1]] == "high"))/length(FP_af_t[[1]])
#0.2278732

#False positve: 
#white Americans: 
FP_wh_t <- df_thresh %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 0) %>% 
  select(threshold)

#FPR of Whites:
FPR_wh_t <- length(subset(FP_wh_t[[1]],FP_wh_t[[1]] == "high"))/length(FP_wh_t[[1]])
#0.2201405

#########
#False negatives: 
#African Americans: 
FN_af_t <- df_thresh %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 1) %>% 
  select(threshold)

FNR_af_t <- length(subset(FN_af_t[[1]],FN_af_t[[1]] == "low"))/length(FN_af_t[[1]])
# 0.4924744


#False negatives: 
#white Americans: 
FN_wh_t <- df_thresh %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 1) %>% 
  select(threshold)

FNR_wh_t <- length(subset(FN_wh_t[[1]],FN_wh_t[[1]] == "low"))/length(FN_wh_t[[1]])
# 0.4963504


### Chouldechova plot with adjusted thresholds:

f_c1_t <-  function(FNR) (p1/(1-p1))*((1-PPV_af_t)/PPV_af_t)*(1-FNR) 
f_c2_t <-  function(FNR) (p2/(1-p2))*((1-PPV_wh_t)/PPV_wh_t)*(1-FNR)


ggplot() +
  geom_function(aes(color = "f_c1_t"), fun = f_c1_t, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2_t"), fun = f_c2_t, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1_t" = "black", "f_c2_t" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1_t" = "African-American", "f_c2_t" = "White")) +
  geom_point(aes(x=FNR_af_t, y=FPR_af_t), colour="black", size = 4) +     #adding FNR and FPR of COMPAS for African-Americans
  geom_point(aes(x=FNR_wh_t, y=FPR_wh_t), colour="#3366FF", size = 4) +   #adding FNR and FPR of COMPAS for Whites
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))


