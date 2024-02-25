##### Requirements:

## 1. Run ProPublica_Analysis.R to get the Compas dataframe "df"
## 2. Run "FairnessMetrics.R" for fairness metrics values used in the following plots
## 3. Run this code

library(scales)

#####
##Adjusting the thresholds for the groups: 

##Idea: Ajust thresholds of the groups to their median score for positive class, since they are not equal: 
## Median score for positive class: 
## African-American: 7   -> set threshold to 7
##            White: 5   -> threshold stays the same 


#####Adjust the thresholds: new modified data frame
df_thresh <- df %>% 
  filter(race %in% c("African-American", "Caucasian")) %>% 
  mutate(threshold = ifelse(race == "African-American" & decile_score > 6 | race == "Caucasian" & decile_score > 4, "high", "low"))

##Check PPV: 
#African-Americans:
PV_af_t <- df_thresh %>% 
  filter(race == "African-American") %>% 
  filter(threshold == "high") %>% 
  select(two_year_recid)

PPV_af_t <- length(subset(PV_af_t[[1]],PV_af_t[[1]] == 1))/length(PV_af_t[[1]])
#[1] 0.709596 -> higher PPV then before

#White-Americans:
PV_wh_t <- df_thresh %>% 
  filter(race == "Caucasian") %>% 
  filter(threshold == "high") %>% 
  select(two_year_recid)

PPV_wh_t <- length(subset(PV_wh_t[[1]],PV_wh_t[[1]] == 1))/length(PV_wh_t[[1]])
#[1] 0.5948276 (stays the same obviously, but for the sake of completeness)


#False positve:  
#African Americans: 
FP_af_t <- df_thresh %>% 
  filter(race == "African-American") %>% 
  filter(two_year_recid == 0) %>% 
  select(threshold)

#FPR of African-Americans:
FPR_af_t <- length(subset(FP_af_t[[1]],FP_af_t[[1]] == "high"))/length(FP_af_t[[1]])
#0.2278732 -> lower FPR than before

#False positve: 
#white Americans: 
FP_wh_t <- df_thresh %>% 
  filter(race == "Caucasian") %>% 
  filter(two_year_recid == 0) %>% 
  select(threshold)

#FPR of Whites:
FPR_wh_t <- length(subset(FP_wh_t[[1]],FP_wh_t[[1]] == "high"))/length(FP_wh_t[[1]])
#0.2201405 (stays the same obvioulsy, but for the sake of completeness)

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
# 0.4963504 -> higher FNR then before (directly adjusted by the median threshold)


## Chouldechovas connection between FPR, FNR, PPV and p calculated: 

# FPR for African-Americans:
0.52/(1 - 0.52) * (1 - PPV_af_t)/PPV_af_t
# 0.4433571

# FPR for Whites:
0.39/(1 - 0.39) * (1 - PPV_wh_t)/PPV_wh_t
# 0.4354954

# => very similar since slope is very similar 

### Chouldechova plot with adjusted thresholds:

f_c1_t <-  function(FNR) (p1/(1-p1))*((1-PPV_af_t)/PPV_af_t)*(1-FNR) 
f_c2_t <-  function(FNR) (p2/(1-p2))*((1-PPV_wh_t)/PPV_wh_t)*(1-FNR)

##Chouldechova's plot with median as thresholds: 
ggplot() +
  geom_function(aes(color = "f_c1_t"), fun = f_c1_t, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2_t"), fun = f_c2_t, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1_t" = "black", "f_c2_t" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1_t" = "African-American", "f_c2_t" = "White")) +
  geom_point(aes(x=FNR_af_t, y=FPR_af_t), colour="black", size = 4) +     #adding FNR and FPR of COMPAS for African-Americans
  geom_point(aes(x=FNR_wh_t, y=FPR_wh_t), colour="#3366FF", size = 4) +   #adding FNR and FPR of COMPAS for Whites
  theme_bw(base_size = 20) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(  
    legend.key           = element_rect(fill = NA, color = NA, size = 1),
    legend.background    = element_rect(color = "black", size = 0.25),
    legend.justification = c(1, 1),
    legend.position      = c(1, 1),
    legend.box.margin    = margin(1, 1, 1, 1),
    
  )
# ggsave("Adjust_thresh.jpg", width = 8, height = 5)


###########
# Check for statisitcal parity: 
###########

# African-Americans:
SP_af <- df_thresh %>% 
  filter(race == "African-American") %>% 
  select(threshold)

length(subset(SP_af[[1]],SP_af[[1]] == "high"))/length(SP_af[[1]])
# [1] 0.3741732 -> lower then before

# Whites:
SP_wh <- df_thresh %>% 
  filter(race == "Caucasian") %>% 
  select(threshold)

length(subset(SP_wh[[1]],SP_wh[[1]] == "high"))/length(SP_wh[[1]])
# [1] 0.3309558 

############################################################################################################
#####Distribution of Risk-Scores for pos. and neg. class: 

#### Plot score for positive cases for African Americans 

d_score_af_pos <- data.frame(scores_pos = as.numeric(score_african_1[[1]]))

# Calculate proportions
proportions_af_p <- table(cut(d_score_af_pos$scores_pos, breaks = seq(0, 10, 1))) / nrow(d_score_af_pos)

# Create a data frame for plotting
d_prop_af_p <- data.frame(
  scores_pos = seq(1,10,1),
  proportion = as.numeric(proportions_af_p)
)

sub_d_prop_af_p <- d_prop_af_p %>% 
  filter(scores_pos <= as.numeric(median_af))

vline_af <- data.frame(x = c(as.numeric(median_af), as.numeric(median_af)), y = c(0, 0.165))


# Plot using geom_point and geom_line
ggplot(d_prop_af_p, aes(x = scores_pos, y = proportion)) +
  geom_point(color = "black", size = 3) +
  geom_line(color = "black", size = 1) +
  geom_area(data = sub_d_prop_af_p, aes(x = scores_pos, y = proportion), fill = "black", alpha = 0.4, position = "identity") +
  geom_line(data = vline_af, aes(x = x, y = y), linetype = "dashed", color = "black", size = 0.7) +
  geom_text(aes(x = as.numeric(median_af), y = 0.169), label = "0.5-Quantile", vjust = 0.5, hjust = 0.5,
            family =  "Arial", size = 4.5) +  
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.17)) +
  labs(
    x = "Scores for positive class",
    y = "Cases in %")
# ggsave("Dist_pos_Scores_af.jpg",  width = 8, height = 5)


############################################################################################################
#### Plot score for positive cases for White Americans 

d_score_wh_pos <- data.frame(scores_pos = as.numeric(score_white_1[[1]]))

# Calculate proportions
proportions_wh_p <- table(cut(d_score_wh_pos$scores_pos, breaks = seq(0, 10, 1))) / nrow(d_score_wh_pos)

# Create a data frame for plotting
d_prop_wh_p <- data.frame(
  scores_pos = seq(1,10,1),
  proportion = as.numeric(proportions_wh_p)
)

sub_d_prop_wh_p <- d_prop_wh_p %>% 
  filter(scores_pos <= as.numeric(median_wh))


# Create a data frame for the vline
vline_wh <- data.frame(x = c(as.numeric(median_wh), as.numeric(median_wh)), y = c(0, 0.165))


# Plot using geom_point and geom_line
ggplot(d_prop_wh_p, aes(x = scores_pos, y = proportion)) +
  geom_point(color = "#3366FF", size = 3) +
  geom_line(color = "#3366FF", size = 1) +
  geom_area(data = sub_d_prop_wh_p, aes(x = scores_pos, y = proportion), fill = "#3366FF", alpha = 0.4, position = "identity") +
  geom_line(data = vline_wh, aes(x = x, y = y), linetype = "dashed", color = "black", size = 0.7) +
  geom_text(aes(x = as.numeric(median_wh), y = 0.169), label = "0.5-Quantile", vjust = 0.5, hjust = 0.5,
            family =  "Arial", size = 4.5) +  
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.17)) +
  labs(
    x = "Scores for positive class",
    y = "Cases in %")
# ggsave("Dist_pos_Scores_wh.jpg",  width = 8, height = 5)

