#########

## RUN THIS FIRST

#########
## Comment: In order to run my analysis code, this file has to be executed first since in ProPublica's 
## analysis the dataframe "df", which I use, is prepared.
#########


##ProPublicas Analysis: 

library(dplyr)
library(ggplot2)
raw_data <- read.csv("./compas-scores-two-years.csv")
nrow(raw_data)

##Filter non usable cases: 

df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>% 
  filter(race %in% c("African-American", "Caucasian")) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") %>%
  filter(score_text != 'N/A')
nrow(df)

#Higher COMPAS scores are slightly correlated with a longer length of stay.
df$length_of_stay <- as.numeric(as.Date(df$c_jail_out) - as.Date(df$c_jail_in))
cor(df$length_of_stay, df$decile_score)

xtabs(~ sex + race, data=df)

##Recidivism: 
nrow(filter(df, two_year_recid == 1))
nrow(filter(df, two_year_recid == 1)) / nrow(df) * 100

##Score distribution: 
library(grid)
library(gridExtra)
pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(decile_score))) + 
  geom_bar() + xlab("Decile Score") +
  ylim(0, 650) + ggtitle("Black Defendant's Decile Scores")
pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(decile_score))) + 
  geom_bar() + xlab("Decile Score") +
  ylim(0, 650) + ggtitle("White Defendant's Decile Scores")
grid.arrange(pblack, pwhite,  ncol = 2)

#Racial distribution of scores: 
xtabs(~ decile_score + race, data=df)


#####
#Racial Bias in Compas
#####

#Is there a significant difference in Compas scores between races

#1. Change some variables into factors
df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race)) %>%
  within(race_factor <- relevel(race_factor, ref = 2)) %>%
  mutate(gender_factor = factor(sex, labels= c("Female","Male"))) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2)) %>%
  mutate(score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))

#2. run a logistic regression: 
model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
               priors_count + crime_factor + two_year_recid, family="binomial", data=df)
summary(model)


control <- exp(-1.52554) / (1 + exp(-1.52554))
exp(0.47721) / (1 - control + (control * exp(0.47721)))

# -> Black defendants are 45% more likely than white defendants to receive a higher score
#correcting for the seriousness of their crime, previous arrests, and future criminal behavior.

exp(0.22127) / (1 - control + (control * exp(0.22127)))
#-> Women are 19.4% more likely than men to get a higher score.

exp(1.30839) / (1 - control + (control * exp(1.30839)))
#-> Most surprisingly, people under 25 are 2.5 times as likely to get a
#higher score as middle aged defendants.
