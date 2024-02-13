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
df <- df <- mutate(df, crime_factor = factor(c_charge_degree)) %>%
  mutate(age_factor = as.factor(age_cat)) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  mutate(race_factor = factor(race)) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
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

####
# Predictive Accuracy of COMPAS
####

# # In order to test whether Compas scores do an accurate job of deciding whether an offender is Low, Medium or High risk, 
# we ran a Cox Proportional Hazards model. Northpointe, the company that created COMPAS and markets it to Law Enforcement, 
# also ran a Cox model in their validation study.
# # We used the counting model and removed people when they were incarcerated. Due to errors in the underlying jail data,
# we need to filter out 32 rows that have an end date more than the start date. Considering that there are 13,334 total
# rows in the data, such a small amount of errors will not affect the results.

library(survival)
library(ggfortify)

data <- filter(filter(read.csv("./cox-parsed.csv"), score_text != "N/A"), end > start) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(score_factor = factor(score_text)) %>%
  within(score_factor <- relevel(score_factor, ref=2))

grp <- data[!duplicated(data$id),]
nrow(grp)

##Model call 
f <- Surv(start, end, event, type="counting") ~ score_factor
model <- coxph(f, data=data)
summary(model)

# #-> People placed in the High category are 3.5 times as likely to recidivate, 
# and the COMPAS system's concordance 63.6%.
# This is lower than the accuracy quoted in the Northpoint study of 68%.


decile_f <- Surv(start, end, event, type="counting") ~ decile_score
dmodel <- coxph(decile_f, data=data)
summary(dmodel)

#COMPAS's decile scores are a bit more accurate at 66%.

# -> We can test if the algorithm is behaving differently across races by
# including a race interaction term in the cox model.


f2 <- Surv(start, end, event, type="counting") ~ race_factor + score_factor + race_factor * score_factor
model <- coxph(f2, data=data)
print(summary(model))

#The interaction term shows a similar disparity as the logistic regression above.

# High risk white defendants are 3.61 more likely than low risk white defendants,
# while High risk black defendants are 2.99 more likely than low

# Black High Hazard: 2.99
# White High Hazard: 3.61
# Black Medium Hazard: 1.95
# White Medium Hazard: 2.32

