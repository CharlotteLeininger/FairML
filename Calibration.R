library(tidyr)
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

vector_score_pos1 <- vector()
for (i in 1:10) {
  vector_score_pos1[[i]] <- Score_x_af[[i]][2,2]*Score_af[[i]]
}
avp_af <- sum(vector_score_pos1)/k1


vector_score_neg1 <- vector()
for (i in 1:10) {
  vector_score_neg1[[i]] <- Score_x_af[[i]][1,2]*Score_af[[i]]
}
avn_af <- sum(vector_score_neg1)/(N1-k1)



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

vector_score_pos2 <- vector()
for (i in 1:10) {
  vector_score_pos2[[i]] <- Score_x_wh[[i]][2,2]*Score_wh[[i]]
}
avp_wh <- sum(vector_score_pos2)/k2


vector_score_neg2 <- vector()
for (i in 1:10) {
  vector_score_neg2[[i]] <- Score_x_wh[[i]][1,2]*Score_wh[[i]]
}
avn_wh <- sum(vector_score_neg2)/(N2-k2)

Calibration <- as.data.frame(cbind("Score"= 1:10, Score_af, Score_wh))
Calibration_long <- pivot_longer(Calibration, cols = c(Score_af, Score_wh),
                                 names_to = "Group", values_to = "v_b")

ggplot(data = Calibration_long, aes(x = factor(Score), y = v_b, fill = factor(Group))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = c("black", "#3366FF"), labels = c("African-American", "White")) +  # Farben anpassen
  labs(x = "Score", y = "Positive Cases", fill = "Group") +
  theme_bw(12)


ggplot(Calibration_long, aes(x = Score, y = v_b, colour = Group, linetype = Group, size = Group)) +
  geom_line() +  # Linien dicker und beide Ästhetiken auf Group setzen
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("black", "#3366FF"), labels = c("African-American", "White")) +
  scale_linetype_manual(values = c(1, 2), labels = c("African-American", "White")) +
  scale_size_manual(values = c(1.5, 1), labels = c("African-American", "White")) +
  labs(x = "Score", y = "Positive Cases %", linetype = "Group") +
  theme_bw(12) +
  theme(legend.position = c(0.8, 0.2),
        legend.background = element_rect(colour = "black", size = 0.3)) +  # Rahmen um die Legende hinzufügen
  scale_x_continuous(breaks = seq(1,10))
ggsave("Calibration_Plot1.jpg")

### Calibration threshold: Compas
ggplot(Calibration_long, aes(x = Score, y = v_b, colour = Group, linetype = Group, size = Group)) +
  geom_line() +  # Linien dicker und beide Ästhetiken auf Group setzen
  geom_point(size = 2.5) +
  geom_vline(xintercept=5, linetype="dotted") +
  scale_colour_manual(values = c("black", "#3366FF"), labels = c("African-American", "White")) +
  scale_linetype_manual(values = c(1, 2), labels = c("African-American", "White")) +
  scale_size_manual(values = c(1.5, 1), labels = c("African-American", "White")) +
  labs(x = "Score", y = "Positive Cases %", linetype = "Group") +
  theme_bw(12) +
  theme(legend.position = c(0.8, 0.2),
        legend.background = element_rect(colour = "black", size = 0.3)) +  # Rahmen um die Legende hinzufügen
  scale_x_continuous(breaks = seq(1,10))

Calibration_long_area <- subset(Calibration_long, Score > 4)

## Calibration: Connection to Predicitve Parity
ggplot() +
  geom_line(data = Calibration_long, aes(x = Score, y = v_b, colour = Group, linetype = Group, size = Group)) +
  geom_point(data = Calibration_long, aes(x = Score, y = v_b, colour = Group), size = 2.5) +
  geom_vline(xintercept = 5, linetype = "dotted") +
  geom_area(data = Calibration_long_area, aes(x = Score, y = v_b, fill = Group), alpha = 0.2, position = "identity") +
  scale_colour_manual(values = c("black", "#3366FF"), labels = c("African-American", "White")) +
  scale_fill_manual(values = c("black", "#3366FF"), labels = c("African-American", "White")) +  # Lighter, transparent colors
  scale_linetype_manual(values = c(1, 2), labels = c("African-American", "White")) +
  scale_size_manual(values = c(1.5, 1), labels = c("African-American", "White")) +
  labs(x = "Score", y = "Positive Cases %", linetype = "Group", fill = "Group") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        legend.background = element_rect(colour = "black", size = 0.3)) +  # Add frame around the legend
  scale_x_continuous(breaks = seq(1, 10))
