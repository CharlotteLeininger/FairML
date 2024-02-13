###Descriptive plots: 

### Proof Kleinberg Plot: 

#Group 1: African-Americans:
k1 <- sum(recid_af[[1]])    #positive cases (Recidivist) Group 1 (African-Americans)
N1 <- length(recid_af[[1]]) #all cases Group 1

#Group 2: White
k2 <- sum(recid_wh[[1]])    #positive cases (Recidivist) Group 2 (White)
N2 <- length(recid_wh[[1]]) #all cases Group 2

#Kleinberg's linear connection between base rates (k/N), score for negative class & positive class
f_1 <- function(x) (k1/N1)/(1-(k1/N1))*(1-x)  
f_2 <- function(x) (k2/N2)/(1-(k2/N2))*(1-x)

ggplot() +
  geom_function(aes(color = "f_1"), fun = f_1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_2"), fun = f_2, size = 1.5, linetype = 2) +
  # geom_point(aes(x=avp_af, y=avn_af), colour="black", size = 4) +
  # geom_point(aes(x=avp_wh, y=avn_wh), colour="red", size = 4) +
  scale_color_manual(values = c("f_1" = "black", "f_2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_1" = "African-American", "f_2" = "White")) +
  theme_bw(base_size = 12) +
  xlab("x = Average Score For Positive Class") +
  ylab("y = Average Score For Negative Class") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Kleinberg_Plot1.jpg")

### Proof Chouldechova Plot: 

#Base rates of group 1 and 2:
p1 <- k1/N1       #African-Americans
p2 <- k2/N2       #Whites

#COMPAS fairness metrics calculated:
#FPR of African-Americans and Whites:
FPR_af <- length(subset(FP_af[[1]],FP_af[[1]] == "HighScore"))/length(FP_af[[1]])
#0.4233818
FPR_wh <- length(subset(FP_wh[[1]],FP_wh[[1]] == "HighScore"))/length(FP_wh[[1]])
# 0.2201405

#FNR of African-Americans and Whites:
FNR_af <- length(subset(FN_af[[1]],FN_af[[1]] == "LowScore"))/length(FN_af[[1]])
# 0.2847682
FNR_wh <- length(subset(FN_wh[[1]],FN_wh[[1]] == "LowScore"))/length(FN_wh[[1]])
# 0.4963504

# Positive predicitve values (PPV) of African-Americans and Whites:
PPV_af <- length(subset(PV_af[[1]],PV_af[[1]] == 1))/length(PV_af[[1]])
PPV_wh <- length(subset(PV_wh[[1]],PV_wh[[1]] == 1))/length(PV_wh[[1]])

#Chouldechova's linear connection between base rates (p), PPV, FPR and FNR
f_c1 <-  function(FNR) (p1/(1-p1))*((1-PPV_af)/PPV_af)*(1-FNR) # FPR1 = 0.722 * (1 - FNR1)
f_c2 <-  function(FNR) (p2/(1-p2))*((1-PPV_wh)/PPV_wh)*(1-FNR) # FPR2 = 0.462 * (1 - FNR2)

#functions for FNRs: (inverted functions)
f_FNR1 <- function(FPR) 1 - ((1 - p1)/p1)*(PPV_af/(1 - PPV_af))*FPR
f_FNR2 <- function(FPR) 1 - ((1 - p2)/p2)*(PPV_wh/(1 - PPV_wh))*FPR

ggplot() +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1" = "African-American", "f_c2" = "White")) +
  geom_point(aes(x=FNR_af, y=FPR_af), colour="black", size = 4) +     #adding FNR and FPR of COMPAS for African-Americans
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="#3366FF", size = 4) +   #adding FNR and FPR of COMPAS for Whites
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Chouldechova_Plot.jpg")

###############################################################################################
##Plots: Fulfilling 2 fairness metrics 

##Adjust FPR White:
ggplot() +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1" = "African-American", "f_c2" = "White")) +
  geom_point(aes(x=FNR_af, y=FPR_af), colour="black", size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="#3366FF", size = 4) +
  geom_point(aes(x=FNR_af, y=f_c2(FNR_af)), colour = "#3366FF", size = 4, pch=21) +
  geom_vline(xintercept=FNR_af, linetype="dotted") +
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Chouldechova_FPR.jpg")

##Adjust FPR Black:
ggplot() +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1" = "African-American", "f_c2" = "White")) +
  geom_point(aes(x=FNR_af, y=FPR_af), colour="black", size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="#3366FF", size = 4) +
  geom_point(aes(x=FNR_wh, y=f_c1(FNR_wh)), colour = "black", size = 4, pch=21) +
  geom_vline(xintercept=FNR_wh, linetype="dotted") +
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Chouldechova_FPR2.jpg")

##Adjusted FNR black: 
ggplot() +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1" = "African-American", "f_c2" = "White")) +
  geom_point(aes(x=FNR_af, y=FPR_af), colour="black", size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="#3366FF", size = 4) +
  geom_hline(yintercept=FPR_wh, linetype="dotted") + # adjust FPR of black
  geom_point(aes(x=f_FNR1(FPR_wh), y = FPR_wh), colour = "black", size = 4, pch=21) + # Black: new FNR = 0.628
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Chouldechova_FNR.jpg")

##Adjusted FNR white: 
ggplot() +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1" = "African-American", "f_c2" = "White")) +
  geom_point(aes(x=FNR_af, y=FPR_af), colour="black", size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="#3366FF", size = 4) +
  geom_hline(yintercept=FPR_af, linetype="dotted") + # adjust FPR of white
  geom_point(aes(x=f_FNR2(FPR_af), y = FPR_af), colour = "black", size = 4, pch=21) + # White: new FNR = 0.031
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Chouldechova_FNR2.jpg")


#Adjust PPV: 
f_adjustPPV_upper <- function(FNR) (p1/(1-p1))*((1-(PPV_af+0.125))/(PPV_af+0.125))*(1-FNR)
f_adjustPPV_lower <- function(FNR) (p1/(1-p1))*((1-(PPV_af-0.125))/(PPV_af-0.125))*(1-FNR)

PPV_data <- data.frame(x=seq(0,1, 0.1))
PPV_data[[2]]  <- sapply(PPV_data$x, FUN = function(FNR) (p1/(1-p1))*((1-(PPV_af+0.1))/(PPV_af+0.1))*(1-FNR))
PPV_data[[3]] <- sapply(PPV_data$x, FUN = function(FNR) (p1/(1-p1))*((1-(PPV_af-0.1))/(PPV_af-0.1))*(1-FNR))

ggplot(PPV_data, aes(x=x, y = V3)) +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1" = "African-American", "f_c2" = "White")) +
  geom_point(aes(x=FNR_af, y=FPR_af), colour="black", size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="#3366FF", size = 4) +
  geom_line(aes(y = V2)) + 
  geom_line(aes(y = V3)) + 
  geom_ribbon(aes(ymin = V3, ymax = V2), fill = "darkgrey", alpha = .5) +
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Chouldechova_PPV.jpg")

