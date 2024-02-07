### Proof Kleinberg Plot: 


k1 <- sum(recid_af[[1]])
N1 <- length(recid_af[[1]])

k2 <- sum(recid_wh[[1]])
N2 <- length(recid_wh[[1]])

x <- seq(from =0, to =1, by = 0.1)
y1 <- 1-((1-k1/N1)/(k1/N1))*x
y2 <- 1-((1-k2/N2)/(k2/N2))*x

f1 <- function(x) 1-((1-k1/N1)/(k1/N1))*x
f2 <- function(x) 1-((1-k2/N2)/(k2/N2))*x

plot(x,y1, type = "l", col="green", lwd = 5)
lines(x,y2, col="red", lwd = 5)


ggplot() +
  geom_function(aes(color = "f1"), fun = f1, size = 1.5) +
  geom_function(aes(color = "f2"), fun = f2, size = 1.5) +
  scale_color_manual(values = c("f1" = "black", "f2" = "red"),
                     name = NULL,
                     labels = c("f1" = "Gruppe 1", "f2" = "Gruppe 2")) +
  theme_bw(base_size = 12) +
  xlab("x") +
  ylab(expression(gamma))


###Alternative: 
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


p1 <- k1/N1
p2 <- k2/N2

FPR_af <- length(subset(FP_af[[1]],FP_af[[1]] == "HighScore"))/length(FP_af[[1]])
FPR_wh <- length(subset(FP_wh[[1]],FP_wh[[1]] == "HighScore"))/length(FP_wh[[1]])

FNR_af <- length(subset(FN_af[[1]],FN_af[[1]] == "LowScore"))/length(FN_af[[1]])
FNR_wh <- length(subset(FN_wh[[1]],FN_wh[[1]] == "LowScore"))/length(FN_wh[[1]])

PPV_af <- length(subset(PV_af[[1]],PV_af[[1]] == 1))/length(PV_af[[1]])
PPV_wh <- length(subset(PV_wh[[1]],PV_wh[[1]] == 1))/length(PV_wh[[1]])

f_c1 <-  function(FNR) (p1/(1-p1))*((1-PPV_af)/PPV_af)*(1-FNR)
f_c2 <-  function(FNR) (p2/(1-p2))*((1-PPV_wh)/PPV_wh)*(1-FNR)

ggplot() +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1" = "African-American", "f_c2" = "White")) +
  geom_point(aes(x=FNR_af, y=FPR_af), colour="black", size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="#3366FF", size = 4) +
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Chouldechova_Plot.jpg")

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

f_adjustPPV_upper <- function(FNR) (p1/(1-p1))*((1-(PPV_af+0.125))/(PPV_af+0.125))*(1-FNR)
f_adjustPPV_lower <- function(FNR) (p1/(1-p1))*((1-(PPV_af-0.125))/(PPV_af-0.125))*(1-FNR)

PPV_data <-   data.frame(x=seq(0,1, 0.1))
PPV_data[[2]]  <- sapply(PPV_data$x, FUN = function(FNR) (p1/(1-p1))*((1-(PPV_af+0.1))/(PPV_af+0.1))*(1-FNR))
PPV_data[[3]] <- sapply(PPV_data$x, FUN = function(FNR) (p1/(1-p1))*((1-(PPV_af-0.1))/(PPV_af-0.1))*(1-FNR))

ggplot(PPV_data, aes(x=x, y = V3)) +
  geom_line(aes(y = V2)) + 
  geom_line(aes(y = V3)) + 
  geom_ribbon(aes(ymin = V3, ymax = V2), fill = "grey", alpha = .5)

#Adjust PPV: 
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




##Adjusted FNR: 
ggplot() +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5, linetype = 1) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5, linetype = 2) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "#3366FF"),
                     name = NULL,
                     labels = c("f_c1" = "African-American", "f_c2" = "White")) +
  geom_point(aes(x=FNR_af, y=FPR_af), colour="black", size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh), colour="#3366FF", size = 4) +
  geom_point(aes(x=0.64, y = FPR_wh), colour = "black", size = 4, pch=21) +
  geom_hline(yintercept=FPR_wh, linetype="dotted") +
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_rect(colour = "black", size = 0.3))
ggsave("Chouldechova_FNR.jpg")


ggplot() +
  geom_function(aes(color = "f_c1"), fun = f_c1, size = 1.5) +
  geom_function(aes(color = "f_c2"), fun = f_c2, size = 1.5) +
  scale_color_manual(values = c("f_c1" = "black", "f_c2" = "red"),
                     name = NULL,
                     labels = c("f_c1" = "Group 1", "f_c2" = "Group 2")) +
  geom_point(aes(x=FNR_af, y=FPR_af, color="FNR & FPR for COMPAS"), size = 4) +
  geom_point(aes(x=FNR_wh, y=FPR_wh, color="FNR & FPR for COMPAS"), size = 4) +
  scale_color_manual(values = c("FNR & FPR for COMPAS" = "black"),
                     name = NULL,
                     labels = c("FNR & FPR for COMPAS")) +
  theme_bw(base_size = 12) +
  xlab("FNR") +
  ylab("FPR") +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  theme(legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        legend.key = element_blank()) +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, 
                              label.position = "bottom", label.hjust = 0.5, 
                              override.aes = list(size = 3)))
