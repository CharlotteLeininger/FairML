##### Requirements:

## 1. Run ProPublica_Analysis.R to get the Compas dataframe "df"
## 2. Run "FairnessMetrics.R" for fairness metrics values used in the following plots
## 3. Run this code

########################################################################################################
######################### The approximate theroem: by Bell et al.
########################################################################################################

### Replicating the fairness region from Bell et al. using the COMPAS data: 

p1 <- sum(recid_af[[1]])/length(recid_af[[1]]) #prevalence for African-Americans = 0.52
p2 <- sum(recid_wh[[1]])/length(recid_wh[[1]]) #prevalence for Whites = 0.39

# Epsilon prevalence difference for COMPAS:
e_p <- p1 - p2 # = 0.13

# Allowed epsilon difference for approximate fairness metrics: epsilon <= 0.05
e_FNR <- seq(-0.05, 0.05, by = 0.01)
e_FPR <- seq(-0.05, 0.05, by = 0.01)
e_ACC <- seq(-0.05, 0.05, by = 0.01)


# Bell et al.'s connection of FPR, FNR, ACC and p: 
# (They used this new connection since it is easier to implement with ACC than with PPV as in Chouldechova's connection)
f_FPR <- function(FNR, e_FNR, e_FPR, e_ACC) {
  FNR + e_FNR - e_FPR + (1/e_p)*(e_FNR*p1 + e_FPR - e_FPR*p1 - e_ACC)
}

## Plotting the fairness region by calculating all possible combinations of the epsilon values 
## using a 0.01 margin within the epsilon range:

# Create a data frame with all combinations of e_FNR, e_FPR, and e_ACC
grid <- expand.grid(FNR = seq(0, 1, by = 0.01), e_FNR = e_FNR, e_FPR = e_FPR, e_ACC = e_ACC)


# Evaluate the function for all combinations of FNR, e_FNR, e_FPR, and e_ACC
grid$f_FPR <- with(grid, f_FPR(FNR = FNR, e_FNR = e_FNR, e_FPR = e_FPR, e_ACC = e_ACC))

# Limit data to FPR values between 0 and 1 since others not reasonable:
grid_data <- grid %>% 
  filter(f_FPR >= 0 & f_FPR <= 1)

# Plot the region:
ggplot(grid_data, aes(x = FNR, y = f_FPR)) +
  geom_point(color = "grey60",position = position_jitter(width = 0, height = 0)) +
  theme_bw(base_size = 20) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  labs(x = "FNR", y = "FPR")
# ggsave("FairnessRegion.jpg",  width = 8, height = 5)


#######################################################################################
##### Trying to plot the fairness region using PPV instead of ACC (using Chouldechova's connection):
#######################################################################################

f <- e_FPR
n <- e_FNR
u <- seq(-0.05, 0.05, by = 0.01)
e <- e_p
p <- p2
v <- length(subset(PV_wh[[1]],PV_wh[[1]] == 1))/length(PV_wh[[1]]) # = PPV of White


f_FPR_PPV <- function(b, n, f, u) { (sqrt((b*(p^2)*u - b*p*u + e*b*p*u - e*b*u - e*b*v + e*b -
                                             f*(p^2)*u - e*f*p*u + 2*f*p*u + e*f*u -
             f*u + n*(p^2)*u + n*(p^2)*v - n*(p^2) + e*n*p*u - n*p*u + e*n*p*v - n*p*v - e*n*p + n*p - 
             e*n*u - e*n*v + e*n - (p^2)*u - e*p*u + p*u + e*u + e*v - e)^2 - 4*((p^2)*(-u) - e*p*u +
            2*p*u + e*u - u)*((-b)*f*(p^2)*v + b*f*(p^2) + b*f*p*v - e*b*f*p*v - b*f*p + e*b*f*p + f*(p^2)*v -
            f*(p^2) + e*f*p*v - f*p*v - e*f*p + f*p)) - b*(p^2)*u + b*p*u - e*b*p*u + e*b*u + e*b*v -
       e*b + f*(p^2)*u + e*f*p*u - 2*f*p*u - e*f*u + f*u - n*(p^2)*u - n*(p^2)*v + n*(p^2) - e*n*p*u + n*p*u -
       e*n*p*v + n*p*v + e*n*p - n*p + e*n*u + e*n*v - e*n + (p^2)*u + e*p*u - p*u - e*u - e*v + e)/(2*((p^2)*(-u) -
      e*p*u + 2*p*u + e*u - u))
}


# Create a data frame with all combinations of e_FNR, e_FPR, and e_ACC
grid_p <- expand.grid(FNR = seq(0, 1, by = 0.01), e_FNR = n, e_FPR = f, e_PPV = u)

# Evaluate the function for all combinations of FNR, e_FNR, e_FPR, and e_ACC
grid_p$f_FPR <- with(grid_p, f_FPR_PPV(b = FNR, n = e_FNR, f = e_FPR, u = e_PPV))

plot_grid_p <- grid_p %>% 
  filter(f_FPR >= 0 & f_FPR <= 1)

# Plot the fairness region: 
ggplot(plot_grid_p, aes(x = FNR, y = f_FPR)) +
  geom_point(color = "grey50") +
  theme_bw(base_size = 16) +
  xlim(c(0,1)) +
  ylim(c(0,1)) +
  labs(
    x = "FNR", y = "FPR")

##Does not produce nice results, maybe therefore not mentioned by Bell et al. 
