################################
## Test For Normality In Data ##
################################

rm(list=ls())

# Load the data
TGR_Jun <- read.csv("TGR_Jun.csv", header=TRUE)
TGR_AugDuringRain <- read.csv("TGR_AugDry.csv", header=TRUE)
TGR_AugAfterRain <- read.csv("TGR_AugWet.csv", header=TRUE)
TGR_Summer <- read.csv("TGR_Summer.csv", header=TRUE)
TGR_Jan <- read.csv("TGR_Jan.csv", header=TRUE)
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Diffusive CO2 fluxes # Units are mg CO2 m-2 h-1
y <- TGR_All$FCO2
y.max <- max(y, na.rm = TRUE)
y.min <- min(y, na.rm = TRUE)

qqnorm(y, ylim = c(y.min, y.max), main = expression("Normal" ~ "Q-Q" ~ "Plot" ~ CO[2] ~ (mg ~ m^-2 ~ h^-1)),
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7) # Data non-normal #Bimodal distribution indicated
install.packages("rcompanion")
library(rcompanion)
plotNormalHistogram(y)
#H0:  Samples come from a normal distribution
#HA:  Samples do not come from a normal distribution
shapiro.test(y) #p-value = 1.953e-12 #Reject H0 #Samples do not come from a normal distribution

# Try transforming the data using square root, cube root, log, and Tukey's Ladder of Powers
# From http://rcompanion.org/handbook/I_12.html
plotNormalHistogram(y)
shapiro.test(y)
y_sqrt <- sqrt(y)
plotNormalHistogram(y_sqrt)
shapiro.test(y_sqrt)
y_cube <- sign(y)*abs(y)^(1/3)
plotNormalHistogram(y_cube)
shapiro.test(y_cube)
y_log <- log(y)
plotNormalHistogram(y_log)
shapiro.test(y_log)
y_Tuk <- transformTukey(y, plotit = FALSE)
plotNormalHistogram(y_Tuk)
shapiro.test(y_Tuk)

# Diffusive CH4 fluxes #Units are mg CH4 m-2 h-1
y <- TGR_All$FCH4
y.max <- max(y, na.rm = TRUE)
y.min <- min(y, na.rm = TRUE)

qqnorm(y, ylim = c(y.min, y.max), main = expression("Normal" ~ "Q-Q" ~ "Plot" ~ CH[4] ~ (mg ~ m^-2 ~ h^-1)),
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7) #Data non-normal #Right-skewed distribution indicated     
plotNormalHistogram(y)
shapiro.test(y) #p-value < 2.2e-16 Reject H0 #Samples do not come from a normal distribution
y_cube <- sign(y)*abs(y)^(1/3)
plotNormalHistogram(y_cube)
shapiro.test(y_cube)

y <- TGR_All$CH4_k
y.max <- max(y, na.rm = TRUE)
y.min <- min(y, na.rm = TRUE)

qqnorm(y, ylim = c(y.min, y.max), main = expression("Normal" ~ "Q-Q" ~ "Plot" ~ CH[4] ~ k[T] ~ (cm ~ h^-1)),
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7) #Data non-normal #Bimodal distribution indicated     
plotNormalHistogram(y)
shapiro.test(y) #p-value < 2.2e-16 Reject H0 #Samples do not come from a normal distribution

# Diffusive N2O fluxes #Units are mg N2O m-2 h-1
y <- TGR_All$FN2O
y.max <- max(y, na.rm = TRUE)
y.min <- min(y, na.rm = TRUE)

qqnorm(y, ylim = c(y.min, y.max), main = expression("Normal" ~ "Q-Q" ~ "Plot" ~ "N"[2]*"O" ~ (mg ~ m^-2 ~ h^-1)),
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7) #Data non-normal #Heavy-tailed distribution indicated     
plotNormalHistogram(y)
shapiro.test(y) #p-value < 2.2e-16 Reject H0 #Samples do not come from a normal distribution
y_sqrt <- sqrt(y)
plotNormalHistogram(y_sqrt)
shapiro.test(y_sqrt)

# O2 percent saturation
y <- TGR_All$O2_PERCENT
y.max <- max(y, na.rm = TRUE)
y.min <- min(y, na.rm = TRUE)

qqnorm(y, ylim = c(y.min, y.max), main = expression("Normal" ~ "Q-Q" ~ "Plot" ~ O[2] ~ "Percent" ~ "Saturation"),
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7) #Data non-normal #Bimodal distribution indicated     
plotNormalHistogram(y)
shapiro.test(y) #p-value < 2.2e-16 Reject H0 #Samples do not come from a normal distribution
y_cube <- sign(y)*abs(y)^(1/3)
plotNormalHistogram(y_cube)
shapiro.test(y_cube)
