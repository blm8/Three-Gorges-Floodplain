###########
## NOTES ##
###########

#Last updated 04 Jan 2019 by BLM


################################
## Test For Normality In Data ##
################################

rm(list=ls())
setwd("~/Desktop/TGR")

#Load the data
TGR_Jun <- read.csv("TGR_Jun.csv", header=TRUE)
TGR_AugDuringRain <- read.csv("TGR_AugDry.csv", header=TRUE)
TGR_AugAfterRain <- read.csv("TGR_AugWet.csv", header=TRUE)
TGR_Summer <- read.csv("TGR_Summer.csv", header=TRUE)
TGR_Jan <- read.csv("TGR_Jan.csv", header=TRUE)
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Diffusive CO2 fluxes #Units are mg CO2 m-2 h-1
y <- TGR_All$FCO2
y.max <- max(y, na.rm = TRUE)
y.min <- min(y, na.rm = TRUE)

qqnorm(y, ylim = c(y.min, y.max), main = expression("Normal" ~ "Q-Q" ~ "Plot" ~ CO[2] ~ (mg ~ m^-2 ~ h^-1)),
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE)
qqline(y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7) #Data non-normal #Bimodal distribution indicated
install.packages("rcompanion")
library(rcompanion)
plotNormalHistogram(y)
#H0:  Samples come from a normal distribution
#HA:  Samples do not come from a normal distribution
shapiro.test(y) #p-value = 1.953e-12 #Reject H0 #Samples do not come from a normal distribution

##=========Try transforming the data using square root, cube root, log, and Tukey's Ladder of Powers=========## 
#From http://rcompanion.org/handbook/I_12.html
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
##=========Try transforming the data using square root, cube root, log, and Tukey's Ladder of Powers=========##

#Diffusive CH4 fluxes #Units are mg CH4 m-2 h-1
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

#Diffusive N2O fluxes #Units are mg N2O m-2 h-1
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

#O2 percent saturation
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


############################################################################
## Kruskall-Wallis Analyses Of Variance To Determine If Differences Exist ##
############################################################################

colnames(TGR_All)

#Need to determine whether variance is equal or unequal (Behrens-Fischer problem) #Use Bartlett's test

#Post hoc pairwise comparisons to determine WHERE sig differences exist #Results in a problematic loss in power and interaction effects #Repeat using parametric methods?

##=========By month comparisons=========##
#Use a Bonferroni correction, or alpha / n of comparisons being made 
0.05/(6) #New alpha = 0.008333333 #No difference in interpretation of monthly comparisons

#FCH4 by month #Units are mg CH4 m-2 h-1
bartlett.test(FCH4 ~ MONTH, data = TGR_Summer) #Unequal variance #Use Welch's t-test rather than post hoc Mann-Whitney-Wilcoxon comparisons? #var.equal = FALSE
kruskal.test(FCH4 ~ MONTH, data = TGR_Summer) #df = k - 1

wilcox.test(TGR_AugDuringRain$FCH4, TGR_AugAfterRain$FCH4) #p-value = 1.228e-14
boxplot(TGR_AugDuringRain$FCH4, TGR_AugAfterRain$FCH4) #Higher FCH4 during rain
wilcox.test(TGR_AugDuringRain$FCH4, TGR_Jun$FCH4) #p-value = 2.277e-14
boxplot(TGR_AugDuringRain$FCH4, TGR_Jun$FCH4) #Higher FCH4 during rain
wilcox.test(TGR_AugAfterRain$FCH4, TGR_Jun$FCH4) #p-value = 0.1639 #Not sig 
wilcox.test(TGR_Jun$FCH4, TGR_Jan$FCH4) #p-value = 5.149e-09
boxplot(TGR_Jun$FCH4, TGR_Jan$FCH4) #Higher FCH4 during Jun
wilcox.test(TGR_AugDuringRain$FCH4, TGR_Jan$FCH4) #p-value < 2.2e-16
boxplot(TGR_AugDuringRain$FCH4, TGR_Jan$FCH4) #Higher FCH4 during Aug
wilcox.test(TGR_AugAfterRain$FCH4, TGR_Jan$FCH4) #p-value = 5.768e-16
boxplot(TGR_AugAfterRain$FCH4, TGR_Jan$FCH4) #Higher FCH4 during Aug

t.test(TGR_AugDuringRain$FCH4, TGR_AugAfterRain$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 9.894e-09
boxplot(TGR_AugDuringRain$FCH4, TGR_AugAfterRain$FCH4) #Higher FCH4 during rain
t.test(TGR_AugDuringRain$FCH4, TGR_Jun$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 3.11e-10
boxplot(TGR_AugDuringRain$FCH4, TGR_Jun$FCH4) #Higher FCH4 during rain
t.test(TGR_AugAfterRain$FCH4, TGR_Jun$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 0.3857 #Not sig 
t.test(TGR_Jun$FCH4, TGR_Jan$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 1.42e-05
boxplot(TGR_Jun$FCH4, TGR_Jan$FCH4) #Higher FCH4 during Jun
t.test(TGR_AugDuringRain$FCH4, TGR_Jan$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 6.708e-15
boxplot(TGR_AugDuringRain$FCH4, TGR_Jan$FCH4) #Higher FCH4 during Aug
t.test(TGR_AugAfterRain$FCH4, TGR_Jan$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 3.81e-06
boxplot(TGR_AugAfterRain$FCH4, TGR_Jan$FCH4) #Higher FCH4 during Aug

#FCO2 by month #Units are mg CO2 m-2 h-1
bartlett.test(FCO2 ~ MONTH, data = TGR_Summer) #Unequal variance 
kruskal.test(FCO2 ~ MONTH, data = TGR_Summer) #df = k - 1

wilcox.test(TGR_AugDuringRain$FCO2, TGR_AugAfterRain$FCO2) #p-value = 0.785 #Not sig
wilcox.test(TGR_AugDuringRain$FCO2, TGR_Jun$FCO2) #p-value = 1.14e-05
boxplot(TGR_AugDuringRain$FCO2, TGR_Jun$FCO2) #Higher FCO2 during Jun
wilcox.test(TGR_AugAfterRain$FCO2, TGR_Jun$FCO2) #p-value = 1.12e-05 
boxplot(TGR_AugAfterRain$FCO2, TGR_Jun$FCO2) #Higher FCO2 during Jun
wilcox.test(TGR_Jun$FCO2, TGR_Jan$FCO2) #p-value = 0.0001571
boxplot(TGR_Jun$FCO2, TGR_Jan$FCO2) #Higher FCO2 during Jun
wilcox.test(TGR_AugDuringRain$FCO2, TGR_Jan$FCO2) #p-value = 0.07114 #Not sig
wilcox.test(TGR_AugAfterRain$FCO2, TGR_Jan$FCO2) #p-value = 0.09358 #Not sig

t.test(TGR_AugDuringRain$FCO2, TGR_AugAfterRain$FCO2, var.equal = FALSE, conf.level = 0.95) #p-value = 0.507 #Not sig
t.test(TGR_AugDuringRain$FCO2, TGR_Jun$FCO2, var.equal = FALSE, conf.level = 0.95) #p-value = 7.642e-06
boxplot(TGR_AugDuringRain$FCO2, TGR_Jun$FCO2) #Higher FCO2 during Jun
t.test(TGR_AugAfterRain$FCO2, TGR_Jun$FCO2, var.equal = FALSE, conf.level = 0.95) #p-value = 2.352e-05 
boxplot(TGR_AugAfterRain$FCO2, TGR_Jun$FCO2) #Higher FCO2 during Jun
t.test(TGR_Jun$FCO2, TGR_Jan$FCO2, var.equal = FALSE, conf.level = 0.95) #p-value = 0.0001261
boxplot(TGR_Jun$FCO2, TGR_Jan$FCO2) #Higher FCO2 during Jun
t.test(TGR_AugDuringRain$FCO2, TGR_Jan$FCO2, var.equal = FALSE, conf.level = 0.95) #p-value = 0.04466 #Not sig
t.test(TGR_AugAfterRain$FCO2, TGR_Jan$FCO2, var.equal = FALSE, conf.level = 0.95) #p-value = 0.1615 #Not sig

#FN2O by month #Units are mg N2O m-2 h-1
bartlett.test(FN2O ~ MONTH, data = TGR_Summer) #Equal variance 
kruskal.test(FN2O ~ MONTH, data = TGR_Summer) #df = k - 1

#O2 percent saturation by month
bartlett.test(O2_PERCENT ~ MONTH, data = TGR_Summer) #Unequal variance 
kruskal.test(O2_PERCENT ~ MONTH, data = TGR_Summer) #df = k - 1

t.test(TGR_AugDuringRain$O2_PERCENT, TGR_AugAfterRain$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 0.08147 #Not sig
t.test(TGR_AugDuringRain$O2_PERCENT, TGR_Jun$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 7.445e-05
boxplot(TGR_AugDuringRain$O2_PERCENT, TGR_Jun$O2_PERCENT) #Higher O2 percent during Jun
t.test(TGR_AugAfterRain$O2_PERCENT, TGR_Jun$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 0.00275 #Not sig
t.test(TGR_Jun$O2_PERCENT, TGR_Jan$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 1.651e-13
boxplot(TGR_Jun$O2_PERCENT, TGR_Jan$O2_PERCENT) #Higher O2 percent during Jan
t.test(TGR_AugDuringRain$O2_PERCENT, TGR_Jan$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value < 2.2e-16
boxplot(TGR_AugDuringRain$O2_PERCENT, TGR_Jan$O2_PERCENT) #Higher O2 percent during Jan
t.test(TGR_AugAfterRain$O2_PERCENT, TGR_Jan$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value < 2.2e-16
boxplot(TGR_AugAfterRain$O2_PERCENT, TGR_Jan$O2_PERCENT) #Higher O2 percent during Jan

#CH4 k by month #Units are cm h-1
bartlett.test(CH4_k ~ MONTH, data = TGR_Summer) #Unequal variance 
kruskal.test(CH4_k ~ MONTH, data = TGR_Summer) #df = k - 1

t.test(TGR_AugDuringRain$CH4_k, TGR_AugAfterRain$CH4_k, var.equal = FALSE, conf.level = 0.95) #p-value = 0.08147 #Not sig
t.test(TGR_AugDuringRain$O2_PERCENT, TGR_Jun$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 7.445e-05
boxplot(TGR_AugDuringRain$O2_PERCENT, TGR_Jun$O2_PERCENT) #Higher O2 percent during Jun
t.test(TGR_AugAfterRain$O2_PERCENT, TGR_Jun$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 0.00275 #Not sig
t.test(TGR_Jun$O2_PERCENT, TGR_Jan$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 1.651e-13
boxplot(TGR_Jun$O2_PERCENT, TGR_Jan$O2_PERCENT) #Higher O2 percent during Jan
t.test(TGR_AugDuringRain$O2_PERCENT, TGR_Jan$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value < 2.2e-16
boxplot(TGR_AugDuringRain$O2_PERCENT, TGR_Jan$O2_PERCENT) #Higher O2 percent during Jan
t.test(TGR_AugAfterRain$O2_PERCENT, TGR_Jan$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value < 2.2e-16
boxplot(TGR_AugAfterRain$O2_PERCENT, TGR_Jan$O2_PERCENT) #Higher O2 percent during Jan

##=========By site comparisons=========##
#Use a Bonferroni correction, or alpha / n of comparisons being made 
0.05/(4) #New alpha = 0.0125 #Little difference in interpretation of monthly comparisons

#FCH4 by site #Units are mg CH4 m-2 h-1
bartlett.test(FCH4 ~ SITE, data = TGR_Summer) #Unequal variance 
kruskal.test(FCH4 ~ SITE, data = TGR_Summer) #df = k - 1

#Subset the data
Jun_NAT <- subset(TGR_Jun, SITE=="NAT")
Jun_NAT
Jun_ENG <- subset(TGR_Summer, SITE=="ENG")
AugDuringRain_NAT <- subset(TGR_AugDuringRain, SITE=="NAT")
AugDuringRain_ENG <- subset(TGR_AugDuringRain, SITE=="ENG")
AugAfterRain_NAT <- subset(TGR_AugAfterRain, SITE=="NAT")
AugAfterRain_ENG <- subset(TGR_AugAfterRain, SITE=="ENG")
Jan_NAT <- subset(TGR_Jan, SITE=="NAT")
Jan_ENG <- subset(TGR_Jan, SITE=="ENG")

t.test(Jun_NAT$FCH4, Jun_ENG$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 0.6693 #Not sig
t.test(AugDuringRain_NAT$FCH4, AugDuringRain_ENG$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 0.6495 #Not sig
t.test(AugAfterRain_NAT$FCH4, AugAfterRain_ENG$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 0.003701
boxplot(AugAfterRain_NAT$FCH4, AugAfterRain_ENG$FCH4) #FCH4 higher in NAT
t.test(Jan_NAT$FCH4, Jan_ENG$FCH4, var.equal = FALSE, conf.level = 0.95) #p-value = 0.0002588
boxplot(Jan_NAT$FCH4, Jan_ENG$FCH4) #FCH4 higher in ENG

#FCO2 by site #Units are mg CO2 m-2 h-1
bartlett.test(FCO2 ~ SITE, data = TGR_Summer) #Equal variance 
kruskal.test(FCO2 ~ SITE, data = TGR_Summer) #df = k - 1

t.test(Jun_NAT$FCO2, Jun_ENG$FCO2, var.equal = TRUE, conf.level = 0.95) #p-value = 0.7112 #Not sig
t.test(AugDuringRain_NAT$FCO2, AugDuringRain_ENG$FCO2, var.equal = TRUE, conf.level = 0.95) #p-value = 0.1495 #Not sig
t.test(AugAfterRain_NAT$FCO2, AugAfterRain_ENG$FCO2, var.equal = TRUE, conf.level = 0.95) #p-value = 0.02405 #Not sig
t.test(Jan_NAT$FCO2, Jan_ENG$FCO2, var.equal = TRUE, conf.level = 0.95) #p-value = 0.7551 #Not sig

#FN2O by site #Units are mg N2O m-2 h-1
bartlett.test(FN2O ~ SITE, data = TGR_Summer) #Uequal variance 
kruskal.test(FN2O ~ SITE, data = TGR_Summer) #df = k - 1

#O2 percent saturation by site
bartlett.test(O2_PERCENT ~ SITE, data = TGR_Summer) #Unequal variance 
kruskal.test(O2_PERCENT ~ SITE, data = TGR_Summer) #df = k - 1

t.test(Jun_NAT$O2_PERCENT, Jun_ENG$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 0.0008203 
boxplot(Jun_NAT$O2_PERCENT, Jun_ENG$O2_PERCENT) #O2 percent saturation higher in natural pond
t.test(AugDuringRain_NAT$O2_PERCENT, AugDuringRain_ENG$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 3.49e-13
boxplot(AugDuringRain_NAT$O2_PERCENT, AugDuringRain_ENG$O2_PERCENT) #O2 percent saturation higher in natural pond
t.test(AugAfterRain_NAT$O2_PERCENT, AugAfterRain_ENG$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value < 2.2e-16
boxplot(AugAfterRain_NAT$O2_PERCENT, AugAfterRain_ENG$O2_PERCENT) #O2 percent saturation higher in natural pond
t.test(Jan_NAT$O2_PERCENT, Jan_ENG$O2_PERCENT, var.equal = FALSE, conf.level = 0.95) #p-value = 0.2285 #Not sig
boxplot(Jan_NAT$O2_PERCENT, Jan_ENG$O2_PERCENT)

#CH4 k by site #Units are cm h-1
bartlett.test(CH4_k ~ SITE, data = TGR_Summer) #Unequal variance 
kruskal.test(CH4_k ~ SITE, data = TGR_Summer) #df = k - 1

##=========By day night comparisons=========##
#Use a Bonferroni correction, or alpha / n of comparisons being made 
0.05/(5) #New alpha = 0.01 #No difference in interpretation of daynight comparisons

#FCH4 by day night #Units are mg CH4 m-2 h-1
bartlett.test(FCH4 ~ DAYNIGHT, data = TGR_Summer) #Unequal variance 
kruskal.test(FCH4 ~ DAYNIGHT, data = TGR_Summer) #df = k - 1
kruskal.test(FCH4 ~ DAYNIGHT, data = TGR_Jun) #df = k - 1
kruskal.test(FCH4 ~ DAYNIGHT, data = TGR_AugDuringRain) #df = k - 1
kruskal.test(FCH4 ~ DAYNIGHT, data = TGR_AugAfterRain) #df = k - 1
kruskal.test(FCH4 ~ DAYNIGHT, data = TGR_Jan) #df = k - 1

#FCO2 by day night #Units are mg CO2 m-2 h-1
bartlett.test(FCO2 ~ DAYNIGHT, data = TGR_Summer) #Equal variance 
kruskal.test(FCO2 ~ DAYNIGHT, data = TGR_Summer) #df = k - 1
kruskal.test(FCO2 ~ DAYNIGHT, data = TGR_Jun) #df = k - 1
kruskal.test(FCO2 ~ DAYNIGHT, data = TGR_AugDuringRain) #df = k - 1
kruskal.test(FCO2 ~ DAYNIGHT, data = TGR_AugAfterRain) #df = k - 1
kruskal.test(FCO2 ~ DAYNIGHT, data = TGR_Jan) #df = k - 1

#FN2O by day night #Units are mg N2O m-2 h-1
bartlett.test(FN2O ~ DAYNIGHT, data = TGR_Summer) #Equal variance 
kruskal.test(FN2O ~ DAYNIGHT, data = TGR_Summer) #df = k - 1
kruskal.test(FN2O ~ DAYNIGHT, data = TGR_Jun) #df = k - 1
kruskal.test(FN2O ~ DAYNIGHT, data = TGR_AugDuringRain) #df = k - 1
kruskal.test(FN2O ~ DAYNIGHT, data = TGR_AugAfterRain) #df = k - 1
kruskal.test(FN2O ~ DAYNIGHT, data = TGR_Jan) #df = k - 1

#O2 percent saturation by day night
bartlett.test(O2_PERCENT ~ DAYNIGHT, data = TGR_Summer) #Unequal variance 
kruskal.test(O2_PERCENT ~ DAYNIGHT, data = TGR_Summer) #df = k - 1
boxplot(O2_PERCENT ~ DAYNIGHT, data = TGR_Summer) #O2 percent saturation higher during day
kruskal.test(O2_PERCENT ~ DAYNIGHT, data = TGR_Jun) #df = k - 1
boxplot(O2_PERCENT ~ DAYNIGHT, data = TGR_Jun) #O2 percent saturation higher during day
kruskal.test(O2_PERCENT ~ DAYNIGHT, data = TGR_AugDuringRain) #df = k - 1
boxplot(O2_PERCENT ~ DAYNIGHT, data = TGR_AugDuringRain) #O2 percent saturation higher during day
kruskal.test(O2_PERCENT ~ DAYNIGHT, data = TGR_AugAfterRain) #df = k - 1 #Less significant; more respiration happeneing following terrestrial-aquatic transfer?
boxplot(O2_PERCENT ~ DAYNIGHT, data = TGR_AugAfterRain) #O2 percent saturation higher during day
kruskal.test(O2_PERCENT ~ DAYNIGHT, data = TGR_Jan) #df = k - 1
boxplot(O2_PERCENT ~ DAYNIGHT, data = TGR_Jan) #O2 percent saturation higher during day

#CH4 k by day night #Units are cm h-1
bartlett.test(CH4_k ~ DAYNIGHT, data = TGR_Summer) #Equal variance 
kruskal.test(CH4_k ~ DAYNIGHT, data = TGR_Summer) #df = k - 1
kruskal.test(CH4_k ~ DAYNIGHT, data = TGR_Jun) #df = k - 1
kruskal.test(CH4_k ~ DAYNIGHT, data = TGR_AugDuringRain) #df = k - 1
kruskal.test(CH4_k ~ DAYNIGHT, data = TGR_AugAfterRain) #df = k - 1
kruskal.test(CH4_k ~ DAYNIGHT, data = TGR_Jan) #df = k - 1

##=========By method comparisons=========##
#Use a Bonferroni correction, or alpha / n of comparisons being made 
0.05/(3) #New alpha = 0.01666667 #No difference in interpretation of monthly comparisons

#FCH4 #Units are mg CH4 m-2 h-1
wilcox.test(TGR_All$FCH4, TGR_All$TBLCH4) #p-value = 0.01477
boxplot(TGR_All$FCH4, TGR_All$TBLCH4) #TBL overestimates flux relative to floating chambers

#FCO2 #Units are mg CO2 m-2 h-1
wilcox.test(TGR_All$FCO2, TGR_All$TBLCO2) #Not sig
boxplot(TGR_All$FCO2, TGR_All$TBLCO2) 

#N2O #Units are mg N2O m-2 h-1
wilcox.test(TGR_All$FN2O, TGR_All$TBLN2O) #p-value < 2.2e-16
boxplot(TGR_All$FN2O, TGR_All$TBLN2O) #TBL overestimates flux relative to floating chambers

