###############################################################################
## Table 2. Mean diffusive fluxes and partial pressures for CH4, CO2 and N2O ##
###############################################################################

rm(list=ls()) # Clear your workspace, manually select your working directory

TGR_All <- read.csv("TGR_All.csv", header=TRUE) # Load your data
colnames(TGR_All)

# Use a Bonferroni correction, or alpha / n of comparisons being made 
0.05/(2) # New alpha=0.025 for by-site comparisons
0.05/(4) # New alpha=0.013 for by-site and by-month comparisons

levels(TGR_All$MONTH) # Subset data by month and by site
levels(TGR_All$SITE)
Jun <- subset(TGR_All, MONTH=="Jun")
Jun_Nat <- subset(TGR_All, MONTH=="Jun" & SITE=="NAT")
Jun_Eng <- subset(TGR_All, MONTH=="Jun" & SITE=="ENG")
AugWet <- subset(TGR_All, MONTH=="AugDry")
AugWet_Nat <- subset(TGR_All, MONTH=="AugDry" & SITE=="NAT")
AugWet_Eng <- subset(TGR_All, MONTH=="AugDry" & SITE=="ENG")
AugDry <- subset(TGR_All, MONTH=="AugWet")
AugDry_Nat <- subset(TGR_All, MONTH=="AugWet" & SITE=="NAT")
AugDry_Eng <- subset(TGR_All, MONTH=="AugWet" & SITE=="ENG")
Jan <- subset(TGR_All, MONTH=="Jan")
Jan_Nat <- subset(TGR_All, MONTH=="Jan" & SITE=="NAT")
Jan_Eng <- subset(TGR_All, MONTH=="Jan" & SITE=="ENG")

# install.packages("PairedData") # Visualize the change and directionality in data
library(PairedData) # Visualize the change and directionality in data
before <- subset(TGR_All, MONTH=="AugWet", PCO2, drop=TRUE)
after <- subset(TGR_All, MONTH=="Jan", PCO2, drop=TRUE)
pd <- paired(before, after)
pd
plot(pd, type = "profile") + theme_bw()

Table2 <- function(x) { # Calculate means and standard errors
  n <- length(na.omit(x))
  Mean <- mean(na.omit(x))
  SE <- sd(na.omit(x))/sqrt(length(na.omit(x))) 
  Results <- list(n, Mean, SE)
  return(Results)
}

d <- function(x,y) { # Calculate Cohen's d for effect size
  xM <- mean(na.omit(x))
  yM <- mean(na.omit(y))
  sdx <- sd(na.omit(x))
  sdy <- sd(na.omit(y))
  SD <- sqrt(((sdx^2)+(sdy^2))/2)
  d <- (xM-yM)/SD
  return(d)
}

Table2(AugDry$EBULL)
Table2(Jun_Nat$FCH4)
Table2(Jun_Eng$FCH4)
?wilcox.test()
wilcox.test(na.omit(TGR_All$FCH4), na.omit(Jan$O2_CONC, paired=F)) 
d(na.omit(AugDry$O2_CONC), na.omit(Jan$O2_CONC)) 

# Linear regressions

rm(list=ls()) # Clear your workspace, manually select your working directory

TGR_All <- read.csv("TGR_All.csv", header=TRUE) # Load your data

TGR_All <- subset(TGR_All, select = c("PCH4", "FCH4"))
colnames(TGR_All)

# Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All)) # Gives the index of NA values in a vector
nrow(TGR_All)
TGR_All <- na.omit(TGR_All)
nrow(TGR_All)
which(is.na(TGR_All))
length(TGR_All$FCH4) 
summary(lm(na.omit(TGR_All$FN2O)~na.omit(TGR_All$PCH4))) 
cor.test(TGR_All$PCH4, TGR_All$FCH4, method="pearson")

