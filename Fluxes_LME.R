###########################################################
## FCH4 Linear Mixed Effects Modeling - Floodplain Ponds ##
###########################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
# install.packages("lme4") #For mixed effects function
library(lme4)
# install.packages("fmsb") #For VIF function
library(fmsb)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FCH4", "AOU", "PRECIP", "WATER_T", "CHL", "O2_CONC", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # FCH4 in mg CH4 m-2 h-1 
    # AOU in mg O2 L-1 
    # PRECIP is 1=y 2=n 
    # WATER_T in deg C 
    # CHL in ug chla L-1 
    # O2_CONC in mg O2 L-1 
    # COND in mS cm-1

# Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) # Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FCH4)  

# Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$FCH4)  

#Null model
model1 <- lmer(FCH4 ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
coef(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc1  

# Terrestrial-aquatic coupling model
# Check for multi-collinearity
# VIF(lm(COND ~ PH + PRECIP, data = TGR_All_24h)) # If VIF > 5, multi-collinearity strongly suspected  
# VIF(lm(PH ~ COND + PRECIP, data = TGR_All_24h)) 
# VIF(lm(PRECIP ~ PH + COND, data = TGR_All_24h))  
# cor(TGR_All_24h$COND, TGR_All_24h$PH)  
# cor(TGR_All_24h$COND, TGR_All_24h$PRECIP)  
# cor(TGR_All_24h$PRECIP, TGR_All_24h$PH)  

# model2 <- lmer(FCH4 ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) +(0 + PH|MONTH), data = TGR_All_24h)
# logLik(model2)
# coef(model2)
# NLL <- logLik(model2) * -1
# NLL 
# npar <- 13
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC  
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(FCH4 ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
coef(model3)
NLL <- logLik(model3) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc3 

# In-situ production model-b
# Check for multi-collinearity
VIF(lm(HR_SUNRISE ~ WATER_T + O2_CONC + CHL + PH, data = TGR_All_24h)) # If VIF > 5, multi-collinearity strongly suspected # 1.595174
VIF(lm(WATER_T ~ HR_SUNRISE + O2_CONC + CHL + PH, data = TGR_All_24h)) # 2.033396
VIF(lm(O2_CONC ~ HR_SUNRISE + WATER_T + CHL + PH, data = TGR_All_24h)) # 4.310361
VIF(lm(CHL ~ HR_SUNRISE + WATER_T + O2_CONC + PH, data = TGR_All_24h)) # 2.697812
VIF(lm(PH ~ HR_SUNRISE + WATER_T + O2_CONC + CHL, data = TGR_All_24h)) # 5.106104 # Eliminate PH from model4
cor(TGR_All_24h$HR_SUNRISE, TGR_All_24h$WATER_T) # If cor > 0.7, multi-collinearity strongly suspected # 0.4415366
cor(TGR_All_24h$HR_SUNRISE, TGR_All_24h$O2_CONC) # 0.4975102
cor(TGR_All_24h$HR_SUNRISE, TGR_All_24h$CHL) # -0.1930994
cor(TGR_All_24h$HR_SUNRISE, TGR_All_24h$PH) # 0.07856682
cor(TGR_All_24h$WATER_T, TGR_All_24h$O2_CONC) # 0.530077
cor(TGR_All_24h$WATER_T, TGR_All_24h$CHL) # 0.008412699
cor(TGR_All_24h$WATER_T, TGR_All_24h$PH) # 0.03750056
cor(TGR_All_24h$O2_CONC, TGR_All_24h$CHL) #  0.2181988
cor(TGR_All_24h$O2_CONC, TGR_All_24h$PH) # 0.6330743
cor(TGR_All_24h$CHL, TGR_All_24h$PH) # 0.6942383 # Eliminate PH from model4
summary(lm(TGR_All_24h$PH~TGR_All_24h$CHL))
cor(TGR_All_24h$CHL, TGR_All_24h$PH, method="pearson")

model4 <- lmer(FCH4 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + O2_CONC|SITE) + (0 + O2_CONC|MONTH) + (0 + CHL|SITE) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
coef(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc4  

# In-situ respiration model-a
model5 <- lmer(FCH4 ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
coef(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc5  

# In-situ respiration model-b
VIF(lm(HR_SUNSET ~ WATER_T + AOU + PH, data = TGR_All_24h)) # If VIF > 5, multi-collinearity strongly suspected # 1.38556
VIF(lm(WATER_T ~ HR_SUNSET + AOU + PH, data = TGR_All_24h)) # 2.726058
VIF(lm(AOU ~ HR_SUNSET + WATER_T + PH, data = TGR_All_24h)) # 3.898177
VIF(lm(PH ~ HR_SUNSET + WATER_T + AOU, data = TGR_All_24h)) # 2.112591
cor(TGR_All_24h$HR_SUNSET, TGR_All_24h$WATER_T) # If cor > 0.7, multi-collinearity strongly suspected # -0.5029835
cor(TGR_All_24h$HR_SUNSET, TGR_All_24h$AOU) # 0.4439923
cor(TGR_All_24h$HR_SUNSET, TGR_All_24h$PH) # -0.07932031
cor(TGR_All_24h$WATER_T, TGR_All_24h$AOU) # -0.6676912
cor(TGR_All_24h$WATER_T, TGR_All_24h$PH) # 0.03750056
cor(TGR_All_24h$AOU, TGR_All_24h$PH) # -0.5631529

model6 <- lmer(FCH4 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + AOU|SITE) + (0 + AOU|MONTH) + (0 + PH|SITE) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc6 

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 0.1394529
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 0
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 19.28459
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 6.38552
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 15.08294

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.472 + 0.507 + 0.000 + 0.021 + 0.000


#################################################################
## FCH4 Linear Mixed Effects Modeling - Three Gorges Reservoir ##
#################################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
# install.packages("lme4") #For mixed effects function
library(lme4)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FCH4", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # FCH4 in mg CH4 m-2 h-1 
    # AOU in mg O2 L-1 
    # PRECIP is 1=y 2=n 
    # WATER_T in deg C 
    # CHL in ug chla L-1 
    # O2_CONC in mg O2 L-1 
    # COND in mS cm-1

# Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) # Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length((TGR_All_24h$FCH4))  

# Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$FCH4)  

# Null model
model1 <- lmer(FCH4 ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc1 # 70.10917  

# Terrestrial-aquatic coupling model
# model2 <- lmer(FCH4 ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL  
# npar <- 8
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC  
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(FCH4 ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc3 # 81.96117  

# In-situ production model-b
model4 <- lmer(FCH4 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) +  (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc4 # 97.60223  

# In-situ respiration model-a
model5 <- lmer(FCH4 ~ HR_SUNSET + (1|SITE) +  (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc5 # 81.90339  

# In-situ respiration model-b
model6 <- lmer(FCH4 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) +  (0 + HR_SUNSET|SITE) +  (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc6 # 95.91506 

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 0 
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 11.85199
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 27.49305
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 11.79421
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 25.80588

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.995 + 0.003 + 0.000 + 0.002 + 0.000


#####################################################################
## CH4 Ebullition Linear Mixed Effects Modeling - Floodplain Ponds ##
#####################################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
# install.packages("lme4") #For mixed effects function
library(lme4)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "EBULL", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # EBULL in mg CH4 m-2 h-1 
    # AOU in mg O2 L-1 
    # PRECIP is 1=y 2=n 
    # WATER_T in deg C 
    # CHL in ug chla L-1 
    # O2_CONC in mg O2 L-1 
    # COND in mS cm-1
 
# Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$EBULL)  

# Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$EBULL)  

# Null model
model1 <- lmer(EBULL ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
coef(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc1 # 245.6348  

# Terrestrial-aquatic coupling model
# model2 <- lmer(EBULL ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
# logLik(model2)
# coef(model2)
# NLL <- logLik(model2) * -1
# NLL  
# npar <- 13
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC  
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(EBULL ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
coef(model3)
NLL <- logLik(model3) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc3 # 253.3198 

# In-situ production model-b
model4 <- lmer(EBULL ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
coef(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc4 # 296.8706  

# In-situ respiration model-a
model5 <- lmer(EBULL ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
coef(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc5 # 254.2488 

# In-situ respiration model-b
model6 <- lmer(EBULL ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + AOU|SITE) + (0 + AOU|MONTH) + (0 + PH|SITE) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc6 # 289.1059  

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 0 
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 7.684993
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 51.23575
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 8.613997
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 43.47107

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.966 + 0.021 + 0.000 + 0.013 + 0.000


###########################################################################
## CH4 Ebullition Linear Mixed Effects Modeling - Three Gorges Reservoir ##
###########################################################################

rm(list=ls())
setwd("~/Desktop")

# Compare funnel v. chamber ebullition
funnel <- c(0.002,0.002, 0.003)
chamber <- c(0.611, 0.511, 0.145)
ebullition <- chamber <- c(0.611, 0.511, 0.145, 0.611, 0.511, 0.145)
wilcox.test(funnel, chamber) # p-value > 0.001
boxplot(funnel, chamber) # Higher ebullition in chamber
mean(funnel) #0.0023 +/- 0.0003
mean(chamber) #0.4 +/- 0.1
sd(funnel)/(sqrt(3))
sd(chamber)/(sqrt(3))
mean(ebullition) #0.42 +/- 0.09
sd(ebullition)/(sqrt(6))

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
# install.packages("lme4") #For mixed effects function
library(lme4)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "EBULL", "PRECIP", "WATER_T", "CHL", "AOU", "O2_CONC", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # EBULL in mg CH4 m-2 h-1 
    # AOU in mg O2 L-1 
    # PRECIP is 1=y 2=n 
    # WATER_T in deg C 
    # CHL in ug chla L-1 
    # O2_CONC in mg O2 L-1 
    # COND in mS cm-1

# Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) # Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$EBULL)  

# Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$EBULL) 

# Null model
model1 <- lmer(EBULL ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc1 # 6.56525 

# Terrestrial-aquatic coupling model
# model2 <- lmer(EBULL ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL  
# npar <- 8
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC  
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(EBULL ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL   
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc3 # 22.14369  

# In-situ production model-b
model4 <- lmer(EBULL ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc4 # 158.3477  

# In-situ respiration model-a
model5 <- lmer(EBULL ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc5 # 21.36216  

# In-situ respiration model-b
model6 <- lmer(EBULL ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc6 # 155.9245  

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 0 
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 15.57844
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 151.7824
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 14.79691
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 149.3593

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.999 + 0.000 + 0.000 + 0.001 + 0.000


###########################################################
## FCO2 Linear Mixed Effects Modeling - Floodplain Ponds ##
###########################################################

rm(list=ls()) 

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
# install.packages("lme4") #For mixed effects function
library(lme4)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # FCO2 in mg CO2 m-2 h-1 
    # AOU in mg O2 L-1 
    # PRECIP is 1=y 2=n 
    # WATER_T in deg C 
    # CHL in ug chla L-1 
    # O2_CONC in mg O2 L-1 
    # COND in mS cm-1

# Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) # Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FCO2)  

# Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$FCO2) 

# Null model
model1 <- lmer(FCO2 ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc1 # 2861.123 

# Terrestrial-aquatic coupling model
# model2 <- lmer(FCO2 ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
# logLik(model2)
# coef(model2)
# NLL <- logLik(model2) * -1
# NLL  
# npar <- 13
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC  
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(FCO2 ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc3 # 2861.34  

# In-situ production model-b
model4 <- lmer(FCO2 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc4 # 2853.868 

# In-situ respiration model-a
model5 <- lmer(FCO2 ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc5 # 2856.874  

# In-situ respiration model-b
model6 <- lmer(FCO2 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|MONTH) + (0 + AOU|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc6 # 2844.011  

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 17.1118
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 17.3338
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 9.856822
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 12.86361
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 0

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.000 + 0.000 + 0.007 + 0.002 + 0.991


#################################################################
## FCO2 Linear Mixed Effects Modeling - Three Gorges Reservoir ##
#################################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
# install.packages("lme4") #For mixed effects function
library(lme4)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # FCO2 in mg CO2 m-2 h-1 
    # AOU in mg O2 L-1 
    # PRECIP is 1=y 2=n 
    # WATER_T in deg C 
    # CHL in ug chla L-1 
    # O2_CONC in mg O2 L-1 
    # COND in mS cm-1

# Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) # Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FCO2)  

# Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$FCO2)  

# Null model
model1 <- lmer(FCO2 ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc1 # 915.3669  

# Terrestrial-aquatic coupling model
# model2 <- lmer(FCO2 ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL  
# npar <- 8
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC 
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(FCO2 ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc3 # 917.4797  

# In-situ production model-b
model4 <- lmer(FCO2 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL 
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc4 # 909.5041  

# In-situ respiration model-a
model5 <- lmer(FCO2 ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc5 # 916.0607  

# In-situ respiration model-b
model6 <- lmer(FCO2 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc6 # 903.4273  

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 11.93964
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 14.05237
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 6.076815
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 12.63344
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 0

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.002 + 0.001 + 0.045 + 0.002 + 0.950


###########################################################
## FN2O Linear Mixed Effects Modeling - Floodplain Ponds ##
###########################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
# install.packages("lme4") #For mixed effects function
library(lme4)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # FN2O in mg N2O m-2 h-1 
    # AOU in mg O2 L-1 
    # PRECIP is 1=y 2=n 
    # WATER_T in deg C 
    # CHL in ug chla L-1 
    # O2_CONC in mg O2 L-1 
    # COND in mS cm-1

#M ake sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) # Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FN2O)  

# Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$FN2O)  

# Null model
model1 <- lmer(FN2O ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc1 # -530.071

# Terrestrial-aquatic coupling model
# model2 <- lmer(FN2O ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL  
# npar <- 13
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC 
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
# AICc2 

# In-situ production model-a
model3 <- lmer(FN2O ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL 
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc3 # -514.2705

# In-situ production model-b
model4 <- lmer(FN2O ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc4 # -463.9311 

# In-situ respiration model-a
model5 <- lmer(FN2O ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc5 # -514.8101  

# In-situ respiration model-b
model6 <- lmer(FN2O ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|MONTH) + (0 + AOU|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc6 # -475.5814

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 0
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 66.1399
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 66.1399
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 15.26091
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 54.4896

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.999 + 0.000 + 0.000 + 0.001 + 0.000


#################################################################
## FN2O Linear Mixed Effects Modeling - Three Gorges Reservoir ##
#################################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
# install.packages("lme4") #For mixed effects function
library(lme4)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # FN2O in mg N2O m-2 h-1 
    # AOU in mg O2 L-1 
    # PRECIP is 1=y 2=n 
    # WATER_T in deg C 
    # CHL in ug chla L-1 
    # O2_CONC in mg O2 L-1 
    # COND in mS cm-1

# Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) # Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FN2O)  

# Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$FN2O) #n=94

# Null model
length(TGR_All_24h$FN2O)
model1 <- lmer(FN2O ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc1 # -67.55882  

# Terrestrial-aquatic coupling model
# model2 <- lmer(FN2O ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL  
# npar <- 8
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC 
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
# AICc2 

# In-situ production model-a
model3 <- lmer(FN2O ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL 
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc3 # -54.63203  

# In-situ production model-b
model4 <- lmer(FN2O ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC 
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc4 # -30.91906  

# In-situ respiration model-a
model5 <- lmer(FN2O ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL   
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc5 # -54.18944  

# In-situ respiration model-b
model6 <- lmer(FN2O ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc6 # -31.7146  

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 0
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 12.92679
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 36.63975
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 13.36938
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 35.84422

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.997 + 0.002 + 0.000 + 0.001 + 0.000

