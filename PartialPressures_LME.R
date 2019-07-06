###########################################################
## PCH4 Linear Mixed Effects Modeling - Floodplain Ponds ##
###########################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

# Load the packages
install.packages("lme4") #For mixed effects function
library(lme4)
install.packages("fmsb") #For VIF function
library(fmsb)

# Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCH4", "AOU", "PRECIP", "WATER_T", "CHL", "O2_CONC", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # PCH4 in uatm #AOU in mg O2 L-1 
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
length(TGR_All_24h$PCH4) # n=108

# Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$PCH4) # n=36

# Null model
model1 <- lmer(PCH4 ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
coef(model1)
NLL <- logLik(model1) * -1
NLL 
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC 
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc1 # 368.5768

# Terrestrial-aquatic coupling model
# model2 <- lmer(PCH4 ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
# logLik(model2)
# coef(model2)
# NLL <- logLik(model2) * -1
# NLL #166.5645
# npar <- 13
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC 
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
# AICc2 

# In-situ production model-a
model3 <- lmer(PCH4 ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
coef(model3)
NLL <- logLik(model3) * -1
NLL 
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC 
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc3 # 364.388

# In-situ production model-b
model4 <- lmer(PCH4 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + O2_CONC|SITE) + (0 + O2_CONC|MONTH) + (0 + CHL|SITE) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
coef(model4)
NLL <- logLik(model4) * -1
NLL 
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc4 # 352.2732

# In-situ respiration model-a
model5 <- lmer(PCH4 ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
coef(model5)
NLL <- logLik(model5) * -1
NLL 
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc5 # 363.9673

# In-situ respiration model-b
model6 <- lmer(PCH4 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + AOU|SITE) + (0 + AOU|MONTH) + (0 + PH|SITE) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc6 # 341.6233

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 26.95353
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 22.76488
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 10.64987 
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 22.344
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 0

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.000 + 0.000 + 0.005 + 0.000 + 0.995 


#################################################################
## PCH4 Linear Mixed Effects Modeling - Three Gorges Reservoir ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCH4", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # PCH4 in uatm 
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
length((TGR_All_24h$PCH4)) #n=108

# Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$PCH4) # n=72

# Null model
model1 <- lmer(PCH4 ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL 
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC 
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc1 # 841.5514

# Terrestrial-aquatic coupling model
# model2 <- lmer(PCH4 ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL 
# npar <- 8
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC 
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
# AICc2 

# In-situ production model-a
model3 <- lmer(PCH4 ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc3  

# In-situ production model-b
model4 <- lmer(PCH4 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) +  (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc4  

# In-situ respiration model-a
model5 <- lmer(PCH4 ~ HR_SUNSET + (1|SITE) +  (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc5  

# In-situ respiration model-b
model6 <- lmer(PCH4 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) +  (0 + HR_SUNSET|SITE) +  (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc6  

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 34.62146 
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 30.37246
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 5.329615 
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 30.84069
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 0

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.000 + 0.000 + 0.065 + 0.000 + 0.935 


###########################################################
## PCO2 Linear Mixed Effects Modeling - Floodplain Ponds ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # PCO2 in uatm 
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
length(TGR_All_24h$PCO2)

# Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$PCO2) 

# Null model
model1 <- lmer(PCO2 ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc1  # 455.1494

# Terrestrial-aquatic coupling model
# model2 <- lmer(PCO2 ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
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
model3 <- lmer(PCO2 ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL 
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc3 # 448.7621  

# In-situ production model-b
model4 <- lmer(PCO2 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc4  # 418.955

# In-situ respiration model-a
model5 <- lmer(PCO2 ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc5  # 444.9551

# In-situ respiration model-b
model6 <- lmer(PCO2 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|MONTH) + (0 + AOU|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc6  # 413.8843

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 41.26515
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 34.87779
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 5.070672
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 31.07077
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 0

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.000 + 0.000 + 0.073 + 0.000 + 0.927


#################################################################
## PCO2 Linear Mixed Effects Modeling - Three Gorges Reservoir ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # PCO2 in uatm 
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
length(TGR_All_24h$PCO2)  

# Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$PCO2) 

# Null model
model1 <- lmer(PCO2 ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc1 # 800.9624  

# Terrestrial-aquatic coupling model
# model2 <- lmer(PCO2 ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL  
# npar <- 8
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC  
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(PCO2 ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC   
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc3 # 790.0098

# In-situ production model-b
model4 <- lmer(PCO2 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc4 # 765.3617  

# In-situ respiration model-a
model5 <- lmer(PCO2 ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc5 # 793.0948  

# In-situ respiration model-b
model6 <- lmer(PCO2 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc6 # 761.7998  

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 39.1626
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 28.21
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 3.56194
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 31.29496
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 0

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.000 + 0.000 + 0.144 + 0.000 + 0.856


###########################################################
## PN2O Linear Mixed Effects Modeling - Floodplain Ponds ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # PN2O in uatm 
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
length(TGR_All_24h$PN2O)  

# Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$PN2O)  

# Null model
model1 <- lmer(PN2O ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC 
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc1 # -95.75674  

# Terrestrial-aquatic coupling model
# model2 <- lmer(PN2O ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL #-45.09671
# npar <- 13
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC  
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(PN2O ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL 
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc3 # -92.14742 

# In-situ production model-b
model4 <- lmer(PN2O ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc4 # -65.6077 

# In-situ respiration model-a
model5 <- lmer(PN2O ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc5 # -92.35215 

# In-situ respiration model-b
model6 <- lmer(PN2O ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|MONTH) + (0 + AOU|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc6 # -74.23029 

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 0
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 3.609327
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 30.14905
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 #  3.404598
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 21.52645

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.743 + 0.122 + 0.00 + 0.135 + 0.000


#################################################################
## PN2O Linear Mixed Effects Modeling - Three Gorges Reservoir ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

# Notes on units 
    # PN2O in uatm 
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
length(TGR_All_24h$PN2O)  

# Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$PN2O)  

# Null model
length(TGR_All_24h$FN2O)
model1 <- lmer(PN2O ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL  
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC  
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc1 # 31.92161  

# Terrestrial-aquatic coupling model
# model2 <- lmer(PN2O ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
# logLik(model2)
# NLL <- logLik(model2) * -1
# NLL   
# npar <- 8
# AIC <- (-2 * logLik(model2)) + (2 * npar)
# AIC  
# AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
# AICc2  

# In-situ production model-a
model3 <- lmer(PN2O ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC  
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc3 # 18.62681  

# In-situ production model-b
model4 <- lmer(PN2O ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC  
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc4 # 27.92529

# In-situ respiration model-a
model5 <- lmer(PN2O ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL  
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC  
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc5 # -4.336365

# In-situ respiration model-b
model6 <- lmer(PN2O ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL  
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC  
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc6 # 1.455298  

# Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc1 # 36.25798
# delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
# delAICc2 
delAICc3 <- AICc3 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc3 # 22.96317
delAICc4 <- AICc4 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc4 # 32.26166
delAICc5 <- AICc5 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc5 # 0
delAICc6 <- AICc6 - min(c(AICc1, AICc3, AICc4, AICc5, AICc6))
delAICc6 # 5.791663

# Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
# exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) 
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6))
0.000 + 0.000 + 0.000 + 0.948 + 0.052

