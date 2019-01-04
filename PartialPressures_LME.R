###########
## NOTES ##
###########

#Last updated 04 Jan 2019 by BLM


###########################################################
## PCH4 Linear Mixed Effects Modeling - Floodplain Ponds ##
###########################################################

rm(list=ls())
setwd("~/Desktop")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Load the packages
#install.packages("lme4") #For mixed effects function
library(lme4)
#install.packages("fmsb") #For VIF function
library(fmsb)

#Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCH4", "AOU", "PRECIP", "WATER_T", "CHL", "O2_CONC", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#PCH4 in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$PCH4) #n=108

#Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$PCH4) #n=36

#Null model
model1 <- lmer(PCH4 ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
coef(model1)
NLL <- logLik(model1) * -1
NLL #184.2884
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #376.5768
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc1 #368.5768 #df=4

#Terrestrial-aquatic coupling model
model2 <- lmer(PCH4 ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model2)
coef(model2)
NLL <- logLik(model2) * -1
NLL #166.5645
npar <- 13
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #359.129
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc2 #333.129 #df=13

#In-situ production model-a
model3 <- lmer(PCH4 ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
coef(model3)
NLL <- logLik(model3) * -1
NLL #182.1941
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #378.3882
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc3 #364.3882 #df=7

#In-situ production model-b
model4 <- lmer(PCH4 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + O2_CONC|SITE) + (0 + O2_CONC|MONTH) + (0 + CHL|SITE) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
coef(model4)
NLL <- logLik(model4) * -1
NLL #176.135
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #382.2699
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc4 #352.2699 #df=16

#In-situ respiration model-a
model5 <- lmer(PCH4 ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
coef(model5)
NLL <- logLik(model5) * -1
NLL #181.9837
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #377.9673
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc5 #363.9673 #df=7

#In-situ respiration model-b
model6 <- lmer(PCH4 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + AOU|SITE) + (0 + AOU|MONTH) + (0 + PH|SITE) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL #170.804 
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #371.6079
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc6 #341.6079 #df=16

#Reload the data
#TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Make a dataframe with **incomplete** 24 h data
#colnames(TGR_All)
#TGR_All$SEASON
#TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCH4", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          #"PH", "COND", "WIND", "P"))
#colnames(TGR_All_24h)

#Notes on units 
#PCH4 in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 #WIND in m s-1 #P in atm

#Make sure that the linear regression matrix is complete by determining which columns have NA values
#which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
#nrow(TGR_All_24h)
#TGR_All_24h <- na.omit(TGR_All_24h)
#nrow(TGR_All_24h)
#which(is.na(TGR_All_24h))
#length(TGR_All_24h$PCH4) #n=108

#Subset summer
#TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
#length(TGR_All_24h$PCH4) #n=36

#Physical forcing model
#model7 <- lmer(PCH4 ~ WATER_T + COND + WIND + P + (1|SITE) + (0 + WATER_T|SITE) + (0 + COND|SITE) + (0 + WIND|SITE) + (0 + P|SITE), data = TGR_All_24h)
#logLik(model7)
#coef(model7)
#NLL <- logLik(model7) * -1
#NLL #169.4034
#npar <- 10
#AIC <- (-2 * logLik(model7)) + (2 * npar)
#AIC #358.8067
#AICc7 <- (-2* logLik(model7)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
#AICc7 #338.8067 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #35.44786
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #0
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #31.2592
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #19.14095
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #30.83834
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #8.478942

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.978557e-08
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9857215
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.606588e-07
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #6.876215e-05
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.98287e-07
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.01420939
0.000 + 0.986 + 0.000 + 0.000 + 0.000 + 0.014


#################################################################
## PCH4 Linear Mixed Effects Modeling - Three Gorges Reservoir ##
#################################################################

rm(list=ls())
setwd("~/Desktop")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Load the packages
#install.packages("lme4") #For mixed effects function
library(lme4)

#Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCH4", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#PCH4 in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length((TGR_All_24h$PCH4)) #n=108

#Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$PCH4) #n=72

#Null model
model1 <- lmer(PCH4 ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #609.2958
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #1224.592
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc1 #1218.592 #df=3

#Terrestrial-aquatic coupling model
model2 <- lmer(PCH4 ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #593.6368
npar <- 8
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #1203.274
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc2 #1187.274 #df=8

#In-situ production model-a
model3 <- lmer(PCH4 ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #604.4697
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #1218.939
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc3 #1208.939 #df=5

#In-situ production model-b
model4 <- lmer(PCH4 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) +  (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #584.1823
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #1190.365
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc4 #1168.365 #df=11

#In-situ respiration model-a
model5 <- lmer(PCH4 ~ HR_SUNSET + (1|SITE) +  (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #604.7541
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #1219.508
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc5 #1209.508 #df=5

#In-situ respiration model-b
model6 <- lmer(PCH4 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) +  (0 + HR_SUNSET|SITE) +  (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL #581.6737
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #1185.347
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc6 #1163.347 #df=11

#Reload the data
#TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Make a dataframe with **incomplete** 24 h data
#colnames(TGR_All)
#TGR_All$SEASON
#TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCH4", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
#"PH", "COND", "WIND", "P"))
#colnames(TGR_All_24h)

#Notes on units 
#PCH4 in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 #WIND in m s-1 #P in atm

#Make sure that the linear regression matrix is complete by determining which columns have NA values
#which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
#nrow(TGR_All_24h)
#TGR_All_24h <- na.omit(TGR_All_24h)
#nrow(TGR_All_24h)
#which(is.na(TGR_All_24h))
#length(TGR_All_24h$PCH4) #n=108

#Subset winter
#TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
#length(TGR_All_24h$PCH4) #n=72

#Physical forcing model
#model7 <- lmer(PCH4 ~ WATER_T + COND + WIND + P + (1|SITE) + (0 + WATER_T|SITE) + (0 + COND|SITE) + (0 + WIND|SITE) + (0 + P|SITE), data = TGR_All_24h)
#logLik(model7)
#coef(model7)
#NLL <- logLik(model7) * -1
#NLL #578.8072
#npar <- 10
#AIC <- (-2 * logLik(model7)) + (2 * npar)
#AIC #1177.614
#AICc7 <- (-2* logLik(model7)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
#AICc7 #1157.614 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #55.2441
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #23.92609
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #45.59193
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #5.01708
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #46.16066
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #0

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #9.330716e-13
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #5.895644e-06
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.163738e-10
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.07526121
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #8.757001e-11
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9247329
0.000 + 0.000 + 0.000 + 0.075 + 0.000 + 0.925


###########################################################
## PCO2 Linear Mixed Effects Modeling - Floodplain Ponds ##
###########################################################

rm(list=ls())
setwd("~/Desktop")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Load the packages
#install.packages("lme4") #For mixed effects function
library(lme4)

#Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#PCO2 in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$PCO2) #n=108

#Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$PCO2) #n=36

#Null model
model1 <- lmer(PCO2 ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #227.5747
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #463.1494
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc1 #455.1494 #df=4

#Terrestrial-aquatic coupling model
model2 <- lmer(PCO2 ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model2)
coef(model2)
NLL <- logLik(model2) * -1
NLL #207.5062
npar <- 13
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #441.0124
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc2 #415.0124 #df=13

#In-situ production model-a
model3 <- lmer(PCO2 ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #224.381
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #462.7621
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc3 #448.7621 #df=7

#In-situ production model-b
model4 <- lmer(PCO2 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #209.456
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #448.912
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc4 #418.912 #df=16

#In-situ respiration model-a
model5 <- lmer(PCO2 ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #222.4775
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #458.9551
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc5 #444.9551 #df=7

#In-situ respiration model-b
model6 <- lmer(PCO2 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|MONTH) + (0 + AOU|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL #206.9418
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #445.8835
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc6 #413.8835 #df=16

#Reload the data
#TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Make a dataframe with **incomplete** 24 h data
#colnames(TGR_All)
#TGR_All$SEASON
#TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
#                                          "PH", "COND", "WIND", "P"))
#colnames(TGR_All_24h)

#Notes on units 
#PCO2 in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 #WIND in m s-1 #P in atm

#Make sure that the linear regression matrix is complete by determining which columns have NA values
#which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
#nrow(TGR_All_24h)
#TGR_All_24h <- na.omit(TGR_All_24h)
#nrow(TGR_All_24h)
#which(is.na(TGR_All_24h))
#length(TGR_All_24h$PCO2) #n=108

#Subset summer
#TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
#length(TGR_All_24h$PCO2) #n=36

#Physical forcing model
#model7 <- lmer(PCO2 ~ WATER_T + COND + WIND + P + (1|SITE) + (0 + WATER_T|SITE) + (0 + COND|SITE) + (0 + WIND|SITE) + (0 + P|SITE), data = TGR_All_24h)
#logLik(model7)
#coef(model7)
#NLL <- logLik(model7) * -1
#NLL #206.0161
#npar <- 10
#AIC <- (-2 * logLik(model7)) + (2 * npar)
#AIC #432.0321
#AICc7 <- (-2* logLik(model7)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
#AICc7 #412.0321 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #41.26589
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #1.128896
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #34.87853
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #5.028469
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #31.07151
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #0

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #6.635085e-10
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.3447347
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.617504e-08 
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.04905728
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.085247e-07
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.6062079
0.000 + 0.345 + 0.000 + 0.049 + 0.000 + 0.606


#################################################################
## PCO2 Linear Mixed Effects Modeling - Three Gorges Reservoir ##
#################################################################

rm(list=ls())
setwd("~/Desktop")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Load the packages
#install.packages("lme4") #For mixed effects function
library(lme4)

#Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#PCO2 in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$PCO2) #n=108

#Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$PCO2) #n=72

#Null model
model1 <- lmer(PCO2 ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #400.4812
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #806.9624
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc1 #800.9624 #df=3

#Terrestrial-aquatic coupling model
model2 <- lmer(PCO2 ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #389.4317
npar <- 8
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #794.8635 
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc2 #778.8635 #df=8

#In-situ production model-a
model3 <- lmer(PCO2 ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #395.0049
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #800.0098
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc3 #790.0098 #df=5

#In-situ production model-b
model4 <- lmer(PCO2 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #382.6809
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #787.3617
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc4 #765.3617 #df=11

#In-situ respiration model-a
model5 <- lmer(PCO2 ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #396.5474
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #803.0948
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc5 #793.0948 #df=5

#In-situ respiration model-b
model6 <- lmer(PCO2 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL #380.8999
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #783.7998
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc6 #761.7998 #df=11

#Reload the data
#TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Make a dataframe with **incomplete** 24 h data
#colnames(TGR_All)
#TGR_All$SEASON
#TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
#                                          "PH", "COND", "WIND", "P"))
#colnames(TGR_All_24h)

#Notes on units 
#PCO2 in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 #WIND in m s-1 #P in atm

#Make sure that the linear regression matrix is complete by determining which columns have NA values
#which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
#nrow(TGR_All_24h)
#TGR_All_24h <- na.omit(TGR_All_24h)
#nrow(TGR_All_24h)
#which(is.na(TGR_All_24h))
#length(TGR_All_24h$PCO2) #n=108

#Subset winter
#TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
#length(TGR_All_24h$PCO2) #n=72

#Physical forcing model
#model7 <- lmer(PCO2 ~ WATER_T + COND + WIND + P + (1|SITE) + (0 + WATER_T|SITE) + (0 + COND|SITE) + (0 + WIND|SITE) + (0 + P|SITE), data = TGR_All_24h)
#logLik(model7)
#coef(model7)
#NLL <- logLik(model7) * -1
#NLL #377.8146
#npar <- 10
#AIC <- (-2 * logLik(model7)) + (2 * npar)
#AIC #775.6292
#AICc7 <- (-2* logLik(model7)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
#AICc7 #755.6292 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #39.1626
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #17.06366
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #28.21
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #3.56194
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #31.29496
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #0

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #2.681194e-09
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0001686479
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #6.40703e-07
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.144159
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.369915e-07
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.8556716
0.000 + 0.000 + 0.000 + 0.144 + 0.000 + 0.856


###########################################################
## PN2O Linear Mixed Effects Modeling - Floodplain Ponds ##
###########################################################

rm(list=ls())
setwd("~/Desktop")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Load the packages
#install.packages("lme4") #For mixed effects function
library(lme4)

#Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#PN2O in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$PN2O) #n=108

#Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$PN2O) #n=36

#Null model
model1 <- lmer(PN2O ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #-47.87837
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #-87.75674
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc1 #-95.75674 #df=4

#Terrestrial-aquatic coupling model
model2 <- lmer(PN2O ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #-45.09671
npar <- 13
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #-64.19342
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc2 #-90.19342 #df=13

#In-situ production model-a
model3 <- lmer(PN2O ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #-46.07371
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #-78.14742
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc3 #-92.14742 #df=7

#In-situ production model-b
model4 <- lmer(PN2O ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #-32.86036
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #-33.72071
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc4 #-65.72071 #df=16

#In-situ respiration model-a
model5 <- lmer(PN2O ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #-46.17607
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #-78.35215
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc5 #-92.35215 #df=7

#In-situ respiration model-b
model6 <- lmer(PN2O ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|MONTH) + (0 + AOU|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL #-37.11537
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #-42.23074
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc6 #-74.23074 #df=16

#Reload the data
#TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Make a dataframe with **incomplete** 24 h data
#colnames(TGR_All)
#TGR_All$SEASON
#TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
#                                          "PH", "COND", "WIND", "P"))
#colnames(TGR_All_24h)

#Notes on units 
#PN2O in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 #WIND in m s-1 #P in atm

#Make sure that the linear regression matrix is complete by determining which columns have NA values
#which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
#nrow(TGR_All_24h)
#TGR_All_24h <- na.omit(TGR_All_24h)
#nrow(TGR_All_24h)
#which(is.na(TGR_All_24h))
#length(TGR_All_24h$PN2O) #n=108

#Subset summer
#TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
#length(TGR_All_24h$PN2O) #n=36

#Physical forcing model
#model7 <- lmer(PN2O ~ WATER_T + COND + WIND + P + (1|SITE) + (0 + WATER_T|SITE) + (0 + COND|SITE) + (0 + WIND|SITE) + (0 + P|SITE), data = TGR_All_24h)
#logLik(model7)
#coef(model7)
#NLL <- logLik(model7) * -1
#NLL #-39.30241
#npar <- 10
#AIC <- (-2 * logLik(model7)) + (2 * npar)
#AIC #-58.60482
#AICc7 <- (-2* logLik(model7)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
#AICc7 #-78.60482 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #0
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #5.563327
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #3.609327
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #30.03603
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #3.404597
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #21.526

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.7098488
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.04396477
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.1167913
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #2.132676e-07
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.1293799
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.502635e-05
0.710 + 0.044 + 0.117 + 0.000 + 0.129 + 0.000


#################################################################
## PN2O Linear Mixed Effects Modeling - Three Gorges Reservoir ##
#################################################################

rm(list=ls())
setwd("~/Desktop/TGR")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Load the packages
#install.packages("lme4") #For mixed effects function
library(lme4)

#Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#PN2O in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$PN2O) #n=108

#Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$PN2O) #n=72

#Null model
length(TGR_All_24h$FN2O)
model1 <- lmer(PN2O ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #15.96081
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #37.92161
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc1 #31.92161 #df=3

#Terrestrial-aquatic coupling model
model2 <- lmer(PN2O ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #11.42617 
npar <- 8
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #38.85233
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc2 #22.85233 #df=8

#In-situ production model-a
model3 <- lmer(PN2O ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #9.313404
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #28.62681
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc3 #18.62681 #df=5

#In-situ production model-b
model4 <- lmer(PN2O ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #13.48787
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #48.97574
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc4 #26.97574 #df=11

#In-situ respiration model-a
model5 <- lmer(PN2O ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #-2.168183 
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #5.663635 
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc5 #-4.336365 #df=5

#In-situ respiration model-b
model6 <- lmer(PN2O ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL #0.7276483
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #23.4553
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc6 #1.455297 #df=11

#Reload the data
#TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Make a dataframe with **incomplete** 24 h data
#colnames(TGR_All)
#TGR_All$SEASON
#TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "PN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
#                                          "PH", "COND", "WIND", "P"))
#colnames(TGR_All_24h)

#Notes on units 
#PN2O in uatm #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1 #WIND in m s-1 #P in atm

#Make sure that the linear regression matrix is complete by determining which columns have NA values
#which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
#nrow(TGR_All_24h)
#TGR_All_24h <- na.omit(TGR_All_24h)
#nrow(TGR_All_24h)
#which(is.na(TGR_All_24h))
#length(TGR_All_24h$PN2O) #n=36

#Subset winter
#TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
#length(TGR_All_24h$PN2O) #n=72

#Physical forcing model
#model7 <- lmer(PN2O ~ WATER_T + COND + WIND + P + (1|SITE) + (0 + WATER_T|SITE) + (0 + COND|SITE) + (0 + WIND|SITE) + (0 + P|SITE), data = TGR_All_24h)
#logLik(model7)
#coef(model7)
#NLL <- logLik(model7) * -1
#NLL #377.8146
#npar <- 11
#AIC <- (-2 * logLik(model7)) + (2 * npar)
#AIC #775.6292
#AICc7 <- (-2* logLik(model7)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
#AICc7 #755.6292 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #36.25798
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #27.1887
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #22.96317
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #31.31211
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #0
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #5.791662

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.268581e-08 
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.18219e-06
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #9.777974e-06
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.504183e-07
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9476294
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.05235946 
0.000 + 0.000 + 0.000 + 0.000 + 0.948 + 0.052

