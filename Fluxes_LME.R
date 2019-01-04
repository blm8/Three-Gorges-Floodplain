###########
## NOTES ##
###########

#Last updated 04 Jan 2019 by BLM
#Opted out of model7 or the physical forcing model due to small sample size for WIND; try with partial pressures?
#Is the selection of random effects essentially based on heteroscedacity? Random intercepts?


###########################################################
## FCH4 Linear Mixed Effects Modeling - Floodplain Ponds ##
###########################################################

rm(list=ls())
setwd("~/Desktop")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Load the packages
install.packages("lme4") #For mixed effects function
library(lme4)
install.packages("fmsb") #For VIF function
library(fmsb)

#Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FCH4", "AOU", "PRECIP", "WATER_T", "CHL", "O2_CONC", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#FCH4 in mg CH4 m-2 h-1 #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FCH4) #n=327

#Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$FCH4) #n=236

#Null model
model1 <- lmer(FCH4 ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
coef(model1)
NLL <- logLik(model1) * -1
NLL #774.6174
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #1557.235
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc1 #1557.408 #df=4

#Terrestrial-aquatic coupling model
#Check for multi-collinearity
VIF(lm(COND ~ PH + PRECIP, data = TGR_All_24h)) #If VIF > 5, multi-collinearity strongly suspected #3.166587
VIF(lm(PH ~ COND + PRECIP, data = TGR_All_24h)) #3.09485
VIF(lm(PRECIP ~ PH + COND, data = TGR_All_24h)) #1.120215
cor(TGR_All_24h$COND, TGR_All_24h$PH) #If cor > 0.7,#0.8043296 #Eliminate PH from model2
cor(TGR_All_24h$COND, TGR_All_24h$PRECIP) #0.1568687
cor(TGR_All_24h$PRECIP, TGR_All_24h$PH) #-0.04470506

model2 <- lmer(FCH4 ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) +(0 + PH|MONTH), data = TGR_All_24h)
logLik(model2)
coef(model2)
NLL <- logLik(model2) * -1
NLL #757.5809
npar <- 13
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #1541.162
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc2 #1542.801 #df=13

#In-situ production model-a
model3 <- lmer(FCH4 ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
coef(model3)
NLL <- logLik(model3) * -1
NLL #771.1911
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #1556.382
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc3 #1556.873 #df=7

#In-situ production model-b
#Check for multi-collinearity
VIF(lm(HR_SUNRISE ~ WATER_T + O2_CONC + CHL + PH, data = TGR_All_24h)) #If VIF > 5, multi-collinearity strongly suspected #1.565632
VIF(lm(WATER_T ~ HR_SUNRISE + O2_CONC + CHL + PH, data = TGR_All_24h)) #1.947603
VIF(lm(O2_CONC ~ HR_SUNRISE + WATER_T + CHL + PH, data = TGR_All_24h)) #4.204408
VIF(lm(CHL ~ HR_SUNRISE + WATER_T + O2_CONC + PH, data = TGR_All_24h)) #2.718373
VIF(lm(PH ~ HR_SUNRISE + WATER_T + O2_CONC + CHL, data = TGR_All_24h)) #5.260382 #Eliminate PH from model4
cor(TGR_All_24h$HR_SUNRISE, TGR_All_24h$WATER_T) #If cor > 0.7,#0.433528
cor(TGR_All_24h$HR_SUNRISE, TGR_All_24h$O2_CONC) #0.4913991
cor(TGR_All_24h$HR_SUNRISE, TGR_All_24h$CHL) #-0.1569368
cor(TGR_All_24h$HR_SUNRISE, TGR_All_24h$PH) #0.09641144
cor(TGR_All_24h$WATER_T, TGR_All_24h$O2_CONC) #0.5006428
cor(TGR_All_24h$WATER_T, TGR_All_24h$CHL) #0.016558
cor(TGR_All_24h$WATER_T, TGR_All_24h$PH) #0.02628956
cor(TGR_All_24h$O2_CONC, TGR_All_24h$CHL) #0.2621836
cor(TGR_All_24h$O2_CONC, TGR_All_24h$PH) #0.6516579
cor(TGR_All_24h$CHL, TGR_All_24h$PH) #0.7088416 #Eliminate PH from model4

model4 <- lmer(FCH4 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + O2_CONC|SITE) + (0 + O2_CONC|MONTH) + (0 + CHL|SITE) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
coef(model4)
NLL <- logLik(model4) * -1
NLL #771.2683
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #1574.537
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc4 #1577.021 #df=16

#In-situ respiration model-a
model5 <- lmer(FCH4 ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
coef(model5)
NLL <- logLik(model5) * -1
NLL #774.6413
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #1563.283
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc5 #1563.774 #df=7

#In-situ respiration model-b
VIF(lm(HR_SUNSET ~ WATER_T + AOU + PH, data = TGR_All_24h)) #If VIF > 5, multi-collinearity strongly suspected #1.361944
VIF(lm(WATER_T ~ HR_SUNSET + AOU + PH, data = TGR_All_24h)) #2.647475
VIF(lm(AOU ~ HR_SUNSET + WATER_T + PH, data = TGR_All_24h)) #3.843642
VIF(lm(PH ~ HR_SUNSET + WATER_T + AOU, data = TGR_All_24h)) #2.209187
cor(TGR_All_24h$HR_SUNSET, TGR_All_24h$WATER_T) #If cor > 0.7,#-0.4946616
cor(TGR_All_24h$HR_SUNSET, TGR_All_24h$AOU) #0.4244961
cor(TGR_All_24h$HR_SUNSET, TGR_All_24h$PH) #-0.08588008
cor(TGR_All_24h$WATER_T, TGR_All_24h$AOU) #-0.6440705 
cor(TGR_All_24h$WATER_T, TGR_All_24h$PH) #0.02628956
cor(TGR_All_24h$AOU, TGR_All_24h$PH) #-0.5819115

model6 <- lmer(FCH4 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + AOU|SITE) + (0 + AOU|MONTH) + (0 + PH|SITE) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL #769.4724
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #1570.945
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc6 #1573.429 #df=16

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #14.6064
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #0
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #14.07195
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #34.21915
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #20.97235
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #30.62731

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0006723176
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9984213
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0008782703
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #3.704426e-08
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #2.787572e-05
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #2.23192e-07
0.001 + 0.998 + 0.001 + 0.000 + 0.000 + 0.000


#################################################################
## FCH4 Linear Mixed Effects Modeling - Three Gorges Reservoir ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FCH4", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#FCH4 in mg CH4 m-2 h-1 #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length((TGR_All_24h$FCH4)) #n=327

#Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$FCH4) #n=91

#Null model
model1 <- lmer(FCH4 ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #38.99849
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #83.99697
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc1 #84.27284 #df=3

#Terrestrial-aquatic coupling model
model2 <- lmer(FCH4 ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #39.07343
npar <- 8
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #93.4758
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc2 #95.23189 #df=8

#In-situ production model-a
model3 <- lmer(FCH4 ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #42.23088
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #94.46175
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc3 #95.16764 #df=5

#In-situ production model-b
model4 <- lmer(FCH4 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) +  (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #41.8385
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #105.677
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc4 #109.0188 #df=11

#In-situ respiration model-a
model5 <- lmer(FCH4 ~ HR_SUNSET + (1|SITE) +  (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #42.57673
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #95.15347
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc5 #95.85935 #df=5

#In-situ respiration model-b
model6 <- lmer(FCH4 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) +  (0 + HR_SUNSET|SITE) +  (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL #40.13773
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #102.2755
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCH4) - npar - 1)) 
AICc6 #105.6172 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #0
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #10.95906
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #10.8948
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #24.74595
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #11.58652
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #21.34441

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9885777
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.004123651
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.004258287
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #4.183088e-06
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.00301322
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #2.291564e-05
0.989 + 0.004 + 0.004 + 0.000 + 0.003 + 0.000


#####################################################################
## CH4 Ebullition Linear Mixed Effects Modeling - Floodplain Ponds ##
#####################################################################

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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "EBULL", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#EBULL in mg CH4 m-2 h-1 #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$EBULL) #n=45

#Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$EBULL) #n=31

#Null model
model1 <- lmer(EBULL ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
coef(model1)
NLL <- logLik(model1) * -1
NLL #118.0482
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #244.0963
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc1 #245.6348 #df=4

#Terrestrial-aquatic coupling model
model2 <- lmer(EBULL ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model2)
coef(model2)
NLL <- logLik(model2) * -1
NLL #106.1545
npar <- 13
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #238.309
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc2 #259.7207 #df=13

#In-situ production model-a
model3 <- lmer(EBULL ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
coef(model3)
NLL <- logLik(model3) * -1
NLL #117.2251
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #248.4502
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc3 #253.3198 #df=7

#In-situ production model-b
model4 <- lmer(EBULL ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
coef(model4)
NLL <- logLik(model4) * -1
NLL #113.0067
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #258.0134
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc4 #296.8706 #df=16

#In-situ respiration model-a
model5 <- lmer(EBULL ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
coef(model5)
NLL <- logLik(model5) * -1
NLL #117.6896
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #249.3792
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc5 #254.2488 #df=7

#In-situ respiration model-b
model6 <- lmer(EBULL ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|SITE) + (0 + WATER_T|MONTH) + (0 + AOU|SITE) + (0 + AOU|MONTH) + (0 + PH|SITE) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL #109.1243
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #250.2486
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc6 #289.1058 #df=16

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #0
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #14.08594
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #7.684993
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #51.23575
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #8.613997
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #43.47097

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.965449
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.000843349
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.02069924
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #7.228167e-12
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0130084
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #3.508551e-10


###########################################################################
## CH4 Ebullition Linear Mixed Effects Modeling - Three Gorges Reservoir ##
###########################################################################

rm(list=ls())
setwd("~/Desktop")

#Compare funnel v. chamber ebullition
funnel <- c(0.002,0.002, 0.003)
chamber <- c(0.611, 0.511, 0.145)
ebullition <- chamber <- c(0.611, 0.511, 0.145, 0.611, 0.511, 0.145)
wilcox.test(funnel, chamber) #p-value > 0.001
boxplot(funnel, chamber) #Higher ebullition in chamber
mean(funnel) #0.0023 +/- 0.0003
mean(chamber) #0.4 +/- 0.1
sd(funnel)/(sqrt(3))
sd(chamber)/(sqrt(3))
mean(ebullition) #0.42 +/- 0.09
sd(ebullition)/(sqrt(6))

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)

#Load the packages
#install.packages("lme4") #For mixed effects function
library(lme4)

#Make a dataframe with complete 24 h data
colnames(TGR_All)
TGR_All$SEASON
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "EBULL", "PRECIP", "WATER_T", "CHL", "AOU", "O2_CONC", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#EBULL in mg CH4 m-2 h-1 #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$EBULL) #n=45

#Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$EBULL) #n=14

#Null model
model1 <- lmer(EBULL ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #-0.9173749
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #4.16525
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc1 #6.56525 #df=3

#Terrestrial-aquatic coupling model
model2 <- lmer(EBULL ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #-0.6739782 
npar <- 8
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #14.65204
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc2 #43.45204 #df=8

#In-situ production model-a
model3 <- lmer(EBULL ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #2.321845 
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #14.64369
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc3 #22.14369 #df=5

#In-situ production model-b
model4 <- lmer(EBULL ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #2.173829
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #26.34766
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc4 #158.3477 #df=11

#In-situ respiration model-a
model5 <- lmer(EBULL ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #1.931079
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #13.86216
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc5 #21.36216 #df=5

#In-situ respiration model-b
model6 <- lmer(EBULL ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL #0.9622708
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #23.92454
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$EBULL) - npar - 1)) 
AICc6 #155.9245

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #0
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #36.88679
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #15.57844
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #151.7824
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #14.79691
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #149.3593

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9989747 
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #9.765379e-09
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0004137512
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.097548e-33
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.000611571
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #3.686349e-33


###########################################################
## FCO2 Linear Mixed Effects Modeling - Floodplain Ponds ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#FCO2 in mg CO2 m-2 h-1 #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FCO2) #n=316

#Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$FCO2) #n=230

#Null model
model1 <- lmer(FCO2 ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #1426.472
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #2860.945
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc1 #2861.123 #df=4

#Terrestrial-aquatic coupling model
model2 <- lmer(FCO2 ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model2)
coef(model2)
NLL <- logLik(model2) * -1
NLL #1405.625
npar <- 13
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #2837.25
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc2 #2838.935 #df=13

#In-situ production model-a
model3 <- lmer(FCO2 ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #1423.42
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #2860.84
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc3 #2861.345 #df=7

#In-situ production model-b
model4 <- lmer(FCO2 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #1409.657
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #2851.313
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc4 #2853.867 #df=16

#In-situ respiration model-a
model5 <- lmer(FCO2 ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #1421.185
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #2856.37
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc5 #2856.874 #df=7

#In-situ respiration model-b
model6 <- lmer(FCO2 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|MONTH) + (0 + AOU|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL #1404.712
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #2841.425
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc6 #2843.979 #df=16

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #22.18732
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #0
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #22.40924
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #14.93174
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #17.93905
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #5.043229

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.406812e-05
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9250196
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.259059e-05
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0005293763
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0001176892
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.07430666
0.000 + 0.925 + 0.000 + 0.001 + 0.000 + 0.074


#################################################################
## FCO2 Linear Mixed Effects Modeling - Three Gorges Reservoir ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FCO2", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#FCO2 in mg CO2 m-2 h-1 #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FCO2) #n=316

#Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$FCO2) #n=86

#Null model
model1 <- lmer(FCO2 ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #454.5371
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #915.0743
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc1 #915.3669 #df=3

#Terrestrial-aquatic coupling model
model2 <- lmer(FCO2 ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #445.8909
npar <- 8
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #907.7818
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc2 #909.6519 #df=8

#In-situ production model-a
model3 <- lmer(FCO2 ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #453.3648
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #916.7297 
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc3 #917.4797 #df=5

#In-situ production model-b
model4 <- lmer(FCO2 ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #441.9683
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #905.9365
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc4 #909.5041 #df=11

#In-situ respiration model-a
model5 <- lmer(FCO2 ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #452.6554
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #915.3107
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc5 #916.0607 #df=5

#In-situ respiration model-b
model6 <- lmer(FCO2 ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL #438.9299
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #899.8597
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FCO2) - npar - 1)) 
AICc6 #903.4273 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #11.93964
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #6.22463
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #14.05237
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #6.076815
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #12.63344 
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #0

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.002327411
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.04053889
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0008092826
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.04364851
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.001645192
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9110307
0.002 + 0.040 + 0.001 + 0.044 + 0.002 + 0.911


###########################################################
## FN2O Linear Mixed Effects Modeling - Floodplain Ponds ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#FN2O in mg N2O m-2 h-1 #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FN2O) #n=330

#Subset summer
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Summer")
length(TGR_All_24h$FN2O) #n=236

#Null model
model1 <- lmer(FN2O ~ (1|SITE) + (1|MONTH), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #-269.1221
npar <- 4
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #-530.2442
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc1 #-530.071 #df=4

#Terrestrial-aquatic coupling model
model2 <- lmer(FN2O ~ COND + PRECIP + PH + (1|SITE) + (1|MONTH) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE) + (0 + COND|MONTH) + (0 + PRECIP|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #-261.5035
npar <- 13
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #-497.007 
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc2 #-495.3674 #df=13

#In-situ production model-a
model3 <- lmer(FN2O ~ HR_SUNRISE + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + HR_SUNRISE|MONTH), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #-264.381
npar <- 7
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #-514.762
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc3 #-514.2708 #df=7

#In-situ production model-b
model4 <- lmer(FN2O ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (1|MONTH) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE) + (0 + HR_SUNRISE|MONTH) + (0 + WATER_T|MONTH) + (0 + O2_CONC|MONTH) + (0 + CHL|MONTH), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #-249.2089
npar <- 16
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #-466.4179
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc4 #-463.9339 #df=16

#In-situ respiration model-a
model5 <- lmer(FN2O ~ HR_SUNSET + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + HR_SUNSET|MONTH), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #-264.6507
npar <- 7
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #-515.3014
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc5 #-514.8101 #df=7

#In-situ respiration model-b
model6 <- lmer(FN2O ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (1|MONTH) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE) + (0 + HR_SUNSET|MONTH) + (0 + WATER_T|MONTH) + (0 + AOU|MONTH) + (0 + PH|MONTH), data = TGR_All_24h)
logLik(model6)
coef(model6)
NLL <- logLik(model6) * -1
NLL #-255.0332
npar <- 16
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #-478.0663
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc6 #-475.5823 #df=16

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #0
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #34.70364
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #15.80023
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #66.13717
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #15.26091
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #54.48871

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.9991446
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #2.909574e-08
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0003703847
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #4.34635e-15
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0004850239
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.470806e-12
0.999 + 0.000 + 0.000 + 0.000 + 0.001 + 0.000


#################################################################
## FN2O Linear Mixed Effects Modeling - Three Gorges Reservoir ##
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
TGR_All_24h <- subset(TGR_All, select = c("MONTH", "DAYNIGHT", "SEASON", "SITE", "HR_SUNRISE", "HR_SUNSET", "FN2O", "PRECIP", "WATER_T", "CHL", "O2_CONC", "AOU", "O2_PERCENT",
                                          "PH", "COND"))
colnames(TGR_All_24h)

#Notes on units 
#FN2O in mg N2O m-2 h-1 #AOU in mg O2 L-1 #PRECIP is 1=y 2=n #WATER_T in deg C #CHL in ug chla L-1 #O2_CONC in mg O2 L-1 #COND in mS cm-1

#Make sure that the linear regression matrix is complete by determining which columns have NA values
which(is.na(TGR_All_24h)) #Gives the index of NA values in a vector
nrow(TGR_All_24h)
TGR_All_24h <- na.omit(TGR_All_24h)
nrow(TGR_All_24h)
which(is.na(TGR_All_24h))
length(TGR_All_24h$FN2O) #n=330

#Subset winter
TGR_All_24h <- subset(TGR_All_24h, SEASON=="Winter")
length(TGR_All_24h$FN2O) #n=94

#Null model
length(TGR_All_24h$FN2O)
model1 <- lmer(FN2O ~ (1|SITE), data = TGR_All_24h)
logLik(model1)
NLL <- logLik(model1) * -1
NLL #-36.91274
npar <- 3
AIC <- (-2 * logLik(model1)) + (2 * npar)
AIC #-67.82549
AICc1 <- (-2* logLik(model1)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc1 #-67.55882 #df=3

#Terrestrial-aquatic coupling model
model2 <- lmer(FN2O ~ COND + PRECIP + PH + (1|SITE) + (0 + COND|SITE) + (0 + PRECIP|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model2)
NLL <- logLik(model2) * -1
NLL #-33.86326
npar <- 8
AIC <- (-2 * logLik(model2)) + (2 * npar)
AIC #-51.72652
AICc2 <- (-2* logLik(model2)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc2 #-50.0324 #df=8

#In-situ production model-a
model3 <- lmer(FN2O ~ HR_SUNRISE + (1|SITE) + (0 + HR_SUNRISE|SITE), data = TGR_All_24h)
logLik(model3)
NLL <- logLik(model3) * -1
NLL #-32.65692
npar <- 5
AIC <- (-2 * logLik(model3)) + (2 * npar)
AIC #-55.31385
AICc3 <- (-2* logLik(model3)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc3 #-54.63203 #df=5

#In-situ production model-b
model4 <- lmer(FN2O ~ HR_SUNRISE + WATER_T + O2_CONC + CHL + (1|SITE) + (0 + HR_SUNRISE|SITE) + (0 + WATER_T|SITE) + (0 + O2_CONC|SITE) + (0 + CHL|SITE), data = TGR_All_24h)
logLik(model4)
NLL <- logLik(model4) * -1
NLL #-28.06929
npar <- 11
AIC <- (-2 * logLik(model4)) + (2 * npar)
AIC #-34.13858
AICc4 <- (-2* logLik(model4)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc4 #-30.91907 #df=11

#In-situ respiration model-a
model5 <- lmer(FN2O ~ HR_SUNSET + (1|SITE) + (0 + HR_SUNSET|SITE), data = TGR_All_24h)
logLik(model5)
NLL <- logLik(model5) * -1
NLL #-32.43563 
npar <- 5
AIC <- (-2 * logLik(model5)) + (2 * npar)
AIC #-54.87126
AICc5 <- (-2* logLik(model5)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc5 #-54.18944 #df=5

#In-situ respiration model-b
model6 <- lmer(FN2O ~ HR_SUNSET + WATER_T + AOU + PH + (1|SITE) + (0 + HR_SUNSET|SITE) + (0 + WATER_T|SITE) + (0 + AOU|SITE) + (0 + PH|SITE), data = TGR_All_24h)
logLik(model6)
NLL <- logLik(model6) * -1
NLL #-28.46706
npar <- 11
AIC <- (-2 * logLik(model6)) + (2 * npar)
AIC #-34.93411
AICc6 <- (-2* logLik(model6)) + (2 * npar) + (((2 * npar) * (npar + 1)) / (length(TGR_All_24h$FN2O) - npar - 1)) 
AICc6 #-31.7146 #df=11

#Calculate delAICc
delAICc1 <- AICc1 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc1 #0
delAICc2 <- AICc2 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc2 #17.52642
delAICc3 <- AICc3 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc3 #12.92679
delAICc4 <- AICc4 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc4 #36.63975
delAICc5 <- AICc5 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc5 #13.36938
delAICc6 <- AICc6 - min(c(AICc1, AICc2, AICc3, AICc4, AICc5, AICc6))
delAICc6 #35.84422

#Calculate AICc Weights
exp(-0.5*delAICc1)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.997043
exp(-0.5*delAICc2)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.0001559194
exp(-0.5*delAICc3)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.00155488
exp(-0.5*delAICc4)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.102789e-08
exp(-0.5*delAICc5)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #0.001246207
exp(-0.5*delAICc6)/sum(exp(-0.5*delAICc1), exp(-0.5*delAICc2), exp(-0.5*delAICc3), exp(-0.5*delAICc4), exp(-0.5*delAICc5), exp(-0.5*delAICc6)) #1.6415e-08
0.997 + 0.000 + 0.002 + 0.000 + 0.001 + 0.000

