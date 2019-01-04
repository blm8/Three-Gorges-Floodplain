###########
## NOTES ##
###########

#Last updated 04 Jan 2019 by BLM


######################################
## Figure. Temp v. Henry's Constant ##
######################################

rm(list = ls())
setwd("~/Desktop/TGR")

Fig3 <- read.csv("TvKH.csv", stringsAsFactors = F)
Fig3

#Set standard margins, leaving room for second axis
par(mar=c(5, 4, 4, 6) + 0.25)

#Plot first set of data 
min(Fig3$CH4)
max(Fig3$CH4)

plot(Fig3$degC, Fig3$CH4, pch=16, axes=FALSE, ylim=c(1, 3), xlab=expression("Water Temperature" ~ (degree*C)), ylab="", 
     cex.lab=1.35, type="b", font=2, las=1, cex=1.25, col="black")

#Draw first y axis
mtext(CH[4] ~ "&" ~ O[2] ~ "Solubility" ~ (mmol ~ L^-1 ~ atm^-1), side=2, line=2.5, cex=1.25)
axis(2, ylim=c(1, 3), col="black", las=1, cex=1.25, cex.lab=1.35, font=2)  #las=1 makes horizontal labels

#Plot second set of data on the same graph
par(new=TRUE)

min(Fig3$CO2)
max(Fig3$CO2)
min(Fig3$N2O)
max(Fig3$N2O)

plot(Fig3$degC, Fig3$CO2, pch=15,  xlab="", ylab="", ylim=c(10, 80), 
     axes=FALSE, type="b", cex=1.25, cex.lab=1.35, font=2, col="gray44")

#Draw second y axis
mtext(expression(CO[2] ~ "&" ~ "N"[2]*"O" ~ "Solubility" ~ (mmol ~ L^-1 ~ atm^-1)), side=4, col="gray44", line=3.15, cex=1.25, cex.lab=1.35, font=2)
axis(4, ylim=c(10, 80), col="gray44", col.axis="gray48", cex=1.25, cex.lab=1.35, font=2, las=1)

#Plot third set of data on the same graph
par(new=TRUE)

plot(Fig3$degC, Fig3$N2O, pch=2,  xlab="", ylab="", ylim=c(10, 80), 
     axes=FALSE, type="b", cex=1.15, col="gray44")

#Plot fourth set of data on the same graph
par(new=TRUE)

plot(Fig3$degC, Fig3$O2, pch=5,  xlab="", ylab="", ylim=c(1, 3), 
     axes=FALSE, type="b", cex=1.15, col="black")

#Draw x axis
axis(1, xlim=c(0, 35), col="black", col.axis="black", las=1, cex=1.25, cex.lab=1.35, font=4)

#Add Legend
legend("top", ncol=3, legend=c(expression(CH[4]), expression(O[2]), expression(CO[2]), expression("N"[2]*"O")),
       text.col=c("black", "black", "gray44", "gray44"), bty="n", pch=c(16, 5, 15, 2), cex=c(1.25,1.25,1.25,1.25), pt.cex=c(1.35,1.35,1.35,1.35), col=c("black", "black", "gray44", "gray44"))


###############################################################################
## Table 2. Mean diffusive fluxes and partial pressures for CH4, CO2 and N2O ##
###############################################################################

rm(list=ls())
setwd("~/Desktop/TGR")

#Load data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)
colnames(TGR_All)

Summer <- subset(TGR_All, SEASON=="Summer")
Winter <- subset(TGR_All, SEASON=="Winter")
max(na.omit(Winter$O2_PERCENT))
wilcox.test(Summer$O2_PERCENT, Winter$O2_PERCENT) #W = 7710, p-value = 0.1469
boxplot(Summer$O2_PERCENT, Winter$O2_PERCENT) 

Summer <- subset(TGR_All, SEASON=="Summer")
Summer$DAYNIGHT
Day <- subset(Summer, DAYNIGHT=="DAY")
Night <- subset(Summer, DAYNIGHT=="NIGHT")
wilcox.test(Day$FCH4, Night$FCH4) #W = 7710, p-value = 0.1469
boxplot(Day$FCH4, Night$FCH4) 
wilcox.test(Day$PCH4, Night$PCH4) #W = 187, p-value = 0.4429
boxplot(Day$PCH4, Night$PCH4) 
wilcox.test(Day$FCO2, Night$FCO2) #W = 5759, p-value = 0.09252
boxplot(Day$FCO2, Night$FCO2) 
wilcox.test(Day$PCO2, Night$PCO2) #W = 163, p-value = 0.9875
boxplot(Day$PCO2, Night$PCO2) 
wilcox.test(Day$FN2O, Night$FN2O) #W = 7596, p-value = 0.2087
boxplot(Day$FN2O, Night$FN2O) 
wilcox.test(Day$PN2O, Night$PN2O) #W = 227.5, p-value = 0.0397*
boxplot(Day$PN2O, Night$PN2O) #Significantly higher PN2O during the day

Winter <- subset(TGR_All, SEASON=="Winter")
Winter$DAYNIGHT
Day <- subset(Winter, DAYNIGHT=="DAY")
Night <- subset(Winter, DAYNIGHT=="NIGHT")
wilcox.test(Day$FCH4, Night$FCH4) #W = 804, p-value = 0.06835
boxplot(Day$FCH4, Night$FCH4) #Close to alpha; higher FCH4 during the night
wilcox.test(Day$PCH4, Night$PCH4) #W = 812.5, p-value = 0.06474
boxplot(Day$PCH4, Night$PCH4) #Close to alpha; higher PCH4 during the night
wilcox.test(Day$FCO2, Night$FCO2) #W = 769, p-value = 0.1819
boxplot(Day$FCO2, Night$FCO2) 
wilcox.test(Day$PCO2, Night$PCO2) #W = 1004, p-value = 3.634e-05***
boxplot(Day$PCO2, Night$PCO2) #Significantly higher PCO2 during the day
wilcox.test(Day$FN2O, Night$FN2O) #W = 1166, p-value = 0.6446
boxplot(Day$FN2O, Night$FN2O) 
wilcox.test(Day$PN2O, Night$PN2O) # = 1259, p-value = 6.145e-12***
boxplot(Day$PN2O, Night$PN2O) #Significantly higher PN2O during the day

#Values by month and site
TEMP <- subset(TGR_All, SEASON=="Summer")
plot(TEMP$PH, TEMP$FCH4)
abline(lm(FCH4 ~ PH, data = TEMP))
max(TEMP$WATER_T) #39.377
min(TEMP$WATER_T) #25
TEMP <- subset(TGR_All, SEASON=="Winter")
max(TEMP$WATER_T) #13.637
min(TEMP$WATER_T) #13.283
ENG <- subset(TGR_All, SITE=="ENG")
ENG <- subset(ENG, MONTH==c("AugDry", "AugWet"))
ENG$MONTH
ENG$SITE
FN2O <- na.omit(ENG$FN2O)
length(FN2O)
mean(FN2O)
sd(FN2O)/sqrt(length(FN2O))

NAT <- subset(TGR_All, SITE=="NAT")
NAT
Jun_NAT <- subset(NAT, MONTH=="Jun")
FN2O <- na.omit(Jun_NAT$FN2O)
length(FN2O)
mean(FN2O)
sd(FN2O)/sqrt(length(FN2O))
PN2O <- na.omit(Jun_NAT$PN2O)
length(PN2O)
mean(PN2O)
sd(PN2O)/sqrt(length(PN2O))
EBULL <- na.omit(Jun_NAT$EBULL)
length(EBULL)
mean(EBULL)
sd(EBULL)/sqrt(length(EBULL))

ENG <- subset(TGR_All, SITE=="ENG")
ENG
Jun_ENG <- subset(ENG, MONTH=="Jun")
FN2O <- na.omit(Jun_ENG$FN2O)
length(FN2O)
mean(FN2O)
sd(FN2O)/sqrt(length(FN2O))
PN2O <- na.omit(Jun_ENG$PN2O)
length(PN2O)
mean(PN2O)
sd(PN2O)/sqrt(length(PN2O))
EBULL <- na.omit(Jun_ENG$EBULL)
length(EBULL)
mean(EBULL)
sd(EBULL)/sqrt(length(EBULL))


######################################################################################
## Figure S2. Saturation of O2 and CO2 in water relative to atmospheric equilibrium ##
######################################################################################

rm(list=ls())
setwd("~/Desktop/TGR")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)
colnames(TGR_All)

#Subset summer data in natural pond
TGR_All$SEASON
Natural <- subset(TGR_All, SEASON=="Summer")
Natural$SITE
Natural <- subset(Natural, SITE=="NAT")
colnames(Natural)
Natural <- subset(Natural, select = c("EXC_O2", "EXC_CO2"))
colnames(Natural)
nrow(Natural)
Natural <- na.omit(Natural)
nrow(Natural)
Natural

#Subset summer data in aquaculture pond
TGR_All$SEASON
Aquaculture <- subset(TGR_All, SEASON=="Summer")
Aquaculture <- subset(Aquaculture, SITE=="ENG")
colnames(Aquaculture)
Aquaculture <- subset(Aquaculture, select = c("EXC_O2", "EXC_CO2"))
colnames(Aquaculture)
nrow(Aquaculture)
Aquaculture <- na.omit(Aquaculture)
nrow(Aquaculture)
Aquaculture

#Subset winter data in Three Gorges Reservoir
TGR <- subset(TGR_All, SEASON=="Winter")
TGR <- subset(TGR, select = c("EXC_O2", "EXC_CO2"))
colnames(TGR)
nrow(TGR)
TGR <- na.omit(TGR)
nrow(TGR)
TGR

par(mar=c(5, 5, 2, 2)) #bottom, left, top, right
max(na.omit(TGR_All$EXC_CO2)) #52.157
min(na.omit(TGR_All$EXC_CO2)) #4.343
max(na.omit(TGR_All$EXC_O2)) #22.948
min(na.omit(TGR_All$EXC_O2)) #-179.343
plot(jitter(Natural$EXC_CO2, factor=4), Natural$EXC_O2, pch=24, col="deepskyblue4", bg="deepskyblue2", xlim=c(-20, 100), ylim=c(-200, 50), xlab=expression("Excess" ~ CO[2] ~ (mmol ~ L^{-1})), ylab=expression("Excess" ~ O[2] ~ (mmol ~ L^{-1})), frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
points(jitter(Aquaculture$EXC_CO2, factor=4), Aquaculture$EXC_O2, pch=22, col="red3", bg="red", cex=1.25)
points(jitter(TGR$EXC_CO2, factor=4), TGR$EXC_O2, pch=21, col="navy", bg="navyblue", cex=1.25)
abline(h=0, col="gray", lty=1, lwd=1)
abline(v=0, col="gray", lty=1, lwd=1)
abline(0, -1, lty=2, lwd=2, col="black")
text(locator(1),"1:1 Line", col="black", cex=1.35) 
summary(lm(Natural$EXC_O2~Natural$EXC_CO2))
text(locator(1), "Slope = -1.21", col="deepskyblue2", cex=1.35) 
summary(lm(Aquaculture$EXC_O2~Aquaculture$EXC_CO2))
text(locator(1), "Slope = -0.95", col="red", cex=1.35) 
summary(lm(TGR$EXC_O2~TGR$EXC_CO2))
text(locator(1), "Slope = 0.38", col="navyblue", cex=1.35)
legend("topright",legend=c("Natural Pond", "Aquaculture Pond", "Three Gorges Reservoir"), pch=c(17, 15, 16), cex=c(1.25,1.25,1.25), pt.cex=c(1.35,1.35,1.35), col=c("deepskyblue2", "red", "navyblue"), bty="n")


###################################################################################
## Figure 3. Fluxes of CH4, CO2, and N2O expressed in mg CO2-equivalents m-2 h-1 ##
###################################################################################

rm(list=ls())
setwd("~/Desktop/TGR")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)
colnames(TGR_All)

#Values by month
par(mfrow=c(2,2))
JUN <- subset(TGR_All, MONTH=="Jun")
JUN
CH4 <- (na.omit(JUN$FCH4) * 25) 
max(CH4) #428.625
min(CH4) #-4.025
EBULL <- (na.omit(JUN$EBULL) * 25)
max(EBULL) #1027.715
min(EBULL) #4.013465
CO2 <- (na.omit(JUN$FCO2))
max(CO2) #514.02
min(CO2) #-346.513
N2O <- (na.omit(JUN$FN2O) * 298)
max(N2O) #51.554
min(N2O) #-55.428
boxplotLab <- c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O))
par(mar=c(1, 5, 5, 1)) 
boxplot(CH4, EBULL, CO2, N2O, ylim=c(-500, 1000), xaxt="n", frame=FALSE, names=boxplotLab, col="gray", font.axis=2, las=1, cex=1.25, varwidth=TRUE, boxwex=0.5, 
        xlab=expression("June"), outline=FALSE, ylab=expression("Flux" ~ (mg ~ CO[2]-"Eq" ~ m^{-2} ~ h^{-1})), cex.lab=1.35)
abline(h=0, col="deepskyblue2")
mtext("June", side=3, cex=1.15)

AUGDRY <- subset(TGR_All, MONTH=="AugDry")
AUGDRY
CH4 <- (na.omit(AUGDRY$FCH4) * 25)
max(CH4) #31.767
EBULL <- (na.omit(AUGDRY$EBULL) * 25)
max(EBULL) #20.86486
CO2 <- (na.omit(AUGDRY$FCO2))
mean(CO2) #0.1528571
N2O <- (na.omit(AUGDRY$FN2O) * 298)
max(N2O) #3.890688
boxplotLab <- c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O))
par(mar=c(1, 1, 5, 2)) 
boxplot(CH4, EBULL, CO2, N2O, ylim=c(-500, 1000), xaxt="n", frame=FALSE, names=boxplotLab, col="gray", font.axis=2, las=1, cex=1.25, varwidth=TRUE, boxwex=0.5, 
        xlab=expression("August During Rain"), outline=FALSE, yaxt="n", ylab="", cex.lab=1.35)
abline(h=0, col="deepskyblue2")
mtext("August During Rain", side=3, cex=1.15)

AUGWET <- subset(TGR_All, MONTH=="AugWet")
AUGWET
CH4 <- (na.omit(AUGWET$FCH4) * 25)
max(CH4) #19.383
EBULL <- (na.omit(AUGWET$EBULL) * 25)
max(EBULL) #11.40717
CO2 <- (na.omit(AUGWET$FCO2))
mean(CO2) #0.3851517
N2O <- (na.omit(AUGWET$FN2O) * 298)
max(N2O) #2.460288
boxplotLab <- c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O))
par(mar=c(5, 5, 1, 1)) 
boxplot(CH4, EBULL, CO2, N2O, ylim=c(-500, 1000), frame=FALSE, names=boxplotLab, col="gray", font.axis=2, las=1, cex=1.25, varwidth=TRUE, boxwex=0.5, 
        xlab=expression("August After Rain"), outline=FALSE, ylab=expression("Flux" ~ (mg ~ CO[2]-"Eq" ~ m^{-2} ~ h^{-1})), cex.lab=1.35)
abline(h=0, col="deepskyblue2")

JAN <- subset(TGR_All, MONTH=="Jan")
JAN
CH4 <- (na.omit(JAN$FCH4) * 25)
max(CH4) #1.6716
EBULL <- (na.omit(JAN$EBULL) * 25)
max(EBULL) #11.40717
CO2 <- (na.omit(JAN$FCO2))
mean(CO2) #0.3851517
N2O <- (na.omit(JAN$FN2O) * 298)
max(N2O) #3.769104
boxplotLab <- c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O))
par(mar=c(5, 1, 1, 2)) 
boxplot(CH4, EBULL, CO2, N2O, ylim=c(-500, 1000), frame=FALSE, names=boxplotLab, font.axis=2, las=1, cex=1.25, varwidth=TRUE, boxwex=0.5, 
        xlab=expression("January"), yaxt="n", ylab="", outline=FALSE, cex.lab=1.35)
abline(h=0, col="deepskyblue2")



#######################################################################
## Figure 4. Diel variation in dissolved O2 and diffusive CH4 fluxes ##
#######################################################################

rm(list=ls())
setwd("~/Desktop/TGR")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)
colnames(TGR_All)

par(mfrow=c(1,4))
min(na.omit(TGR_All$FCH4)) #-0.742
max(na.omit(TGR_All$FCH4)) #52.945
#Tile 1
par(mar=c(5, 6, 2, 1)) #bottom, left, top, right
Jun <- subset(TGR_All, MONTH=="Jun")
Jun_NAT <- subset(Jun, SITE=="NAT")
Jun_ENG <- subset(Jun, SITE=="ENG")
plot(as.numeric(Jun_NAT$TIME), Jun_NAT$O2_CONC, pch=24, col="deepskyblue4", bg="deepskyblue2", xaxt="n", ylim=c(0, 12), xlab="June", ylab=expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
points(as.numeric(Jun_ENG$TIME), Jun_ENG$O2_CONC, pch=22, col="red3", bg="red", cex=1.25)
par(new=TRUE)
plot(as.numeric(Jun_NAT$TIME), jitter(Jun_NAT$FCH4, factor=8), pch=2, col="dimgray", xaxt="n", ylim=c(0, 40), axes=FALSE, xlab="", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
par(new=TRUE)
plot(as.numeric(Jun_ENG$TIME), jitter(Jun_ENG$FCH4, factor=8), pch=0, col="dimgray", xaxt="n", ylim=c(0, 40), axes=FALSE, xlab="", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
axis(side=1, at=c(4, 8, 12, 16, 20, 24), labels=c("10:00", "14:00", "18:00", "22:00", "4:00", "8:00"), font.axis=2, las=1)
legend("topright",legend=c(expression("Natural" ~ "Dissolved" ~ O[2]), expression("Aquaculture" ~ "Dissolved" ~ O[2]), expression("Natural" ~ "Diffusive" ~ CH[4] ~ "Flux"), expression("Aquaculture" ~ "Diffusive" ~ CH[4] ~ "Flux")), pch=c(17,15,2,0), cex=c(1.25,1.25,1.25,1.25), pt.cex=c(1.35,1.35,1.35,1.35), col=c("deepskyblue4", "red", "dimgray", "dimgray"), bty="n", text.col=c("black", "black", "dimgray", "dimgray"))
#Tile 2
par(mar=c(5, 2, 2, 1)) #bottom, left, top, right
AugDry <- subset(TGR_All, MONTH=="AugDry")
AugDry_NAT <- subset(AugDry, SITE=="NAT")
AugDry_ENG <- subset(AugDry, SITE=="ENG")
plot(as.numeric(AugDry_NAT$TIME), AugDry_NAT$O2_CONC, pch=24, col="deepskyblue4", bg="deepskyblue2", ylim=c(0, 12), xlab="August During Rain", xaxt="n", ylab="", yaxt="n", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
points(as.numeric(AugDry_ENG$TIME), AugDry_ENG$O2_CONC, pch=22, col="red3", bg="red", cex=1.25)
par(new=TRUE)
plot(as.numeric(AugDry_NAT$TIME), jitter(AugDry_NAT$FCH4, factor=8), pch=2, col="dimgray", xaxt="n", ylim=c(0, 40), axes=FALSE, xlab="", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
par(new=TRUE)
plot(as.numeric(AugDry_ENG$TIME), jitter(AugDry_ENG$FCH4, factor=8), pch=0, col="dimgray", xaxt="n", ylim=c(0, 40), axes=FALSE, xlab="", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
axis(side=1, at=c(4, 8, 12, 16, 20, 24), labels=c("10:00", "14:00", "18:00", "22:00", "4:00", "8:00"), font.axis=2, las=1)
#Tile 3
par(mar=c(5, 2, 2, 1)) #bottom, left, top, right
AugWet <- subset(TGR_All, MONTH=="AugWet")
AugWet_NAT <- subset(AugWet, SITE=="NAT")
AugWet_ENG <- subset(AugWet, SITE=="ENG")
plot(as.numeric(AugWet_NAT$TIME), AugWet_NAT$O2_CONC, pch=24, col="deepskyblue4", bg="deepskyblue2", ylim=c(0, 12), xlab="August After Rain", xaxt="n", yaxt="n", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
points(as.numeric(AugWet_ENG$TIME), AugWet_ENG$O2_CONC, pch=22, col="red3", bg="red", cex=1.25)
par(new=TRUE)
plot(as.numeric(AugWet_NAT$TIME), jitter(AugWet_NAT$FCH4, factor=8), pch=2, col="dimgray", xaxt="n", ylim=c(0, 40), axes=FALSE, xlab="", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
par(new=TRUE)
plot(as.numeric(AugWet_ENG$TIME), jitter(AugWet_ENG$FCH4, factor=8), pch=0, col="dimgray", xaxt="n", ylim=c(0, 40), axes=FALSE, xlab="", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
axis(side=1, at=c(4, 8, 12, 16, 20, 24), labels=c("10:00", "14:00", "18:00", "22:00", "4:00", "8:00"), font.axis=2, las=1)
#Tile 4
par(mar=c(5, 2, 2, 6)) #bottom, left, top, right
Jan <- subset(TGR_All, MONTH=="Jan")
Jan_NAT <- subset(Jan, SITE=="NAT")
Jan_ENG <- subset(Jan, SITE=="ENG")
plot(as.numeric(Jan_NAT$TIME), Jan_NAT$O2_CONC, pch=24, col="deepskyblue4", bg="deepskyblue2", ylim=c(0, 12), xlab="January", xaxt="n", yaxt="n", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
points(as.numeric(Jan_ENG$TIME), Jan_ENG$O2_CONC, pch=22, col="red3", bg="red", cex=1.25)
par(new=TRUE)
plot(as.numeric(Jan_NAT$TIME), jitter(Jan_NAT$FCH4, factor=8), pch=2, col="dimgray", xaxt="n", ylim=c(0, 40), axes=FALSE, xlab="", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
par(new=TRUE)
plot(as.numeric(Jan_ENG$TIME), jitter(Jan_ENG$FCH4, factor=8), pch=0, col="dimgray", xaxt="n", ylim=c(0, 40), axes=FALSE, xlab="", ylab="", frame=FALSE, font.axis=2, las=1, cex.lab=1.35, cex=1.25)
axis(side=1, at=c(4, 8, 12, 16, 20, 24), labels=c("10:00", "14:00", "18:00", "22:00", "4:00", "8:00"), font.axis=2, las=1)
axis(side=4, col.axis="dimgray", col.lab="dimgray", font.axis=2, las=1)
mtext(expression("Diffusive" ~ CH[4] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side = 4, cex=1, col="dimgray", padj=2.2)


##########################################################
## Figure S3a. LME model regression for floodplain FCH4 ##
##########################################################

rm(list=ls())
setwd("~/Desktop/TGR")

#Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)
colnames(TGR_All)
CH4 <- subset(TGR_All, SEASON=="Summer")
par(mfrow=c(1,2))
par(mar=c(2, 5, 2, 2)) #bottom, left, top, right
plot(CH4$COND, CH4$FCH4, type="n", bty="n", xaxt="n", xlab=expression("x"), ylab=expression(CH[4] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), ylim=c(0,20), font.axis=2, las=1, cex.lab=1.35, cex=1.25)
abline(lm(FCH4 ~ COND, data=CH4), col="deepskyblue2", lty=1, lwd=2)
par(new=T)
plot(CH4$PH, CH4$FCH4, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0,20))
abline(lm(FCH4 ~ PH, data=CH4), col="deepskyblue2", lty=3, lwd=2)
par(new=T)
plot(CH4$PRECIP, CH4$FCH4, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0,20))
abline(lm(FCH4 ~ PRECIP, data=CH4), col="deepskyblue2", lty=5, lwd=2)
legend("topright",legend=c("Conductivity", "Precipitation", "pH", "Hours Since Sunset", "AOU", "Water Temperature", "Floodplain Ponds", "Three Gorges Reservoir"), lty=c(1,5,3,2,4,6,1,1), lwd=c(2,2,2,2,2,2,2,2), col=c("darkgray", "darkgray", "darkgray", "darkgray", "darkgray", "darkgray", "deepskyblue2", "navyblue"), cex=c(1.25,1.25,1.25,1.25,1.25,1.25,1.25,1.25), bty="n")
mtext(expression(x[i]), side=1, font=3, font.axis=2, las=1, cex.lab=1.35, cex=1.25)


########################################################################
## Figure S3b. LME model regression for floodplain and reservoir FCO2 ##
########################################################################

CO2 <- subset(TGR_All, SEASON=="Summer")
par(mar=c(2, 5, 2, 2)) #bottom, left, top, right
plot(CO2$COND, CO2$FCO2, type="n", bty="n", xaxt="n", xlab=expression("x"), ylab=expression(CO[2] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), ylim=c(0,120), font.axis=2, las=1, cex.lab=1.35, cex=1.25)
abline(lm(FCO2 ~ COND, data=CO2), col="deepskyblue2", lty=1, lwd=2)
par(new=T)
plot(CO2$PH, CO2$FCO2, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0,120))
abline(lm(FCO2 ~ PH, data=CH4), col="deepskyblue2", lty=3, lwd=2)
par(new=T)
plot(CO2$PRECIP, CO2$FCO2, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0,120))
abline(lm(FCO2 ~ PRECIP, data=CO2), col="deepskyblue2", lty=5, lwd=2)
mtext(expression(x[i]), side=1, font=3, font.axis=2, las=1, cex.lab=1.35, cex=1.25)

#Winter reservoir
CO2 <- subset(TGR_All, SEASON=="Winter")
par(new=T)
plot(CO2$HR_SUNSET, CO2$FCO2, type="n", bty="n", xaxt="n", xlab="", yaxt="n", ylab="", ylim=c(0,120))
abline(lm(FCO2 ~ HR_SUNSET, data=CO2), col="navyblue", lty=2, lwd=2)
par(new=T)
plot(CO2$O2_CONC, CO2$FCO2, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0,120))
abline(lm(FCO2 ~ AOU, data=CO2), col="navyblue", lty=4, lwd=2)
par(new=T)
plot(CO2$WATER_T, CO2$FCO2, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0,120))
abline(lm(FCO2 ~ WATER_T, data=CO2), col="navyblue", lty=6, lwd=2)
par(new=T)
plot(CO2$PH, CO2$FCO2, type="n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="", ylim=c(0,120))
abline(lm(FCO2 ~ PH, data=CO2), col="navyblue", lty=3, lwd=2)


####################################################################################################################################################################################################
## Figure 6. Relative contributions of diffusive CH4 emissions by the aquatic and terrestrial environments on the Three Gorges Floodplain, the Yangtze River, and its tributaries during drawdown ##
####################################################################################################################################################################################################

rm(list=ls())
setwd("~/Desktop/TGR")

#Load the data
PAIR <- read.csv("LIT_Pairwise.csv", header=TRUE)
colnames(PAIR)

#Compare values
wilcox.test(PAIR$POND, PAIR$WETLAND) #p-value = 0.69  
boxplot(PAIR$POND, PAIR$WETLAND)
t.test(PAIR$POND, PAIR$WETLAND, var.equal = T, conf.level = 0.95) #p-value = 0.3424, df = 22
wilcox.test(LIT_Summer$CO2EQ, LIT_Winter$CO2EQ) 
boxplot(LIT_Summer$CO2EQ, LIT_Winter$CO2EQ)
t.test(LIT_Summer$CO2EQ, LIT_Winter$CO2EQ, var.equal = T, conf.level = 0.95) #p-value = 0.01864, df = 39

#Load the data
LIT <- read.csv("LIT.csv", header=TRUE)
colnames(LIT)
LIT$ENV
par(mfrow=c(1,2))

#Tile 1
par(mar=c(2, 4, 2, 1)) #bottom, left, top, right
LIT_Summer <- subset(LIT, SEASON=="Summer")
LIT_Summer$ENV
Pond <- subset(LIT_Summer, ENV=="PondWetland")
Grassland <- subset(LIT_Summer, ENV=="Grassland")
Forest <- subset(LIT_Summer, ENV=="Forest")
Agriculture <- subset(LIT_Summer, ENV=="Agriculture")
YangtzeTrib  <- subset(LIT_Summer, ENV=="YangtzeTrib")
x <- c(mean(na.omit(Pond$CO2EQ))*100.0, mean(na.omit(Grassland$CO2EQ))*15.1, mean(na.omit(Forest$CO2EQ))*63.8, mean(na.omit(Agriculture$CO2EQ))*89.6)
x
labels <- c("Ponds & Wetlands", "Grasslands", "Forests", "Agriculture")
labels
percentlabels <- round(100*x/sum(x), 1)
pielabels <- paste(percentlabels, "%", sep="")
pie(x, pielabels, col=c("deepskyblue2", "red", "yellow1", "chartreuse1"), border="white")
legend("top", labels, pch=c(15,15,15,15), col=c("deepskyblue2", "red", "yellow1", "chartreuse1"), cex=c(1.25,1.25,1.25,1.25), pt.cex=c(1.35,1.35,1.35,1.35), bty="n")

#Tile 2
par(mar=c(2, 1, 2, 4)) #bottom, left, top, right
LIT_Winter <- subset(LIT, SEASON=="Winter")
LIT_Winter$ENV
Reservoir <- subset(LIT_Winter, ENV=="Reservoir")
Reservoir
Submerged <- subset(LIT_Winter, ENV=="Submerged")
Submerged
x1 <- c(mean(na.omit(Reservoir$CO2EQ))*784.8, mean(na.omit(Submerged$CO2EQ))*321.4)
x1
labels1 <- c("Mainstem Reservoir", "Submerged Floodplain")
labels1
percentlabels1 <- round(100*x1/sum(x1), 1)
pielabels1 <- paste(percentlabels1, "%", sep="")
pie(x1, pielabels1, col=c("navyblue", "cadetblue1"), border="white")
legend("top", labels1, pch=c(15,15), col=c("navyblue", "cadetblue1"), cex=c(1.25,1.25), pt.cex=c(1.35,1.35), bty="n")


