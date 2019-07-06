###################################################################################
## Figure 2. Fluxes of CH4, CO2, and N2O expressed in mg CO2-equivalents m-2 h-1 ##
###################################################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)
colnames(TGR_All)

# Values by month
par(mfrow=c(1,4))
par(oma=c(1, 8, 1, 1))
JUN <- subset(TGR_All, MONTH=="Jun")
JUN
CH4 <- (na.omit(JUN$FCH4) * 25) 
max(CH4) # 428.625
min(CH4) # -4.025
EBULL <- (na.omit(JUN$EBULL) * 25)
max(EBULL) # 1027.715
min(EBULL) # 4.013465
CO2 <- (na.omit(JUN$FCO2))
max(CO2) # 514.02
min(CO2) # -346.513
N2O <- (na.omit(JUN$FN2O) * 298)
max(N2O) # 51.554
min(N2O) # -55.428
boxplotLab <- c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O))
par(mar=c(8, 0.25, 2, 0.25))
boxplot(CH4, EBULL, CO2, N2O, ylim=c(-500, 1100), axes=F, frame=T, names=boxplotLab, col="white", font.axis=2, las=1, cex=1.25, cex.lab=1.25, varwidth=TRUE, boxwex=0.5, 
        xlab="", ylab="", cex.lab=1.5, cex.font=2)
abline(h=0, col="dimgray", lty=1, lwd=2)
axis(2, cex.axis=2, cex=1.75)
mtext(expression("Flux" ~ (mg ~ CO[2] ~ m^{-2} ~ h^{-1})), side=2, cex=1.35, line=4)
axis(1, cex.axis=2, cex=1.5, at=c(1, 2, 3, 4), labels=c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O)), tick=F, las=3)
mtext("June", side=3, cex=1.25, font=2, line=-2)
# text(x=2, y=-175, "n=13", pos=3, cex=1.5, font=3)

AUGDRY <- subset(TGR_All, MONTH=="AugDry")
AUGDRY
CH4 <- (na.omit(AUGDRY$FCH4) * 25)
max(CH4) # 31.767
EBULL <- (na.omit(AUGDRY$EBULL) * 25)
max(EBULL) # 20.86486
CO2 <- (na.omit(AUGDRY$FCO2))
mean(CO2) # 0.1528571
N2O <- (na.omit(AUGDRY$FN2O) * 298)
max(N2O) # 3.890688
boxplotLab <- c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O))
par(mar=c(8, 0.25, 2, 0.25)) 
boxplot(CH4, EBULL, CO2, N2O, ylim=c(-500, 1100), axes=F, frame=T, names=boxplotLab, col="white", font.axis=2, las=1, cex=1.25, cex.lab=1.25, varwidth=TRUE, boxwex=0.5, 
        xlab="", yaxt="n", ylab="", cex.lab=1.5)
abline(h=0, col="dimgray", lty=1, lwd=2)
axis(1, cex.axis=2, cex=1.5, at=c(1, 2, 3, 4), labels=c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O)), tick=F, las=3)
mtext("August During Rain", side=3, cex=1.25, font=2, line=-2)
text(x=1, y=-310, "***", pos=3, cex=1.75, font=4)
text(x=1, y=-410, "d=1.1", pos=3, cex=1.5, font=4)
text(x=1, y=-350, "p<0.001", pos=3, cex=1.5, font=4)
text(x=2, y=-350, "NC", pos=3, cex=1.5, font=4)
text(x=3, y=-310, "***", pos=3, cex=1.75, font=4)
text(x=3, y=-410, "d=0.8", pos=3, cex=1.5, font=4)
text(x=3, y=-350, "p<0.001", pos=3, cex=1.5, font=4)

AUGWET <- subset(TGR_All, MONTH=="AugWet")
AUGWET
CH4 <- (na.omit(AUGWET$FCH4) * 25)
max(CH4) # 19.383
EBULL <- (na.omit(AUGWET$EBULL) * 25)
max(EBULL) # 11.40717
CO2 <- (na.omit(AUGWET$FCO2))
mean(CO2) # 0.3851517
N2O <- (na.omit(AUGWET$FN2O) * 298)
max(N2O) # 2.460288
boxplotLab <- c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O))
par(mar=c(8, 0.25, 2, 0.25)) 
boxplot(CH4, EBULL, CO2, N2O, ylim=c(-500, 1100), axes=F, frame=T, names=boxplotLab, col="white", font.axis=2, las=1, cex=1.25, cex.lab=1.25, varwidth=TRUE, boxwex=0.5, 
        xlab="", ylab="", cex.lab=1.5, cex.font=2)
abline(h=0, col="dimgray", lty=1, lwd=2)
axis(1, cex.axis=2, cex=1.5, at=c(1, 2, 3, 4), labels=c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O)), tick=F, las=3)
mtext("August After Rain", side=3, cex=1.25, font=2, line=-2)
text(x=1, y=-310, "***", pos=3, cex=1.75, font=4)
text(x=1, y=-410, "d=0.9", pos=3, cex=1.5, font=4)
text(x=1, y=-350, "p<0.001", pos=3, cex=1.5, font=4)
text(x=2, y=-350, "NC", pos=3, cex=1.5, font=4)

JAN <- subset(TGR_All, MONTH=="Jan")
JAN
CH4 <- (na.omit(JAN$FCH4) * 25)
max(CH4) # 1.6716
EBULL <- (na.omit(JAN$EBULL) * 25)
max(EBULL) # 11.40717
CO2 <- (na.omit(JAN$FCO2))
mean(CO2) # 0.3851517
N2O <- (na.omit(JAN$FN2O) * 298)
max(N2O) # 3.769104
boxplotLab <- c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O))
par(mar=c(8, 0.25, 2, 0.25)) 
boxplot(CH4, EBULL, CO2, N2O, ylim=c(-500, 1100), axes=F, frame=T, names=boxplotLab, col="white", font.axis=2, las=1, cex=1.25, varwidth=TRUE, boxwex=0.5, 
        xlab="", yaxt="n", ylab="",cex.lab=1.5, cex.font=2)
abline(h=0, col="dimgray", lty=1, lwd=2)
axis(1, cex.axis=2, cex=1.5, at=c(1, 2, 3, 4), labels=c(expression(CH[4]), expression("Ebullition"), expression(CO[2]), expression(N[2]*O)), tick=F, las=3)
mtext("January", side=3, cex=1.25, font=2, line=-2)
text(x=1, y=-310, "**", pos=3, cex=1.5, font=4)
text(x=1, y=-410, "d=0.7", pos=3, cex=1.5, font=4)
text(x=1, y=-350, "p<0.001", pos=3, cex=1.5, font=4)
text(x=2, y=-310, "***", pos=3, cex=1.5, font=4)
text(x=2, y=-410, "d=1.4", pos=3, cex=1.5, font=4)
text(x=2, y=-350, "p<0.001", pos=3, cex=1.5, font=4)


######################################################################################
## Figure 3. Saturation of O2 and CO2 in water relative to atmospheric equilibrium ##
######################################################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)
colnames(TGR_All)

# Subset summer data in natural pond
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

# Subset summer data in aquaculture pond
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

# Subset winter data in Three Gorges Reservoir
TGR <- subset(TGR_All, SEASON=="Winter")
TGR <- subset(TGR, select = c("EXC_O2", "EXC_CO2"))
colnames(TGR)
nrow(TGR)
TGR <- na.omit(TGR)
nrow(TGR)
TGR

par(mar=c(5, 6, 2, 2)) 
par(oma=c(1, 3, 1, 1))
max(na.omit(TGR_All$EXC_CO2)) # 52.157
min(na.omit(TGR_All$EXC_CO2)) # 4.343
max(na.omit(TGR_All$EXC_O2)) # 22.948
min(na.omit(TGR_All$EXC_O2)) # -179.343
plot(jitter(Natural$EXC_CO2, factor=4), Natural$EXC_O2, pch=24, col="white", bg="deepskyblue2", xlim=c(-20, 100), ylim=c(-200, 50), axes=F, xlab="", ylab="", frame=T, font.axis=2, las=1, cex.lab=2, cex=2)
points(jitter(Aquaculture$EXC_CO2, factor=4), Aquaculture$EXC_O2, pch=22, col="white", bg="red", cex=2)
points(jitter(TGR$EXC_CO2, factor=4), TGR$EXC_O2, pch=21, col="white", bg="navyblue", cex=2)
axis(2, cex.axis=2, cex=1.75)
mtext(expression("Excess" ~ O[2] ~ (mmol ~ L^{-1})), side=2, cex=2, line=4)
axis(1, cex.axis=2, cex=1.75)
mtext(expression("Excess" ~ CO[2] ~ (mmol ~ L^{-1})), side=1, cex=2, line=4)
abline(h=0, col="dimgray", lty=1, lwd=2)
abline(v=0, col="dimgray", lty=1, lwd=2)
abline(0, -1, lty=1, lwd=3, col="black") 
summary(lm(Natural$EXC_O2~Natural$EXC_CO2))
cor(Natural$EXC_CO2, Natural$EXC_O2, method="pearson")
summary(lm(Aquaculture$EXC_O2~Aquaculture$EXC_CO2))
cor(Aquaculture$EXC_CO2, Aquaculture$EXC_O2, method="pearson")
summary(lm(TGR$EXC_O2~TGR$EXC_CO2))
cor(TGR$EXC_CO2, TGR$EXC_O2, method="pearson")
abline(lm(Natural$EXC_O2~Natural$EXC_CO2), lty=2, lwd=3, col="deepskyblue2")
abline(lm(Aquaculture$EXC_O2~Aquaculture$EXC_CO2), lty=2, lwd=3, col="red")
abline(lm(TGR$EXC_O2~TGR$EXC_CO2), lty=2, lwd=3, col="navyblue") # The locator() function begins after here...
text(locator(1),"1:1 Line", col="black", cex=1.35, font=4) 
text(locator(1), "Slope = -1.2", col="deepskyblue2", cex=1.35, font=4) 
text(locator(1), "Slope = -1.0", col="red", cex=1.35, font=4) 
text(locator(1), "Slope = 0.4", col="navyblue", cex=1.35, font=4)
legend("topright",legend=c("Natural Pond", "Aquaculture Pond", "Submerged Floodplain"), pch=c(17, 15, 16), cex=c(1.75, 1.75, 1.75), pt.cex=c(1.75, 1.75, 1.75), 
       col=c("deepskyblue2", "red", "navyblue"), bty="n")


#######################################################################
## Figure 4. Diel variation in dissolved O2 and diffusive CH4 fluxes ##
#######################################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("Fig4.csv", header=TRUE)
colnames(TGR_All)
TGR_All$MONTH

par(mfrow=c(4,4))
min(TGR_All$FCH4_M) # -0.503
max(TGR_All$FCH4_M) # 32.65767
min(TGR_All$FCO2_M) # -210.559
max(TGR_All$FCO2_M) # 372.1373
min(TGR_All$FN2O_M) # -0.4273333
max(TGR_All$FN2O_M) # 0.2653333
min(TGR_All$O2_CONC) # 13.283
max(TGR_All$O2_CONC) # 39.377

# Tile 1 # Row 1
par(mar=c(1, 1, 1, 1)) # Bottom, left, top, right
par(oma=c(5, 8, 1, 1)) 
Jun <- subset(TGR_All, MONTH=="Jun")
Jun_NAT <- subset(Jun, SITE=="NAT")
Jun_ENG <- subset(Jun, SITE=="ENG")

plot(Jun_NAT$TIME, Jun_NAT$O2_CONC, pch=2, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
axis(2, cex.axis=2, cex=1.75, col="dimgray", col.axis="dimgray")
mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=2, cex=1.4, line=4, col="dimgray")
par(new=TRUE)
plot(Jun_ENG$TIME, Jun_ENG$O2_CONC, pch=0, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 2
par(mar=c(1, 1, 1, 1)) # Bottom, left, top, right
AugWet <- subset(TGR_All, MONTH=="AugDry")
AugWet_NAT <- subset(AugWet, SITE=="NAT")
AugWet_ENG <- subset(AugWet, SITE=="ENG")

plot(AugWet_NAT$TIME, AugWet_NAT$O2_CONC, pch=2, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
# axis(2, cex.axis=2, cex=1.75, col="dimgray", col.axis="dimgray")
# mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=2, cex=1.4, line=4, col="darkgray")
par(new=TRUE)
plot(AugWet_ENG$TIME, AugWet_ENG$O2_CONC, pch=0, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 3
par(mar=c(1, 1, 1, 1)) # Bottom, left, top, right
AugDry <- subset(TGR_All, MONTH=="AugWet")
AugDry_NAT <- subset(AugDry, SITE=="NAT")
AugDry_ENG <- subset(AugDry, SITE=="ENG")

plot(AugDry_NAT$TIME, AugDry_NAT$O2_CONC, pch=2, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
# axis(2, cex.axis=2, cex=1.75, col="dimgray", col.axis="dimgray")
# mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=2, cex=1.4, line=4, col="darkgray")
par(new=TRUE)
plot(AugDry_ENG$TIME, AugDry_ENG$O2_CONC, pch=0, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 4
par(mar=c(1, 1, 1, 1)) # Bottom, left, top, right
Jan <- subset(TGR_All, MONTH=="Jan")
Jan_NAT <- subset(Jan, SITE=="NAT")
Jan_ENG <- subset(Jan, SITE=="ENG")

plot(Jan_NAT$TIME, Jan_NAT$O2_CONC, pch=2, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
# axis(2, cex.axis=2, cex=1.75, col="dimgray", col.axis="dimgray")
# mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=2, cex=1.4, line=4, col="dimgray")
par(new=TRUE)
plot(Jan_ENG$TIME, Jan_ENG$O2_CONC, pch=0, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 5 # Row 2
par(mar=c(1, 1, 1, 1)) 

plot(Jun_NAT$TIME, Jun_NAT$FCH4_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-10, 50), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jun_NAT$TIME, Jun_NAT$FCH4_M-Jun_NAT$FCH4_SE, Jun_NAT$TIME, Jun_NAT$FCH4_M+Jun_NAT$FCH4_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
mtext(expression(CH[4] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jun_ENG$TIME, Jun_ENG$FCH4_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
arrows(Jun_ENG$TIME, Jun_ENG$FCH4_M-Jun_ENG$FCH4_SE, Jun_ENG$TIME, Jun_ENG$FCH4_M+Jun_ENG$FCH4_SE, length=0.05, angle=90, code=3, col="red")

# Tile 6
par(mar=c(1, 1, 1, 1)) 

plot(AugWet_NAT$TIME, AugWet_NAT$FCH4_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-10, 50), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugWet_NAT$TIME, AugWet_NAT$FCH4_M-AugWet_NAT$FCH4_SE, AugWet_NAT$TIME, AugWet_NAT$FCH4_M+AugWet_NAT$FCH4_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(CH[4] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(AugWet_ENG$TIME, AugWet_ENG$FCH4_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-10, 50), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugWet_ENG$TIME, AugWet_ENG$FCH4_M-AugWet_ENG$FCH4_SE, AugWet_ENG$TIME, AugWet_ENG$FCH4_M+AugWet_ENG$FCH4_SE, length=0.05, angle=90, code=3, col="red")

# Tile 7
par(mar=c(1, 1, 1, 1)) 

plot(AugDry_NAT$TIME, AugDry_NAT$FCH4_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-10, 50), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugDry_NAT$TIME, AugDry_NAT$FCH4_M-AugDry_NAT$FCH4_SE, AugDry_NAT$TIME, AugDry_NAT$FCH4_M+AugDry_NAT$FCH4_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(CH[4] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(AugDry_ENG$TIME, AugDry_ENG$FCH4_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-10, 50), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugDry_ENG$TIME, AugDry_ENG$FCH4_M-AugDry_ENG$FCH4_SE, AugDry_ENG$TIME, AugDry_ENG$FCH4_M+AugDry_ENG$FCH4_SE, length=0.05, angle=90, code=3, col="red")

# Tile 8
par(mar=c(1, 1, 1, 1)) 

plot(Jan_NAT$TIME, Jan_NAT$FCH4_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-10, 50), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jan_NAT$TIME, Jan_NAT$FCH4_M-Jan_NAT$FCH4_SE, Jan_NAT$TIME, Jan_NAT$FCH4_M+Jan_NAT$FCH4_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(CH[4] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jan_ENG$TIME, Jan_ENG$FCH4_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-10, 50), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jan_ENG$TIME, Jan_ENG$FCH4_M-Jan_ENG$FCH4_SE, Jan_ENG$TIME, Jan_ENG$FCH4_M+Jan_ENG$FCH4_SE, length=0.05, angle=90, code=3, col="red")

legend("topright",legend=c("Natural Pond", "Aquaculture Pond"), pch=c(17, 15), cex=c(2, 2), pt.cex=c(2, 2), 
       col=c("deepskyblue2", "red"), bty="n")

# Tile 9 # Row 3
par(mar=c(1, 1, 1, 1)) 
colnames(Jun_NAT)

plot(Jun_NAT$TIME, Jun_NAT$FCO2_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-300, 450), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jun_NAT$TIME, Jun_NAT$FCO2_M-Jun_NAT$FCO2_SE, Jun_NAT$TIME, Jun_NAT$FCO2_M+Jun_NAT$FCO2_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
mtext(expression(CO[2] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jun_ENG$TIME, Jun_ENG$FCO2_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-300, 450), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
arrows(Jun_ENG$TIME, Jun_ENG$FCO2_M-Jun_ENG$FCO2_SE, Jun_ENG$TIME, Jun_ENG$FCO2_M+Jun_ENG$FCO2_SE, length=0.05, angle=90, code=3, col="red")

# Tile 10
par(mar=c(1, 1, 1, 1)) 

plot(AugWet_NAT$TIME, AugWet_NAT$FCO2_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-350, 450), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugWet_NAT$TIME, AugWet_NAT$FCO2_M-AugWet_NAT$FCO2_SE, AugWet_NAT$TIME, AugWet_NAT$FCO2_M+AugWet_NAT$FCO2_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(CO[2] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(AugWet_ENG$TIME, AugWet_ENG$FCO2_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-300, 450), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugWet_ENG$TIME, AugWet_ENG$FCO2_M-AugWet_ENG$FCO2_SE, AugWet_ENG$TIME, AugWet_ENG$FCO2_M+AugWet_ENG$FCO2_SE, length=0.05, angle=90, code=3, col="red")

# Tile 11
par(mar=c(1, 1, 1, 1)) 

plot(AugDry_NAT$TIME, AugDry_NAT$FCO2_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-300, 450), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugDry_NAT$TIME, AugDry_NAT$FCO2_M-AugDry_NAT$FCO2_SE, AugDry_NAT$TIME, AugDry_NAT$FCO2_M+AugDry_NAT$FCO2_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(CO[2] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(AugDry_ENG$TIME, AugDry_ENG$FCO2_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-300, 450), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugDry_ENG$TIME, AugDry_ENG$FCO2_M-AugDry_ENG$FCO2_SE, AugDry_ENG$TIME, AugDry_ENG$FCO2_M+AugDry_ENG$FCO2_SE, length=0.05, angle=90, code=3, col="red")

# Tile 12
par(mar=c(1, 1, 1, 1)) 

plot(Jan_NAT$TIME, Jan_NAT$FCO2_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-300, 450), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jan_NAT$TIME, Jan_NAT$FCO2_M-Jan_NAT$FCO2_SE, Jan_NAT$TIME, Jan_NAT$FCO2_M+Jan_NAT$FCO2_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(CO[2] ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jan_ENG$TIME, Jan_ENG$FCO2_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-300, 450), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jan_ENG$TIME, Jan_ENG$FCO2_M-Jan_ENG$FCO2_SE, Jan_ENG$TIME, Jan_ENG$FCO2_M+Jan_ENG$FCO2_SE, length=0.05, angle=90, code=3, col="red")

# Tile 13 # Row 4
par(mar=c(1, 1, 1, 1)) 

plot(Jun_NAT$TIME, Jun_NAT$FN2O_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-1, 1), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jun_NAT$TIME, Jun_NAT$FN2O_M-Jun_NAT$FN2O_SE, Jun_NAT$TIME, Jun_NAT$FN2O_M+Jun_NAT$FN2O_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
mtext(expression(N[2]*O ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jun_ENG$TIME, Jun_ENG$FN2O_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-1, 1), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
arrows(Jun_ENG$TIME, Jun_ENG$FN2O_M-Jun_ENG$FN2O_SE, Jun_ENG$TIME, Jun_ENG$FN2O_M+Jun_ENG$FN2O_SE, length=0.05, angle=90, code=3, col="red")
axis(side=1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
     labels=c("6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "12:00", "2:00", "4:00"), 
     cex.axis=2, cex=1.75, las=2)

# Tile 14
par(mar=c(1, 1, 1, 1)) 

plot(AugWet_NAT$TIME, AugWet_NAT$FN2O_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-1, 1), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugWet_NAT$TIME, AugWet_NAT$FN2O_M-AugWet_NAT$FN2O_SE, AugWet_NAT$TIME, AugWet_NAT$FN2O_M+AugWet_NAT$FN2O_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(N[2]*O ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(AugWet_ENG$TIME, AugWet_ENG$FN2O_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-1, 1), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugWet_ENG$TIME, AugWet_ENG$FN2O_M-AugWet_ENG$FN2O_SE, AugWet_ENG$TIME, AugWet_ENG$FN2O_M+AugWet_ENG$FN2O_SE, length=0.05, angle=90, code=3, col="red")
axis(side=1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
     labels=c("6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "12:00", "2:00", "4:00"), 
     cex.axis=2, cex=1.75, las=2)

# Tile 15
par(mar=c(1, 1, 1, 1)) 

plot(AugDry_NAT$TIME, AugDry_NAT$FN2O_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-1, 1), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugDry_NAT$TIME, AugDry_NAT$FN2O_M-AugDry_NAT$FN2O_SE, AugDry_NAT$TIME, AugDry_NAT$FN2O_M+AugDry_NAT$FN2O_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(N[2]*O ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(AugDry_ENG$TIME, AugDry_ENG$FN2O_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-1, 1), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugDry_ENG$TIME, AugDry_ENG$FN2O_M-AugDry_ENG$FN2O_SE, AugDry_ENG$TIME, AugDry_ENG$FN2O_M+AugDry_ENG$FN2O_SE, length=0.05, angle=90, code=3, col="red")
axis(side=1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
     labels=c("6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "12:00", "2:00", "4:00"), 
     cex.axis=2, cex=1.75, las=2)

# Tile 16
par(mar=c(1, 1, 1, 1)) 

plot(Jan_NAT$TIME, Jan_NAT$FN2O_M, pch=17, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-1, 1), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jan_NAT$TIME, Jan_NAT$FN2O_M-Jan_NAT$FN2O_SE, Jan_NAT$TIME, Jan_NAT$FN2O_M+Jan_NAT$FN2O_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(N[2]*O ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jan_ENG$TIME, Jan_ENG$FN2O_M, pch=15, col="red", bg="red", xaxt="n", ylim=c(-1, 1), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(Jan_ENG$TIME, Jan_ENG$FN2O_M-Jan_ENG$FN2O_SE, Jan_ENG$TIME, Jan_ENG$FN2O_M+Jan_ENG$FN2O_SE, length=0.05, angle=90, code=3, col="red")
axis(side=1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
     labels=c("6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "12:00", "2:00", "4:00"), 
     cex.axis=2, cex=1.75, las=2)

#############################################################################
## Figure S2. Diel variation in dissolved O2, chlorophyll, water T, and pH ##
#############################################################################

rm(list=ls())

# Load the data
TGR_All <- read.csv("TGR_All.csv", header=TRUE)
colnames(TGR_All)
TGR_All$MONTH

par(mfrow=c(4,4))
min(TGR_All$FCH4_M) # -0.503
max(TGR_All$FCH4_M) # 32.65767
min(TGR_All$FCO2_M) # -210.559
max(TGR_All$FCO2_M) # 372.1373
min(TGR_All$FN2O_M) # -0.4273333
max(TGR_All$FN2O_M) # 0.2653333
min(TGR_All$O2_CONC) # 0.388
max(TGR_All$O2_CONC) # 11.63
min(TGR_All$WATER_T) # 13.283
max(TGR_All$WATER_T) # 39.377
min(TGR_All$PH) # 7.084
max(TGR_All$PH) # 8.76
min(TGR_All$CHL) # -1.547
max(TGR_All$CHL) # 72.47

# Tile 1 # Row 1
par(mar=c(1, 1, 1, 1)) # Bottom, left, top, right
par(oma=c(5, 8, 1, 1)) 
Jun <- subset(TGR_All, MONTH=="Jun")
Jun_NAT <- subset(Jun, SITE=="NAT")
Jun_ENG <- subset(Jun, SITE=="ENG")

plot(Jun_NAT$TIME, Jun_NAT$O2_CONC, pch=2, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
axis(2, cex.axis=2, cex=1.75, col="dimgray", col.axis="dimgray")
mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=2, cex=1.4, line=4, col="dimgray")
par(new=TRUE)
plot(Jun_ENG$TIME, Jun_ENG$O2_CONC, pch=0, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 2
par(mar=c(1, 1, 1, 1)) # Bottom, left, top, right
AugWet <- subset(TGR_All, MONTH=="AugDry")
AugWet_NAT <- subset(AugWet, SITE=="NAT")
AugWet_ENG <- subset(AugWet, SITE=="ENG")

plot(AugWet_NAT$TIME, AugWet_NAT$O2_CONC, pch=2, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
# axis(2, cex.axis=2, cex=1.75, col="dimgray", col.axis="dimgray")
# mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=2, cex=1.4, line=4, col="darkgray")
par(new=TRUE)
plot(AugWet_ENG$TIME, AugWet_ENG$O2_CONC, pch=0, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 3
par(mar=c(1, 1, 1, 1)) # Bottom, left, top, right
AugDry <- subset(TGR_All, MONTH=="AugWet")
AugDry_NAT <- subset(AugDry, SITE=="NAT")
AugDry_ENG <- subset(AugDry, SITE=="ENG")

plot(AugDry_NAT$TIME, AugDry_NAT$O2_CONC, pch=2, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
# axis(2, cex.axis=2, cex=1.75, col="dimgray", col.axis="dimgray")
# mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=2, cex=1.4, line=4, col="darkgray")
par(new=TRUE)
plot(AugDry_ENG$TIME, AugDry_ENG$O2_CONC, pch=0, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 4
par(mar=c(1, 1, 1, 1)) # Bottom, left, top, right
Jan <- subset(TGR_All, MONTH=="Jan")
Jan_NAT <- subset(Jan, SITE=="NAT")
Jan_ENG <- subset(Jan, SITE=="ENG")

plot(Jan_NAT$TIME, Jan_NAT$O2_CONC, pch=2, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
# axis(2, cex.axis=2, cex=1.75, col="dimgray", col.axis="dimgray")
# mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=2, cex=1.4, line=4, col="dimgray")
par(new=TRUE)
plot(Jan_ENG$TIME, Jan_ENG$O2_CONC, pch=0, col="dimgray", bg="dimgray", xaxt="n", ylim=c(0, 12), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 5 # Row 2
par(mar=c(1, 1, 1, 1)) 

plot(Jun_NAT$TIME, Jun_NAT$WATER_T, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(10, 40), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
mtext(expression(Water ~ T ~ (C)), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jun_ENG$TIME, Jun_ENG$WATER_T, pch=0, col="red", bg="red", xaxt="n", ylim=c(10, 40), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 6
par(mar=c(1, 1, 1, 1)) 

plot(AugWet_NAT$TIME, AugWet_NAT$WATER_T, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(10, 40), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
par(new=TRUE)
plot(AugWet_ENG$TIME, AugWet_ENG$WATER_T, pch=0, col="red", bg="red", xaxt="n", ylim=c(10, 40), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")

# Tile 7
par(mar=c(1, 1, 1, 1)) 

plot(AugDry_NAT$TIME, AugDry_NAT$WATER_T, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(10, 40), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
par(new=TRUE)
plot(AugDry_ENG$TIME, AugDry_ENG$WATER_T, pch=0, col="red", bg="red", xaxt="n", ylim=c(10, 40), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")

# Tile 8
par(mar=c(1, 1, 1, 1)) 

plot(Jan_NAT$TIME, Jan_NAT$WATER_T, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(10, 40), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
par(new=TRUE)
plot(Jan_ENG$TIME, Jan_ENG$WATER_T, pch=0, col="red", bg="red", xaxt="n", ylim=c(10, 40), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")

legend("topright",legend=c("Natural Pond", "Aquaculture Pond"), pch=c(2, 0), cex=c(2, 2), pt.cex=c(2, 2), 
       col=c("deepskyblue2", "red"), bty="n")

# Tile 9 # Row 3
par(mar=c(1, 1, 1, 1)) 
colnames(Jun_NAT)

plot(Jun_NAT$TIME, Jun_NAT$PH, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(6, 9), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
mtext(expression(pH), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jun_ENG$TIME, Jun_ENG$PH, pch=0, col="red", bg="red", xaxt="n", ylim=c(6, 9), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")

# Tile 10
par(mar=c(1, 1, 1, 1)) 

plot(AugWet_NAT$TIME, AugWet_NAT$PH, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(6, 9), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
par(new=TRUE)
plot(AugWet_ENG$TIME, AugWet_ENG$PH, pch=0, col="red", bg="red", xaxt="n", ylim=c(6, 9), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")

# Tile 11
par(mar=c(1, 1, 1, 1)) 

plot(AugDry_NAT$TIME, AugDry_NAT$PH, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(6, 9), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
par(new=TRUE)
plot(AugDry_ENG$TIME, AugDry_ENG$PH, pch=0, col="red", bg="red", xaxt="n", ylim=c(6, 9), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")

# Tile 12
par(mar=c(1, 1, 1, 1)) 

plot(Jan_NAT$TIME, Jan_NAT$PH, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(6, 9), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
par(new=TRUE)
plot(Jan_ENG$TIME, Jan_ENG$PH, pch=0, col="red", bg="red", xaxt="n", ylim=c(6, 9), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")

# Tile 13 # Row 4
par(mar=c(1, 1, 1, 1)) 

plot(Jun_NAT$TIME, Jun_NAT$CHL, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-2, 80), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
mtext(expression("Chlorophyll" ~ "a" ~ (ug ~ L^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(Jun_ENG$TIME, Jun_ENG$CHL, pch=0, col="red", bg="red", xaxt="n", ylim=c(-2, 80), xlab="", ylab="", frame=FALSE, axes=F, las=1, cex=2, type="b")
axis(side=1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
     labels=c("6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "12:00", "2:00", "4:00"), 
     cex.axis=2, cex=1.75, las=2)

# Tile 14
par(mar=c(1, 1, 1, 1)) 

plot(AugWet_NAT$TIME, AugWet_NAT$CHL, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-2, 80), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
par(new=TRUE)
plot(AugWet_ENG$TIME, AugWet_ENG$CHL, pch=0, col="red", bg="red", xaxt="n", ylim=c(-2, 80), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
axis(side=1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
     labels=c("6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "12:00", "2:00", "4:00"), 
     cex.axis=2, cex=1.75, las=2)

# Tile 15
par(mar=c(1, 1, 1, 1)) 

plot(AugDry_NAT$TIME, AugDry_NAT$CHL, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-2, 80), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
arrows(AugDry_NAT$TIME, AugDry_NAT$FN2O_M-AugDry_NAT$FN2O_SE, AugDry_NAT$TIME, AugDry_NAT$FN2O_M+AugDry_NAT$FN2O_SE, length=0.05, angle=90, code=3, col="deepskyblue2")
# axis(2, cex.axis=2, cex=1.75, col="black", col.axis="black")
# mtext(expression(N[2]*O ~ "Flux" ~ (mg ~ m^{-2} ~ h^{-1})), side=2, cex=1.4, line=4, col="black")
par(new=TRUE)
plot(AugDry_ENG$TIME, AugDry_ENG$CHL, pch=0, col="red", bg="red", xaxt="n", ylim=c(-2, 80), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
axis(side=1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
     labels=c("6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "12:00", "2:00", "4:00"), 
     cex.axis=2, cex=1.75, las=2)

# Tile 16
par(mar=c(1, 1, 1, 1)) 

plot(Jan_NAT$TIME, Jan_NAT$CHL, pch=2, col="deepskyblue2", bg="deepskyblue2", xaxt="n", ylim=c(-2, 80), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
par(new=TRUE)
plot(Jan_ENG$TIME, Jan_ENG$CHL, pch=0, col="red", bg="red", xaxt="n", ylim=c(-2, 80), xlab="", ylab="", frame=T, axes=F, las=1, cex=2, type="b")
axis(side=1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
     labels=c("6:00", "8:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00", "12:00", "2:00", "4:00"), 
     cex.axis=2, cex=1.75, las=2)


#########################################################
## Figure ia. LME model regression for floodplain FCH4 ##
#########################################################

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
legend("topright",legend=c("Conductivity", "Precipitation", "pH", "Hours Since Sunset", "AOU", "Water Temperature", "Floodplain Ponds", "Submerged Floodplain"), lty=c(1,5,3,2,4,6,1,1), lwd=c(2,2,2,2,2,2,2,2), col=c("darkgray", "darkgray", "darkgray", "darkgray", "darkgray", "darkgray", "deepskyblue2", "navyblue"), cex=c(1.25,1.25,1.25,1.25,1.25,1.25,1.25,1.25), bty="n")
mtext(expression(x[i]), side=1, font=3, font.axis=2, las=1, cex.lab=1.35, cex=1.25)


#######################################################################
## Figure ib. LME model regression for floodplain and reservoir FCO2 ##
#######################################################################

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


#####################################################################################################################################################################################################
## Figure ii. Relative contributions of diffusive CH4 emissions by the aquatic and terrestrial environments on the Three Gorges Floodplain, the Yangtze River, and its tributaries during drawdown ##
#####################################################################################################################################################################################################

rm(list=ls())

# Load the data
PAIR <- read.csv("LIT_Pairwise.csv", header=TRUE)
colnames(PAIR)

# Compare values
wilcox.test(PAIR$POND, PAIR$WETLAND) #p-value = 0.69  
boxplot(PAIR$POND, PAIR$WETLAND)
t.test(PAIR$POND, PAIR$WETLAND, var.equal = T, conf.level = 0.95) # p-value = 0.3424, df = 22
wilcox.test(LIT_Summer$CO2EQ, LIT_Winter$CO2EQ) 
boxplot(LIT_Summer$CO2EQ, LIT_Winter$CO2EQ)
t.test(LIT_Summer$CO2EQ, LIT_Winter$CO2EQ, var.equal = T, conf.level = 0.95) # p-value = 0.01864, df = 39

# Load the data
LIT <- read.csv("LIT.csv", header=TRUE)
colnames(LIT)
LIT$ENV
par(mfrow=c(1,2))

# Tile 1
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


