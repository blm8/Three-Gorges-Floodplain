#####################################
## Temperature v. Henry's Constant ##
#####################################

rm(list = ls()) # Clear your workspace

TvKH <- read.csv("TvKH.csv", stringsAsFactors = F) # Load your data
TvKH

# Set your standard margins, leaving room for second axis
par(mar=c(5, 4, 4, 6) + 0.25)

# Plot CH4 data 
min(na.omit(TvKH$CH4)) # 1.21 mmol/L/atm
max(na.omit(TvKH$CH4)) # 2.6 mmol/L/atm

plot(TvKH$degC, TvKH$CH4, pch=16, axes=FALSE, ylim=c(1, 3), xlab=expression("Water Temperature" ~ (degree*C)), ylab="", 
     cex.lab=1.35, type="b", font=2, las=1, cex=1.25, col="black")

# Draw first y axis
mtext(CH[4] ~ "&" ~ O[2] ~ "Solubility" ~ (mmol ~ L^-1 ~ atm^-1), side=2, line=2.5, cex=1.25)
axis(2, ylim=c(1, 3), col="black", las=1, cex=1.25, cex.lab=1.35, font=2)  #las=1 makes horizontal labels

# Plot CO2 data on the same chart
par(new=TRUE)

min(na.omit(TvKH$CO2)) # 26.53 mmol/L/atm
max(na.omit(TvKH$CO2)) # 77.94 mmol/L/atm
min(na.omit(TvKH$N2O)) # 18.65 mmol/L/atm
max(na.omit(TvKH$N2O)) # 57.97 mmol/L/atm

plot(TvKH$degC, TvKH$CO2, pch=15,  xlab="", ylab="", ylim=c(10, 80), 
     axes=FALSE, type="b", cex=1.25, cex.lab=1.35, font=2, col="gray44")

# Draw second y axis
mtext(expression(CO[2] ~ "&" ~ "N"[2]*"O" ~ "Solubility" ~ (mmol ~ L^-1 ~ atm^-1)), side=4, col="gray44", line=3.15, cex=1.25, cex.lab=1.35, font=2)
axis(4, ylim=c(10, 80), col="gray44", col.axis="gray48", cex=1.25, cex.lab=1.35, font=2, las=1)

# Plot N2O data on the same chart
par(new=TRUE)

plot(TvKH$degC, TvKH$N2O, pch=2,  xlab="", ylab="", ylim=c(10, 80), 
     axes=FALSE, type="b", cex=1.15, col="gray44")

# Plot O2 data on the same chart
par(new=TRUE)

plot(TvKH$degC, TvKH$O2, pch=5,  xlab="", ylab="", ylim=c(1, 3), 
     axes=FALSE, type="b", cex=1.15, col="black")

# Draw x axis
axis(1, xlim=c(0, 35), col="black", col.axis="black", las=1, cex=1.25, cex.lab=1.35, font=4)

# Add Legend
legend("top", ncol=3, legend=c(expression(CH[4]), expression(O[2]), expression(CO[2]), expression("N"[2]*"O")),
       text.col=c("black", "black", "gray44", "gray44"), bty="n", pch=c(16, 5, 15, 2), cex=c(1.25,1.25,1.25,1.25), pt.cex=c(1.35,1.35,1.35,1.35), col=c("black", "black", "gray44", "gray44"))

