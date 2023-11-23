# Bob Punta - SWW paper - stack plot only

#Load packages
library(Bchron)
library(cowplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(parameters)
library(mclust)
library(NbClust)
library(cluster)

#clear previous console
remove (list = ls())

#set working directory on old mac
#setwd("/Users/Steve/Dropbox/BAS/Data/R/Papers/2023_Bob_Punta_SWW/")

#set working directory on new mac
setwd("/Users/sjro/Dropbox/BAS/Data/R/Papers/2023_Bob_Punta_SWW/")

# Clear plots
if(!is.null(dev.list())) dev.off()

# New data
x1 <- read.csv("Data/Bob_Noth_pc_Punta_mositure_USE.csv") # New pollen data 

# Published data
x2 <- read.csv("Data/Villacis_2023_PCA_diatoms_Cipreses.csv") # Villacis_2023_PCA_diatoms
x3 <- read.csv("Data/Lamy_2010_GC2_Hygrophytes_Fig2h_USE.csv") # Lamy_2010_GC2_Hygrophytes_Fig_2h
x4 <- read.csv("Data/Lamy_2010_Palm2_AR_biogenic_carbonate_Fig_2c.csv") # Lamy_2010_Palm2_AR_biogenic_carbonate_Fig_2c
x5 <- read.csv("Data/Lamy_2010_TML1_Misodendron_Fig_2g.csv") # Lamy_2010_Palm2_AR_biogenic_carbonate_Fig_2c

x23 <- read.csv("Data/x23_JRI_ice_core.csv")
x25 <- read.csv("Data/x25_Kaufmann2020.csv")

x30 <- read.csv("Data/x30_Laskar2004_InsAnomaly_53S.csv") # insolation anomaly data
bp <- c('Age_ka')  # create vector of column names to recalculate as ka BP
x30[bp] <- x30[bp] - ((2-1950)/1000)  # convert to BP by subtracting 1950 from samples collection date and assign back

x31 <- read.csv("Data/x31_Baggenstos2019_Irradiance.csv")
x32 <- read.csv("Data/x32_Saunders2018_SPECIM.csv")
x33 <- read.csv("Data/x33_Saunders2018_DCond.csv") # updated age to Sh20
x34 <- read.csv("Data/x34_Moreno2018_SAM.csv")  # updated age to Sh20

# Figure X ----------------------------------------------------------------

# Set up a 0-18 ka plot 12 cm wide to match KGI dimensions
# Clear plots
if(!is.null(dev.list())) dev.off()
plotinch1 <- 8 / cm(1) # -> 8 cm  is  3.149606 inches
plotinch1 <- 3.149608 *(8/8)
b <- ((8.27-plotinch1)/2)-1
checksize <- b*2+plotinch1+2

# FINAL PLOT TO PDF
pdf("output.pdf", pointsize=12,  width = 8.27, height = 11.69) 
layout(matrix(1:7, ncol=1)) # Set up layout
par(mai=c(0.25,0.25,0.25,0.25), omi=c(0,0,0,0), pin=c(plotinch1, plotinch1/1.99710788366826), mgp=c(2,0.5,0), xaxs='i') # Set up internal margins

# PLOT TO SCREEN FIRST
# Clear plots
if(!is.null(dev.list())) dev.off()
layout(matrix(1:7, ncol=1)) # Set up layout
par(mai=c(0.25,0.25,0.25,0.25), pin=c(plotinch1, plotinch1/1.99710788366826),  mgp=c(2,0.5,0), xaxs='i', omi=c(0,0,0,0), ps=16)


# -------------------------------------------------------------------------
# 1) Insolation and Irradiance ------------------------------------------------------------------------

p30 <- plot(Spring_Summer_SONDJF~Age_ka, data = x30, pch = 21, type = "l", col="red", bg="red", 
            cex= 1, xlim = c(-0.1, 18), ylim = c(-20, 20),
            frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE)
p30a <- plot(Winter_JJA~Age_ka, data = x30, pch = 21, type = "l", col="blue", bg="blue", 
             cex= 1, xlim = c(-0.1, 18), ylim = c(-20, 20),
             frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE) 
p30b <- plot(Annual~Age_ka, data = x30, pch = 21, type = "l", lty = 3, col="black", bg="black", 
             cex= 1, xlim = c(-0.1, 18), ylim = c(-20, 20),
             frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE) 
p_axis <- c(#axis(1, seq(0,18,2), tck=-0.04), labels=rep("",33),
  axis(4, seq(-20,20,5), tck=-0.02, las = 1, xpd = 1, line = 1)
)
mtext("53S Insolation 
      Anomaly (W m-2)", cex = 0.75, adj = 0.5, col = "black", xpd = TRUE, side=4, line=6)
par(new = TRUE)
p31 <- plot(LOESS_100_dTSI~LOESS_100yr, data = x31, pch = 21, type = "l", col="#800000", bg="#800000", 
            cex= 1, xlim = c(-0.1, 18), ylim = c(-0.5, 0.5),
            frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE)
# add mean dTSI and error bar
p31 <- plot(mean~plot_age, data = x31, pch = 21, col="#800000", bg="white", 
            cex= 2, xlim = c(-0.1, 18), ylim = c(-0.5, 0.5),
            frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE)
#p_axis <- c(axis(1, labels = FALSE))
arrows(x0=x31$plot_age,
       y0=x31$mean-x31$error,
       x1=x31$plot_age,
       y1=x31$mean+x31$error,
       angle=180,
       code=3,
       length=0,
       col = "#800000",
       lwd=1)
par(new=TRUE)
p31 <- plot(mean~plot_age, data = x31, pch = 21, col="#800000", bg="white", 
            cex= 2, xlim = c(-0.1, 18), ylim = c(-0.5, 0.5),
            frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE)
p_axis <- c(#axis(1, seq(0,18,2), tck=-0.02), labels=rep("",33),
  axis(2, seq(-0.5,0.5,0.5), tck=-0.02, las=1, col = "#800000", col.axis = "#800000", line = 1))
mtext("dTSI  W m-2", cex = 0.75, col = "#800000", side=2, line=5)


# 2) New Mire Surface Wetness (MSW) normalised & Nothofagus dombeyi type - pollen data - moisture proxy-------------------------------------------------------------------------

p2 <- plot(Normal~Age_ka_BP, data = x1, pch = 21, type = "p", col="darkgreen", bg="darkgreen", 
            cex= 0.5, xlim = c(-0.1, 16), ylim = c(0,100),
            frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
par(new = TRUE) 
p2_zones <- plot(zone~Age_zone, data = x1, pch = 3, type = "p", col="darkgreen", bg="darkgreen", 
           cex= 0.5, xlim = c(-0.1, 16), ylim = c(0,100),
           frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
par(new = TRUE) 
lines(Normal~Age_ka_BP, data = x1, 
      lty=1, col="darkgreen", bg="darkgreen", lwd=1)
p_axis <- c(#axis(1, seq(0,16,2), tck=-0.02), axis(1, seq(0,16,0.5),labels=rep("",33), tck=-0.05),
            #axis(2, seq(0, 100, 10), tck=-0.02, las = 1, xpd = 1, line = 1),
            #axis(3, seq(0,16,2), labels=rep("",7), tck=0.02),axis(3, seq(-0.1,16,0.1),labels=rep("",33), tck=0.05),
            axis(4, seq(0, 100, 20), tck=-0.02, las = 1, xpd = 1, line = 1)
)
mtext("Moisture (ND norm %)", side = 4, cex = 0.75, line = 6)
#mtext("Age (cal. ka BP)", side = 1, cex = 0.75, line = 3)
#par(new = FALSE) #remove hold


# 3) Villacis_2023_PCA_diatoms_Cipreses -------------------------------------------------------------------------

p3 <- plot(PC1~Age_ka_BP, data = x2, pch = 21, type = "p", col="darkblue", bg="darkblue", 
           cex= 0.5, xlim = c(-0.1, 16), ylim = c(-5,5),
           frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
lines(PC1~Age_ka_BP, data = x2, 
      lty=1, col="darkblue", bg="darkblue", lwd=1)
p_axis <- c(#axis(1, seq(0,16,2), tck=-0.02), axis(1, seq(0,16,0.5),labels=rep("",33), tck=-0.05),
            axis(2, seq(-4, 6, 2), tck=-0.02, las = 1, xpd = 1, line = 1))
            #axis(3, seq(0,16,2), labels=rep("",7), tck=0.02),axis(3, seq(-0.1,16,0.1),labels=rep("",33), tck=0.05),
            #axis(4, seq(-4, 6, 1), tck=-0.02, las = 1, xpd = 1, line = 1)
mtext("PC1 [Lake level]", side = 2, cex = 0.75, line = 6)
#mtext("Age (cal. ka BP)", side = 1, cex = 0.75, line = 3)
#par(new = FALSE) #remove hold


# 4) Lamy_2010_GC2_Hygrophytes_Fig_2h & Lamy_2010_Palm2_AR_biogenic_carbonate_Fig_2c ---------------------------

p4b <- plot(Acc_rate_CaCO3~Age_ka_BP, data = x4, pch = 21, type = "p", col="black", bg="black", 
            cex= 0.5, xlim = c(-0.1, 16), ylim = c(0,16),
            frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
par(new = TRUE) 
lines(Acc_rate_CaCO3~Age_ka_BP, data = x4, 
      lty=1, col="black", bg="black", lwd=1)
p_axis <- c(#axis(1, seq(0,16,2), tck=-0.02), axis(1, seq(0,16,0.5),labels=rep("",33), tck=-0.02),
  #axis(2, seq(0, 0.1, 10), tck=-0.02, las = 1, xpd = 1, line = 1),
  #axis(3, seq(0,16,2), labels=rep("",7), tck=0.02),axis(3, seq(-0.1,16,0.1),labels=rep("",33), tck=0.05),
  axis(2, seq(0, 16, 4), tck=-0.02, las = 1, xpd = 1, line = 1)
)
mtext("Palm2 Biogenic AR 
      (g/cm2/ka)", side = 2, cex = 0.75, line = 5, col = 'black')
par(new = TRUE)

p4c <- plot(Misodendrum~Age_ka_BP, data = x5, pch = 21, type = "p", col="darkorange", bg="darkorange", 
            cex= 0.5, xlim = c(-0.1, 16), ylim = c(16,0),
            frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
par(new = TRUE) 
lines(Misodendrum~Age_ka_BP, data = x5, 
      lty=1, col="darkorange", bg="darkorange", lwd=1)
p_axis <- c(#axis(1, seq(0,16,2), tck=-0.02), axis(1, seq(0,16,0.5),labels=rep("",33), tck=-0.02),
  #axis(2, seq(0, 0.1, 10), tck=-0.02, las = 1, xpd = 1, line = 1),
  #axis(3, seq(0,16,2), labels=rep("",7), tck=0.02),axis(3, seq(-0.1,16,0.1),labels=rep("",33), tck=0.05),
  axis(4, seq(0, 16, 4), tck=-0.02, las = 1, xpd = 1, line = 1)
)
mtext("GC2 Misodendrum (%)", side = 4, cex = 0.75, line = 6, col = "darkorange")

p4a <- plot(Hygrophytes_pc~Age_ka_BP, data = x3, pch = 21, type = "p", col="lightcoral", bg="lightcoral", 
            cex= 0.5, xlim = c(-0.1, 16), ylim = c(100,0),
            frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
par(new = TRUE) 
lines(Hygrophytes_pc~Age_ka_BP, data = x3, 
      lty=1, col="lightcoral", bg="lightcoral", lwd=1)
p_axis <- c(#axis(1, seq(0,16,2), tck=-0.02), axis(1, seq(0,16,0.5),labels=rep("",33), tck=-0.02),
  #axis(2, seq(0, 0.1, 10), tck=-0.02, las = 1, xpd = 1, line = 1),
  #axis(3, seq(0,16,2), labels=rep("",7), tck=0.02),axis(3, seq(-0.1,16,0.1),labels=rep("",33), tck=0.05),
  axis(4, seq(0, 100, 20), tck=-0.02, las = 1, xpd = 1, line = 1)
)
mtext("Hygrophytic taxa (%)", side = 4, cex = 0.75, line = 6, col="lightcoral")


# 5) Saunders et al 2018 - Macquarie SHW ------------------------------------------------------------------------

p32 <- plot(R850_R900~Age_BP_Sh20_mean, data = x32, pch = 21, type = "l", col="#99CC99", bg="#99CC99", 
            cex= 1, xlim = c(-100, 16000), ylim = c(0.8,1.1),
            frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE)
# set up the LOESS
output_int_yrs = 1000 #enter the final output interval, e.g., 100 year LOESS output = 1000/12100
age_range = 12000-(-100) # enter the age range 
alpha <- output_int_yrs/age_range #calculate the span value
lw32 <- loess(R850_R900~Age_BP_Sh20_mean, 
              data = x32,
              span = alpha)
x32_count <- seq(from=-100, to=12000, by=1) # set up count fro 0-12 ka
x32_count_rev <- order(x32_count, decreasing=TRUE)
x32_pred <- predict(lw32, x32_count, se=TRUE) # Fit the LOESS
lines(x32_pred$fit, lty="solid", col="#008000", lwd=0.75) # add it to graph
lines(x32_pred$fit-1.96*x32_pred$se.fit, 
      lty="dashed", col="grey", lwd=0.75) #Fit 95% error lines - SE is very small here as there's so much data!
lines(x32_pred$fit+1.96*x32_pred$se.fit, 
      lty="dashed", col="grey", lwd=0.751) #Fit 95% error lines - SE is very small here as there's so much data!
lines(x32_pred$fit, lty="solid", col="#008000", lwd=0.75) # add it to graph
#Create polygon error shading - doesn't work for negative values on x-axis
#x32_y_polygon <- c((x32_pred$fit+1.96*x32_pred$se.fit)[x32_count], 
#                   (x32_pred$fit-1.96*x32_pred$se.fit)[x32_count_rev]) 
#x32_x_polygon <- c(x32_count, x32_count_rev)
#polygon(x32_x_polygon, x32_y_polygon, col="#00009933", border=NA)
par(new = TRUE)
p32b <- plot(R850_R900_mean~age_plot, data = x32, pch = 21, type = "l", lty = 2, lwd = 1, col="#666666", bg="#666666", 
             cex= 1, xlim = c(-100, 12000), ylim = c(0.8,1.1),
             frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE)
p32c <- plot(R850_R900_mean+R850_R900_sd~age_plot, data = x32, pch = 21, type = "l", lty = 3, lwd = 1, col="#B2B2B2", bg="#B2B2B2", 
             cex= 1, xlim = c(-100, 12000), ylim = c(0.8,1.1),
             frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE)
p32d <- plot(R850_R900_mean-R850_R900_sd~age_plot, data = x32, pch = 21, type = "l", lty = 3, lwd = 1, col="#B2B2B2", bg="#B2B2B2", 
             cex= 1, xlim = c(-100, 16000), ylim = c(0.8,1.1),
             frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
p_axis <- c(#axis(1, seq(0,16000,2000), tck=-0.04), labels=rep("",33),
            axis(2, seq(0.8,1.1,0.1), tck=-0.02, col = "black", col.axis = "black", las=1, line=1))
            #axis(3, seq(0,16000,2000), tck=-0.04), labels=rep("",33),
            #axis(4, seq(0.8,1.1,0.1), tck=-0.02, col = "#008000", col.axis = "#008000", las=1, line=1))
mtext("R850/R900 index
       Minerogenic input", cex = 0.75, side=2, line=5, col = "#008000", adj = 0.8, xpd = TRUE)

# 6) JRI ice core record - Mulvaney et al 2012 -------------------------------------------------------------------------
p23 <- plot(T_anomaly~Age_BP, data = x23, pch = 21, type = "l", col="#800000", bg="#800000", 
            cex= 1, xlim = c(-0.1, 16), ylim = c(-1.5, 2.5),
            frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
lines(x23$T_upper_err~Age_BP, data = x23, 
      lty=1, col="grey", lwd=0.75) # add upper 95% CI error bounds
lines(x23$T_lower_err~Age_BP, data = x23,
      lty=1, col="grey", lwd=0.75) # add lower 95% CI error bounds
lines(x23$wmean~age1, data = x23, 
      lty=3, col="black", lwd=0.75) # add 12 ka mean
#lines(x23$upper~age1, data = x23, 
#      lty=3, col="grey", lwd=0.75)
#lines(x23$lower~age1, data = x23, 
#      lty=3, col="grey", lwd=0.75)
par(new = TRUE) 
p23 <- plot(T_anomaly~Age_BP, data = x23, pch = 21, type = "l", col="#800000", bg="#800000", 
            cex= 1, xlim = c(-0.1, 16), ylim = c(-1.5, 2.5),
            frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
p_axis <- c(axis(1, seq(0,16,2), tck=-0.04), axis(1, seq(0, 16,0.5),labels=rep("",33), tck=-0.02),
  #axis(2, seq(-2, 2, 1), tck=-0.02,  las = 1, xpd = 1, line = 1, col.axis="#800000"),
  #axis(3, seq(0,16,2), labels=rep("",7), tck=0.04),axis(3, seq(-0.1,16,0.5),labels=rep("",122), tck=0.02),
  axis(4, seq(-2, 2, 1), tck=-0.02, las = 1, xpd = 1, line = 1)
)
mtext("Temp. Anomaly (deg. C)", side = 2, cex = 0.75, line = 5, col="#800000")
mtext("Age (cal. ka BP)", side = 1, cex = 0.75, line = 4)
#par(new = FALSE) #remove hold



dev.off()
# -------------------------------------------------------------------------
# Not included 

# F) SAM Moreno et al 2018 --------------------------------------------------------
p34 <- plot(SAM_index_reconstruction~Age_Sh13, data = x34, pch = 21, type = "l", col="black", bg="black", 
            cex= 1, xlim = c(-100, 16000), ylim = c(-2, 6),
            frame=FALSE, ylab = "", xlab="", yaxt="n", xaxt="n")
par(new = TRUE) #hold the plot frame to add median values
p_axis <- c(#axis(1, seq(0,12000,2000), tck=-0.04), labels=rep("",33),
  axis(4, seq(-2,6,2), tck=-0.02, las=1, line=1))
mtext("Reconstructed
      SAM Index", cex = 0.75, side=4, line=5, col = "black", srt=180, adj = 1, xpd = TRUE)

# H) Kaufmann et al 2020 - 90-60S and global temp -------------------------------------------------------------------------
p25 <- plot(S90_S60_median~Age_Sh20, data = x25, pch = 21, type = "l", col="#800000", bg="#800000", 
            cex= 1, xlim = c(-0.1, 18), ylim = c(-1.5, 0.5),
            frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
par(new = TRUE) 
p25 <- plot(S90_S60_median~Age_Sh20, data = x25, pch = 21, col="#800000", bg="#800000", 
            cex= 0.2, xlim = c(-0.1, 18), ylim = c(-1.5, 0.5),
            frame=FALSE, ylab="", xlab="", xaxt="n", yaxt="n", ann=FALSE)
lines(x25$global_median~Age_Sh20, data = x25, 
      lty=1, col="black", lwd=0.75) # add average median temp for 90-60S
lines(x25$global_median~age1, data = x25, 
      lty=3, col="grey", lwd=0.75) # add global data as a black line
p_axis <- c(axis(1, seq(0, 18, 2), tck=-0.02), axis(1, seq(0,18,0.5),labels=rep("",33), tck=-0.02),
            #axis(2, seq(-1.5, 2.5, 0.5), tck=-0.04,  las = 1, xpd = 1),
            #axis(3, seq(0,12000,2000), labels=rep("",7), tck=0.04),axis(3, seq(-100,12000,100),labels=rep("",122), tck=0.02),
            axis(4, seq(-1.5, 0.5, 0.5), tck=-0.02, las = 1, xpd = 1, line = 1)
)
mtext("Temp. Anomaly (deg. C)", side = 4, cex = 0.75, line = 6)
mtext("Age (cal. a BP)", side = 1, cex = 0.75, line = 3)
#par(new = FALSE) #remove hold




# -------------------------------------------------------------------------



