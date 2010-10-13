# Pat Schmitz
# 2010-10-4

# This code is intended to load the data output from calRN.CR1, and
# calibrate REB RN against CNR2

# A CR1000 code built to record
# 24 REB Q6 and Q7.1 Net Radiometers
# 2 CNR2 Pyranometer + Pyrgeometer
# 1 Young 85000 2-axis Sonic Anemometer

# Output Files from calRN.CR1
# RnCAL_sample.dat 	(2 hz)
# RnCAL_AVG.dat 	(10 minute)

# Setup and leveling completed on DOY 284, 2010

# cd ~/Dropbox/2010.FieldMeasures/DL_code/
setwd("~/Dropbox/2010.FieldMeasures/DL_code/")
rm(list = ls())

# Load Data ###########################################################

# RnCAL_sample.dat
dat <- read.csv("data/RnCAL_sample.dat", 
	skip = 1,
	header = T,
	as.is = T,
	na.strings = c(	"NAN", "mV","Smp", 
			"m/s", "C", "Volts", 
			"RN", "TS"
	)
)
dat <- dat[3: dim(dat)[1], ]
dat$TIMESTAMP <- as.POSIXct(
	x 	= dat$TIMESTAMP, 
	origin 	= "1960-01-01", 
	tz 	= "UTC-5"
)
unique(format(dat$TIMESTAMP, "%j"))

# Indexing Vectors
sensors.not.installed <- c("sn91227", "Q98194", "sn91170", "sn91229", 
		   "sn91231", "sn91236", "Q03050"
)

dat <-	dat[format(dat$TIMESTAMP, "%j") > 284, 
	!(names(dat) %in% sensors.not.installed) ]

installed.sensors <- names(dat)[8:24]

rm(sensors.not.installed)
# Apply Calibrations to CNR2 Pyranometer and Pyrgeometer ##############
# Sensor	Serial	Name	Calibration (μV/Wm2)
# Pyranometer 	100237 	Short_1	16.03
# Pyrgeometer	100237	Long_1	12.47
# Pyranometer 	100239 	Short_1	15.89
# Pyrgeometer	100239	Long_1	12.24

s1c <- 1000 / 16.03
l1c <- 1000 / 12.47
s2c <- 1000 / 15.89
l2c <- 1000 / 12.24

dat$Short_1	<- dat$Short_1 * s1c
dat$Long_1	<- dat$Long_1  * l1c
dat$RN1 <- dat$Short_1 + dat$Long_1

dat$Short_2	<- dat$Short_2 * s2c
dat$Long_2	<- dat$Long_2  * l2c
dat$RN2 <- dat$Short_2 + dat$Long_2

dat$RN <- (dat$RN1 + dat$RN2) / 2

rm(s1c, l1c, s2c, l2c)
# Windspeed Correction ################################################
# According to the manual for the Q7.1 from Campbell Scientific, 
# a correction factor of the following form must be added to the 
# calibrated measure of RN to account for convective cooling as air 
# moves past the sensors

# Plot Correction Calibration Factor in response to U'
u	<- seq(0, 7, .01)
cf.up	<- 1 + { (0.066 * 0.2 * u) / (0.066 +  (0.2 * u)) }
cf.dw	<- (0.00174 * u) + 0.99755

pdf("PLOTS/uCF.pdf")
	xyplot(
	x 	= cf.up + cf.dw ~ u, 
	type 	= "l", 
	lty 	= c(1,2),
	col 	= "black",
	ylim 	= c(.98, 1.08),
	xlim 	= c(-.05, 7),
	ylab 	= "Correction Factor [W/m-2]",
	xlab 	= "Windspeed [m/s]",
	aspect 	= .5,
	main 	= "Correction factor for Windspeed",
	key 	= list(
			lines	= T, 
			text	= list(
				c(
				"Positive Fluxes", 
				"Negative Fluxes"
				)
			),
			lty	= c(1,2)
	)
)
dev.off()


# Calculate Windspeed Correction Factor
source("functions/cf.u.R")
CF <- cf.u(dat$Windspeed, dat$RN)

# Remove Effect of Windspeed from Reference RN
# which will generate convective cooling on target RNs
dat$RN <- dat$RN * (1/CF)

rm(CF, u, cf.up, cf.dw)
# Generate Calibrations ###############################################
source("functions/fit.RN.R")

fits <- data.frame()

for(sensor in installed.sensors){
	out <- fit.RN(dat, sensor)
	
	fits <- rbind(fits, out$vals)
}

write.csv( 
	x = fits, 
	file = "OUTPUT/fits.csv"
)	


