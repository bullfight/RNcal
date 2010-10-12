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
	tz 	= "UTC - 5"
)

# Indexing Vectors
sensors <- names(dat)[8:31]
index <- length(sensors)

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

dat$Short_1 <- 	dat$Short_1 * s1c
dat$Long_1  <- 	dat$Long_1  * l1c
dat$RN1 		<- dat$Short_1 + dat$Long_1

dat$Short_2 <- 	dat$Short_2 * s2c
dat$Long_2  <- 	dat$Long_2  * l2c
dat$RN2 		<- dat$Short_2 + dat$Long_2

p1 <- xyplot(
	RN1 ~ RN2, 
	data = dat, 
	groups = format(TIMESTAMP, "%j"), 
	xlim = range(dat$RN2),
	ylim = range(dat$RN1),
	type = "p", 
	aspect = 1
)

# Generate Calibrations ###############################################
sensor 	<- "sn91234"
vect 		<- dat[,sensor]

# UP ################################
ind <- which(vect > 0 & !is.na(vect))

f.up <- lm(
	as.formula(paste("RN1 ~", sensor)),
	dat[ind,]
)

dat[ind, sensor] <- dat[ind, sensor] * f.up$coefficients[2]

# DOWN ##############################
ind <- which(vect < 0 & !is.na(vect))

f.dw <- lm(
	as.formula(paste("RN1 ~", sensor)), 
	dat[ind,]
)

dat[ind, sensor] <- dat[ind, sensor] * f.dw$coefficients[2]

# Plot Fits
fp1 <- xyplot(
	as.formula(paste(sensor, "+ RN1~ TIMESTAMP")), 
	data = dat, 
	type = "l", 
	aspect = 1
)

fp2 <- xyplot(
	as.formula(paste("RN1 ~ ", sensor)), 
	data = dat, 
	groups = format(TIMESTAMP, "%j"), 
	type = "p", 
	aspect = 1
)

# Produce Rolling Averages ############################################
#library(zoo)

#vals <- as.matrix(dat[3:37], rownames.force = T)
#vals <- as.matrix(dat[3:dim(dat)[2]], rownames.force = T)
#dat.ts <- zoo(vals, dat$TIMESTAMP)

#m.dat <- rollmean(dat.ts,30, na.pad = T, align = "right")
#######################################################################