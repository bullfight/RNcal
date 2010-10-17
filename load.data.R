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
#unique(format(dat$TIMESTAMP, "%j"))

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

source("functions/cnr2.cal.R")

dat$Short_1 <- cnr2.cal(dat$Short_1, 16.03)
dat$Long_1 <- cnr2.cal(dat$Long_1, 12.47)
dat$RN1 <- dat$Short_1 + dat$Long_1

dat$Short_2 <- cnr2.cal(dat$Short_2, 15.89)
dat$Long_2 <- cnr2.cal(dat$Long_2, 12.24)
dat$RN2 <- dat$Short_2 + dat$Long_2

dat$RN <- (dat$RN1 + dat$RN2) / 2

rn.ctrl <- xyplot(
	x = Short_1 + Short_2 + Long_1 + Long_2 ~ TIMESTAMP,
	data = dat,
	auto.key = list(T, points = F, lines = T),
	type = "l", 
	main = "Reference Measurements",
	xlab = "Time",
	ylab = "[W/m-2]"
)

shrt <- xyplot(
	x = Short_1  ~ Short_2, 
	data = dat, 
	auto.key = list(T, points = F, lines = T),
	type = "p", 
	aspect = 1,
	main = "Short Wave",
	xlab = "CNR2 : 100239",
	ylab = "CHR2 : 100237"
)

long <- xyplot(
	x = Long_1  ~ Long_2, 
	data = dat, 
	auto.key = list(T, points = F, lines = T),
	type = "p", 
	aspect = 1,
	main = "Long Wave",
	xlab = "CNR2 : 100239",
	ylab = "CHR2 : 100237"
)

p.rn <- xyplot(
	x = RN + RN1 + RN2 ~ TIMESTAMP, 
	data = dat, 
	auto.key = list(T, points = F, lines = T),
	type = "l", 
	main = "Reference Measurements",
	xlab = "Time",
	ylab = "[W/m-2]"
)



# Windspeed Correction ################################################
# According to the manual for the Q7.1 from Campbell Scientific, 
# a correction factor of the following form must be added to the 
# calibrated measure of RN to account for convective cooling as air 
# moves past the sensors

# Calculate Windspeed Correction Factor
source("functions/cf.u.R")
CF <- cf.u(dat$Windspeed, dat$RN)

# Remove Effect of Windspeed from Reference RN
# which will generate convective cooling on target RNs
dat$RN <- dat$RN * (1/CF)

rm(CF)
# Generate Calibrations ###############################################
source("functions/fit.RN.R")

calibrations <- data.frame()

for(sensor in installed.sensors){
	out <- fit.RN(dat, sensor)
	
	calibrations <- rbind(calibrations, out$vals)
	
	# Generate Calibration Card
	Sweave("calcard.Snw")
	library(tools)
	texi2dvi("calcard.tex",  pdf = TRUE)

	name <- paste("2010_", sensor, "_calibration.pdf", sep="")
	file.rename("calcard.pdf", name)
	file.copy(
		from = name, 
		to = paste("OUTPUT/calibrationcards2/", sep=""), 
		overwrite = T
	)	
	file.remove(
		c(
		list.files(pattern=".pdf"), 
		list.files(pattern=".eps"), 
		list.files(pattern=".log"), 
		list.files(pattern=".aux"), 
		list.files(pattern=".tex")
		)
	)
	
}

write.csv( 
	x = calibrations, 
	file = "OUTPUT/2010.calibrations2.csv",
	row.names = F
)
