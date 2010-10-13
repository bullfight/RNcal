# Pat Schmitz
# 2010 Oct
# This Function fits Q6 or Q7.1 RN to mean CNR2 RN
# Outputs
	# Up Fit
	# Down Fit
	# Fit Plot
	# Regression Fit Plot

fit.RN <- function(dat, sensor){

	#Selected Sensor Vector
	vect <- dat[ ,sensor]
	
	# UP Fit ############################
	ind	<- which(vect > 0 & !is.na(vect))

	f.up	<- lm(
		as.formula(paste("RN ~", sensor)),
		dat[ind,]
	)

	dat[ind, sensor] <- dat[ind, sensor] * f.up$coefficients[[2]]

	# DOWN Fit ##########################
	ind <- which(vect < 0 & !is.na(vect))

	f.dw <- lm(
		as.formula(paste("RN ~", sensor)), 
		dat[ind,]
	)

	dat[ind, sensor] <- dat[ind, sensor] * f.dw$coefficients[[2]]

	# Plot Fits
	fp1 <- xyplot(
		as.formula(paste(sensor, "+ RN ~ TIMESTAMP")), 
		data = dat, 
		auto.key = list(T, points = F, lines = T),
		type = "l", 
		aspect = 1
	)

	fp2 <- xyplot(
		as.formula(paste("RN1 ~ ", sensor)), 
		data = dat, 
		auto.key = list(T, points = F, lines = T),
		groups = format(TIMESTAMP, "%j"), 
		type = "p", 
		aspect = 1
	)
		
	
	fits <- list(
		vals = data.frame(
			sensor = sensor,
			up = f.up$coefficients[[2]],
			down = f.dw$coefficients[[2]]
		),
		p.fit = fp1,
		p.reg = fp2
	)
	
	return(fits)

}