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
	
	# Positive Flux Fit ############################
	ind	<- which(vect > 0 & !is.na(vect))

	f.pos	<- lm(
		as.formula(paste("RN ~ 0 + ", sensor)),
		dat[ind,]
	)
		
	dat[ind, sensor] <- dat[ind, sensor] * f.pos$coefficients[[1]]

	# Negative Flux Fit ##########################
	ind <- which(vect < 0 & !is.na(vect))

	f.neg <- lm(
		as.formula(paste("RN ~ 0 + ", sensor)), 
		dat[ind,]
	)

	dat[ind, sensor] <- dat[ind, sensor] * f.neg$coefficients[[1]]

	# Plot Fits
	fp1 <- xyplot(
		as.formula(paste(sensor, "+ RN ~ TIMESTAMP")), 
		data = dat, 
		auto.key = list(T, points = F, lines = T),
		type = "l", 
		main = "Fit Response",
		xlab = "Time",
		ylab = "[W/m-2]"
	)

	fp2 <- xyplot(
		as.formula(paste("RN ~ ", sensor)), 
		data = dat, 
		auto.key = list(T, points = F, lines = T),
		groups = format(TIMESTAMP, "%j"), 
		type = "p", 
		aspect = 1,
		main = "Fit by Day of Year",
		xlim = c(-100, 600),
		ylim = c(-100, 600),
		panel = function(...){ 
			panel.grid(h=-1,v=-1) 
			panel.xyplot(...) 
			panel.abline(a = 0, b = 1) 
		}
	)
		
	
	fits <- list(
		vals = data.frame(
			Serial = sensor,
			Pos = f.pos$coefficients[[1]],
			Neg = f.neg$coefficients[[1]]
		),
		f.pos = f.pos,
		f.neg = f.neg,
		p.fit = fp1,
		p.reg = fp2
	)
	
	return(fits)

}