# Calculate Windspeed Correction Factor

cf.u <- function(Windspeed, RN){
	CF <- rep(NA, length(Windspeed))

	# UP #######
	ind <- which(RN > 0)
	CF[ind] <- 1 + 
		{
		(0.066 * 0.2 * Windspeed[ind]) / 
		(0.066 +  (0.2 * Windspeed[ind]))
		}

	# DOWN ####
	ind <- which(RN < 0)
	CF[ind] <- (0.00174 * Windspeed[ind]) + 0.99755
	rm(ind)
	
	return(CF)
}

# Plot Correction Calibration Factor in response to U'
plot.response <- function(){
u	<- seq(0, 7, .01)
cf.up	<- 1 + { (0.066 * 0.2 * u) / (0.066 +  (0.2 * u)) }
cf.dw	<- (0.00174 * u) + 0.99755


p <- xyplot(
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

return(p)
}