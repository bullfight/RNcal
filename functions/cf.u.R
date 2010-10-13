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