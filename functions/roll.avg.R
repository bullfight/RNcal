# Produce Rolling Averages ############################################
#library(zoo)

#vals <- as.matrix(dat[3:37], rownames.force = T)
#vals <- as.matrix(dat[3:dim(dat)[2]], rownames.force = T)
#dat.ts <- zoo(vals, dat$TIMESTAMP)

#m.dat <- rollmean(dat.ts,30, na.pad = T, align = "right")
#######################################################################

#x = head(time(m.dat))