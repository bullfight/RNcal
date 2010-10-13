cnr2.cal <-function(mV, cal){

cal <- 1000 / cal
watts <- mV * cal

return(watts)
}