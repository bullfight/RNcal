Code to Calibrate Net Radiometers
=========================

Files
-----
* 2010_calRN.CR1 - datalogger source file for recording mV from 26 radiometers, a sonic anemometer
* 2010_calRN.tdf - binary for datalogger measures
* load.data.R - script to infile recorded data by datalogger, generate calibrations, and produce calibration cards.
* calcard.Snw - this is a Sweave file, mixed LaTeX and R code, which generates the calibration card using the typesetting engine LaTeX.  To generate these documents you must have a latex typesetting engine installed.  Details are included in the script load.data.R
* OUTPUT/calibrationcards3/2010_Q01299_calibration.pdf - example calibration card

Calibration Setup
-----------------
**24 Net Radiometers (to calibrate)**

* Q6
* Q7

**2 Reference Radiometers**

* CNR2 

**Sonic Anemometer**

* Young 85000 2-axis

Set Up
------
* South facing array
* 1.5 meters above surface
* leveled with gauge
* new thermopile shields

![Source](http://github.com/bullfight/RNcal/raw/master/images/rn1.JPG "Q7 and CNR2 Net Radiometer")

![Source](http://github.com/bullfight/RNcal/raw/master/images/rn.JPG "Calibration Setup") ![Source](http://github.com/bullfight/RNcal/raw/master/images/rn2.JPG "net radiometers in plane") ![Source](http://github.com/bullfight/RNcal/raw/master/images/rn3.JPG "inclusion of 2-axis sonic anemometer")