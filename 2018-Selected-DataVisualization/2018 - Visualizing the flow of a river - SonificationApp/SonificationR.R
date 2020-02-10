#####################
## Import packages ##
#####################

#library(sonify)
#sonify(y = NHANES$Weight, waveform="sawtooth", interpolation="spline", duration=8,play=TRUE,pulse_amp = 0.3,stereo=TRUE,player=)
library(dataRetrieval)
#For more information on dataRetrival, uncomment below:
#vignette("dataRetrieval",package = "dataRetrieval")
library(tuneR)
tuneR::setWavPlayer("/usr/bin/afplay")

#####################
## Set Site Info ##
#####################


siteNumber <- "01491000"
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#####################
## Extract data    ##
#####################

# Raw daily data:
rawDailyData <- readNWISdv(siteNumber, parameterCd, "1980-01-01", 
                           "2010-01-01")


# data("NHANES")
# NHANES$Weight2 <- abs(NHANES$Weight)
# NHANES$BMI2 <- abs(NHANES$BMI)
# NHANES$Diet.Iron2 <- abs(NHANES$Diet.Iron)
# 
# NHANES$Weight3 <- round(NHANES$Weight2,digits = 2)
# NHANES$BMI3 <- round(NHANES$BMI2,digits = 2)
# NHANES$Diet.Iron3 <- round(NHANES$Diet.Iron2, digits = 2)


sound.length <- 0.5
sampling.rate <- 6000
bits <- 32

long.pause <- 0.5 # in seconds

w <- silence(duration = long.pause, xunit = c("samples", "time")[2], bit=bits, samp.rate=sampling.rate)

for (i in 1:length(rawDailyData$X_00060_00003)){
  wobj <- sine(freq = rawDailyData$X_00060_00003[i], xunit = c("samples", "time")[2], duration = sound.length, bit = bits, samp.rate = sampling.rate)
  w <- bind(wobj,w)
}

w <- normalize(w,unit = "32")
#play(w)

writeWave(w,"weight.wav")

