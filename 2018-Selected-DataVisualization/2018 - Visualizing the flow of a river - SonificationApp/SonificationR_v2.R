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
library(ggplot2)

#####################
## Set Site Info ##
#####################

#Site 1
siteNumber <- "01491000" #Grensboro, Maryland https://waterdata.usgs.gov/usa/nwis/uv?01491000
ChoptankInfo <- readNWISsite(siteNumber)
parameterCd <- "00060"

#Site 2
siteNumber2 <- "01103500" #The Charles River at Dover MA
CharlesInfo <- readNWISsite(siteNumber)
parameterCd2 <- "00060"


#####################
## Extract data    ##
#####################

# Raw daily data, site 1, 18 years of data:
rawDailyData <- readNWISdv(siteNumber, parameterCd, "2000-01-01", 
                           "2018-01-01")

# Raw daily data, site 2, 18 years of data:
rawDailyData2 <- readNWISdv(siteNumber2, parameterCd2, "2000-01-01", 
                           "2018-01-01")


#####################
## Plot data       ##
#####################

currentPoint = as.numeric(1) #Set currentPoint to start at 1

#print(rawDailyData$X_00060_00003[currentPoint])

g <- ggplot(data=rawDailyData, aes(x=rawDailyData$Date, y=rawDailyData$X_00060_00003)) +
  geom_line()+
  geom_point()+
  geom_point(data=rawDailyData, aes(x=rawDailyData$Date[currentPoint], y=rawDailyData$X_00060_00003[currentPoint]), colour="red", size=5)


#plot(g)

g2 <- ggplot(data=rawDailyData2, aes(x=rawDailyData2$Date, y=rawDailyData2$X_00060_00003)) +
  geom_line()+
  geom_point()+
  geom_point(data=rawDailyData2, aes(x=rawDailyData2$Date[currentPoint], y=rawDailyData2$X_00060_00003[currentPoint]), colour="red", size=5)


#plot(g2)

#####################
## Make the sound  ##
#####################

#This takes approximately 30 minutes to run

sound.length <- 0.1
sampling.rate <- 6000
bits <- 32

long.pause <- 0.5 # in seconds

w <- silence(duration = long.pause, xunit = c("samples", "time")[2], bit = bits, samp.rate = sampling.rate)

#Create a loop for the length of the data frame, adding the frequency representing the flow of each river 

#Loop over all possible iterations 
# for (i in 1:length(rawDailyData$X_00060_00003)) {
#   wobj <- sine(freq = rawDailyData$X_00060_00003[i], xunit = c("samples", "time")[2], duration = sound.length, bit = bits, samp.rate = sampling.rate) +
#     sine(freq = rawDailyData2$X_00060_00003[i], xunit = c("samples", "time")[2], duration = sound.length, bit = bits, samp.rate = sampling.rate)
#   w <- bind(wobj,w)
#   currentPoint = (as.numeric(currentPoint) + as.numeric(i))
#   #print(currentPoint)
#   #print(length(rawDailyData$X_00060_00003))
# }

# Loop over only given observations 
totalrow <- nrow(rawDailyData)

for (row in 1:nrow(rawDailyData)) {
  wobj <- sine(freq = rawDailyData$X_00060_00003[row], xunit = c("samples", "time")[2], duration = sound.length, bit = bits, samp.rate = sampling.rate) +
    sine(freq = rawDailyData2$X_00060_00003[row], xunit = c("samples", "time")[2], duration = sound.length, bit = bits, samp.rate = sampling.rate)
    w <- bind(wobj,w)
}

w <- normalize(w,unit = "32")

#####################
## Play the sound  ##
#####################

#If you just want to play the music, uncomment below
#play(w)

#####################
## Record the sound #
#####################

writeWave(w,"duelingdischarge_small.wav")

length = 658.1 # Length of recording in seconds

datapersec = totalrow/length

print(datapersec)
