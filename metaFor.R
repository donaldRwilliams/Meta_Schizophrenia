# -------- Analysis by Donny --------
require(metafor)
## read data
rawData <- read.csv("meansAndSd_symptoms.csv")


## compute effects size and variance
metaData <- escalc(measure="SMD", m1i=OxyMean, sd1i=OxySd, n1i=nOxy, m2i=PlaMean, sd2i=PlaSd, n2i=nPla, data=rawData)