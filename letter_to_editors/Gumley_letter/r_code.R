** Analysis by Donald Williams **
  install.packages("metafor")
require(metafor)

## data from table 2 of Gumley et al (2014)
totY <- c(-0.11, 0.20, 1.86, -.10)
totSe <- c(.36, .38, .36, .45)
posY <- c(-.1 , 1.17, -.14)
posSe <- c(.36, .35, .44)
negY  <- c(-.12, 1.37, 0.07)
negSe  <- c(.36, .36, .45)
genY <- c(-0.06, .78, -.09)
genSe <- c(.36, .34, .45)
## data frames
dfSymptoms <- data.frame(posY, posSe, negY, negSe, genY, genSe)
dfTotal <- data.frame(totY, totSe)
## fixed and random effects models
## totRE is not similar to what is in paper
totRE <- rma(totY, sei = totSe, data = dfTotal)

## totFE effect size estimate is what is reported for random effects model
## SE and intervals are different, however
totFE <- rma(totY, sei = totSe, data = dfTotal, method = "FE")

## posRE is not what is reported in paper
posRE <- rma(posY, sei = posSe, data = dfSymptoms)

##posFE is similar to estimates in paper
posFE <- rma(posY, sei = posSe, data = dfSymptoms, method = "FE")

##negRE is not the same as in paper
negRE <- rma(negY, sei = negSe, data = dfSymptoms)

##negFE is the same as reported in paper
negFE <- rma(negY, sei = negSe, data = dfSymptoms, method = "FE")

## genRE is not the same as in the paper
genRE <- rma(genY, sei = genSe, data = dfSymptoms)

## genFE is the same as reported
genFE <- rma(genY, sei = genSe, data = dfSymptoms, method = "FE")

## conclusion: 3/4 fixed effect models are correct, whereas 0/4 random effects 
## models are correct. 