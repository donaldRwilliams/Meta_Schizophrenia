cdata <- read.csv2("data/cognitive.csv")
# remember to eclude social_func and syptoms from the complete excel sheet
cdata <- droplevels(subset(cdata, SG1 %in% c("general_cog", "social_cog")))
cdata$obs <- 1:nrow(cdata)
cdata$study <- paste0(cdata$author, " et al. (", cdata$year, ")")
# remove empty factor levels
lvls2 <- levels(cdata$SG2)
cdata$SG2 <- factor(cdata$SG2, lvls2[nchar(lvls2) > 0L])
lvls3 <- levels(cdata$SG3)
cdata$SG3 <- factor(cdata$SG3, lvls3[nchar(lvls3) > 0L])

library(metafor)
# Hedges'g estimates
# for now treat crossover designs as having independent groups
cdata <- escalc(measure="SMD", m1i=oxyMean_pre, sd1i=oxySd_pre, 
                n1i=oxyN, m2i=plaMean_pre, sd2i=plaSd_pre, 
                n2i=plaN, data=cdata, var.names = c("SMD_pre", "vSMD_pre"))

cdata <- escalc(measure="SMD", m1i=oxyMean_post, sd1i=oxySd_post, 
                n1i=oxyN, m2i=plaMean_post, sd2i=plaSd_post, 
                n2i=plaN, data=cdata, var.names = c("SMD_post", "vSMD_post"))

# assumed pre-post correlation
cdata$ri <- 0.5

# SMCR estimates
# standardize based on pre-treatment SD
cdata <- escalc(measure="SMCR", m1i=oxyMean_post, m2i=oxyMean_pre,
                sd1i=oxySd_pre, ni=oxyN, ri = ri,
                data=cdata, var.names = c("oxySMCR", "voxySMCR"))

cdata <- escalc(measure="SMCR", m1i=plaMean_post, m2i=plaMean_pre,
                sd1i=plaSd_pre, ni=plaN, ri = ri,
                data=cdata, var.names = c("plaSMCR", "vplaSMCR"))

# for now treat crossover designs as having independent groups
cdata$SMCR <- cdata$oxySMCR - cdata$plaSMCR
cdata$vSMCR <- cdata$voxySMCR + cdata$vplaSMCR


# correct the direction of effects (positive = improvement)
cdata$SMD_pre <- cdata$SMD_pre * cdata$direction
cdata$SMD_post <- cdata$SMD_post * cdata$direction
cdata$SMCR <- cdata$SMCR * cdata$direction

# define factor contrast
sum_coding <- function(x, lvls = levels(x)) {
  x <- factor(x, levels = lvls)
  contrasts(x) <- contr.sum(length(lvls))
  colnames(contrasts(x)) <- lvls[-length(lvls)]
  x
}
cdata$SG1 <- sum_coding(cdata$SG1)
cdata$SG2 <- sum_coding(cdata$SG2)
cdata$SG3 <- sum_coding(cdata$SG3)

scdata <- droplevels(subset(cdata, SG1 == "social_cog"))
gcdata <- droplevels(subset(cdata, SG1 == "general_cog"))
