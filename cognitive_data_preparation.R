cdata <- read.csv2("data/cognitive.csv")
cdata$obs <- 1:nrow(cdata)
cdata$study <- paste0(cdata$author, " (", cdata$year, ")")
cdata$country_simple <- factor(ifelse(cdata$country != "USA", "other", "USA"))

# remove empty factor levels
lvls2 <- levels(cdata$SG2)
cdata$SG2 <- factor(cdata$SG2, lvls2[nchar(lvls2) > 0L])
lvls3 <- levels(cdata$SG3)
cdata$SG3 <- factor(cdata$SG3, lvls3[nchar(lvls3) > 0L])

library(metafor)
# Hedges'g estimates
cdata$Sd_post_pooled <- with(cdata, 
  sqrt(((oxyN - 1) * oxySd_post^2 + (plaN - 1) * plaSd_post^2) / (oxyN + plaN - 2)))

# use SMD variance for independent trials
cidata <- 
  escalc(measure="SMD", m1i=oxyMean_post, sd1i=oxySd_post, 
         n1i=oxyN, m2i=plaMean_post, sd2i=plaSd_post, 
         n2i=plaN, var.names = c("SMD", "vSMD"),
         data=cdata[cdata$design == "independent", ])

# use SMCR variance for crossover trials
# Since the pooled SD is used, the effect size will be 
# nearly identical to the respective SMD effect size
ccdata <- 
  escalc(measure="SMCR", m1i=oxyMean_post, m2i=plaMean_post,
         sd1i=Sd_post_pooled, ni=oxyN, ri = cor_oxy_pla,
         var.names = c("SMD", "vSMD"),
         data=cdata[cdata$design == "crossover", ])

cdata <- rbind(cidata, ccdata)

# sort alphabetical after cognition type and study
cdata <- cdata[order(cdata$SG1, cdata$study), ]

# assumed pre-post correlation
cdata$cor_pre_post <- 0.5

# SMCR estimates
# standardize based on pre-treatment SD
cdata <- escalc(measure="SMCR", m1i=oxyMean_post, m2i=oxyMean_pre,
                sd1i=oxySd_pre, ni=oxyN, ri = cor_pre_post,
                data=cdata, var.names = c("oxySMCR", "voxySMCR"))

cdata <- escalc(measure="SMCR", m1i=plaMean_post, m2i=plaMean_pre,
                sd1i=plaSd_pre, ni=plaN, ri = cor_pre_post,
                data=cdata, var.names = c("plaSMCR", "vplaSMCR"))

# for now treat crossover designs as having independent groups
cdata$SMCR <- cdata$oxySMCR - cdata$plaSMCR
cdata$vSMCR <- cdata$voxySMCR + cdata$vplaSMCR

# correct the direction of effects (positive = improvement)
cdata$SMD <- cdata$SMD * cdata$direction
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
ncdata <- droplevels(subset(cdata, SG1 == "neurocognition"))
ercdata <- droplevels(subset(cdata, SG2 == "emotionRec"))
ercdata$fear <- factor(ifelse(ercdata$SG3 != "fear", "other", "fear"))
