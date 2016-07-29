cdata <- read.csv2("data/cognitive.csv", na.strings = c("NA", ""))
cdata$obs <- 1:nrow(cdata)
cdata$sample <- paste0(cdata$author, " (", cdata$year, ")")
# Goldman has two separate samples
cdata$study <- ifelse(!grepl("Goldman", cdata$sample), cdata$sample,
                      "Goldman et al. (2011)")
cdata$country_simple <- factor(ifelse(cdata$country != "USA", "other", "USA"))
cdata$level <- factor(cdata$level, levels = c("low_level", "high_level"), 
                      labels = c("Low", "High"))

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

# sort alphabetical after cognition type and sample
cdata <- cdata[order(cdata$SG1, cdata$sample), ]

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
cdata$emotion <- sum_coding(cdata$emotion)

scdata <- droplevels(subset(cdata, SG1 == "social_cog"))
ncdata <- droplevels(subset(cdata, SG1 == "neurocognition"))
ercdata <- droplevels(subset(cdata, SG2 == "emotionRec"))
ercdata$fear <- factor(ifelse(ercdata$emotion != "fear", "other", "fear"))
