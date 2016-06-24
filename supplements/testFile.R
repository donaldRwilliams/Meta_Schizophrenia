# helper function to compute p-values
p_value <- function(x, alternative = c("two.sided", "greater", "less")) {
  if (is(x, "brmshypothesis")) {
    hyps <- x$hypothesis
  } else if (is(x, "brmsfit")) {
    alternative <- match.arg(alternative)
    fixef <- fixef(x)
    sign <- sign(fixef[, 1])
    fixef <- rownames(fixef)
    if (alternative == "greater") {
      hyps <- paste(fixef, "> 0")
    } else if (alternative == "less") {
      hyps <- paste(fixef, "< 0")
    } else {
      hyps <- paste(fixef, ifelse(sign < 0, "<", ">"), "0") 
    }
    hyps <- hypothesis(x, hyps)$hypothesis
  } 
  odds <- hyps$Evid.Ratio
  mult <- ifelse(is(x, "brmsfit") && alternative == "two.sided", 2, 1)
  out <- (1 - odds / (1 + odds)) * mult
  names(out) <- rownames(hyps)
  out
}

# read in data and load packages
metaData <- read.csv("old_data/metaData.csv")
library(brms)

# se expects the standard deviation not the variance 
# yi and vi are computed via metafor right?
# switch to multivariate modeling as soon as I have time
b1 <- brm(yi | se(sqrt(vi)) ~ 0 + factor(sympType) + (1|id), 
          data = metaData)
## your CI's span zero. However, if they did not span zero the probability of 
## being less than zero would be atleast 97.5 % 
b1

# -------- Analysis by Paul --------
# compute p-values more effectively
# corresponds to 1 - p_negLessZero
p_value(b1, alternative = "less")

# compare positive and negative symptoms
(hyp <- hypothesis(b1, "factorsympTypepositive - factorsympTypenegative < 0"))
plot(hyp, chars = NULL)
p_value(hyp)

# -------- Analysis by Donny --------
##samples
samples1 <- posterior_samples(b1)
## lets find the probability that negative symptoms are less than zero
p_negLessZero <- sum(samples1$b_factorsympTypenegative < 0) / 4000
p_negLessZero

## What I then have in mind is comparing positive and negative symptoms 
posVSneg <- abs(samples1$b_factorsympTypepositive) - abs(samples1$b_factorsympTypenegative)

## I think this would be the probability that oxytocin is more effective for negative than 
## positive symptoms
p_negLessZero <- sum(posVSneg < 0)/4000

## The posterior probability that oxytocin is more effect for negative than positive symptoms
p_negLessZero

## This analysis is probably wrong, but think it shows what I want to do. To date,
## medications have not been effective at reducing negative symptoms such as social 
## withdrawl. Oxytocin facilitates social behavior in many species, so it is a candidate 
## treatment. I think oxytocin being more effective in treating negative symptoms than 
## positive symptoms with a probability of 90 % is very interesting. I think we will
## have to explore journals in an effort to find one that is friendly towards posterior
## probabilities.


## We also need to account for correlations between scores within the same study. I could
## email the authors of the studies. I did, however, find a paper that describes the correlations
## between negative, positive, and general symptoms. For the multivariate analaysis, here
## are the correlations~

## general vs. negative: r = 0.60
## general vs. positive: r = 0.68

## There are two correlations for negative vs positive. I am thinking a sensitivity
## analysis could make use of both and possibly a third (the average of the two).

## negative vs positive: r = -0.62
## negative vs positive: r = -0.55