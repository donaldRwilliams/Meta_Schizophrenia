*** code by Donny ***

## function to compute within subjects effect size and variance   
var <- function(n, mean, sd , r) {
  d <- mean/sd  
  v <- (2 * (1-r))/n + d^2/(2* n)
  return(c(d, v))
  
}

## data for mean difference to effect size 
d <- read.csv("https://raw.githubusercontent.com/donaldRwilliams/Meta_Schizophrenia/master/modabbernia.csv", header = TRUE)

## positive symptoms
## r = 0.25
var(d$n[1], d$meanDif[1], d$sd[1], 0.25)
## r = 0.50
var(d$n[1], d$meanDif[1], d$sd[1], 0.50)
## r = 0.75
var(d$n[1], d$meanDif[1], d$sd[1], 0.75)

## negative symptoms
## r = 0.25
var(d$n[2], d$meanDif[2], d$sd[2], 0.25)
## r = 0.50
var(d$n[2], d$meanDif[2], d$sd[2], 0.50)
## r = 0.75
var(d$n[2], d$meanDif[2], d$sd[2], 0.75)

## general symptoms
## r = 0.25
var(d$n[3], d$meanDif[3], d$sd[3], 0.25)
## r = 0.50
var(d$n[3], d$meanDif[3], d$sd[3], 0.50)
## r = 0.75
var(d$n[3], d$meanDif[3], d$sd[3], 0.75)

