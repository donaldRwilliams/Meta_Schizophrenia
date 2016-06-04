# checks related to Modabbernia et al. 2013
sd_change <- function(sd1, sd2 = sd1, r = 0.5) {
  # standard deviation of a mean change
  sqrt(sd1^2+sd2^2-2*sd1*sd2*r)
}

d <- function(mean1, mean2, sd1, sd2, n1, n2) {
  # cohens d
  (mean1-mean2) / sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
}

t_hom <- function(mean1, mean2, sd1, sd2, n1, n2) {
  # t-test statistic for 2 groups with homogenques variance
  (mean1-mean2) / sqrt((1/n1 + 1/n2) * 
                         ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
}

# positive symptoms
sd_change(sd1 = 3.1, sd2 = 3.1, r = 0.8)  
# much greater than the sd_change = 0.9 reported for the placebo group...
d(mean1 = 3.3, mean2 = 0.7, sd1 = 3.1, sd2 = 0.9, n1 = 20, n2 = 20)
t(mean1 = 3.3, mean2 = 0.7, sd1 = 3.1, sd2 = 0.9, n1 = 20, n2 = 20)
