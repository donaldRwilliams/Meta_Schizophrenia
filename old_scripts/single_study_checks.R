sd_change <- function(sd1, sd2 = sd1, r = 0.5) {
  # standard deviation of a mean change
  sqrt(sd1^2+sd2^2 - 2*sd1*sd2*r)
}

d <- function(mean1, mean2, sd1, sd2, n1, n2) {
  # cohens d
  (mean1-mean2) / sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
}

var_d <- function(d, n1, n2) {
  (n1 + n2) / (n1*n2) + d^2 / (2*(n1+n2))
}

t_hom <- function(mean1, mean2, sd1, sd2, n1, n2) {
  # t-test statistic for 2 groups with homogenques variance
  (mean1-mean2) / sqrt((1/n1 + 1/n2) * 
                         ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
}

# -------- checks related to Modabbernia et al. 2013 --------
# positive symptoms
sd_change(sd1 = 3.1, sd2 = 3.1, r = 0.8)  
# much greater than the sd_change = 0.9 reported for the placebo group...
(dm <- d(mean1 = 3.3, mean2 = 0.7, sd1 = 3.1, sd2 = 0.9, n1 = 20, n2 = 20))
2*sqrt(var_d(dm, 20, 20))
t(mean1 = 3.3, mean2 = 0.7, sd1 = 3.1, sd2 = 0.9, n1 = 20, n2 = 20)


# -------- checks related to Feifel et al. 2012 --------
# pre oxy paired t-test values
# recallDiscrim_CVLT
(2.22 - 1.77)/sd_change(0.71, 0.62, r = 0.5) * sqrt(15)
# reported value: 2.63

# sdFreeRecall_CVLT
(10.63 - 8.3)/sd_change(3.8, 3.5, r = 0.5) * sqrt(15)
# reported value: 2.47

# sdFreeRecall_CVLT oxy pla
(55.64 - 51.39)/sd_change(14.52, 16.9, r = 0.92) * sqrt(15)
# reported value: 2.47

# -> oxy pre correlation ~ 0.5


# -------- checks related to Michalopoulou et al. 2015 --------
# DB
sqrt(7.354)  # t = sqrt(F)
(5.9 - 5.38)/sd_change(2.55, 2.42, r = 0.5) * sqrt(21)
# cannot be correct...

# DC
sqrt(0.188)  # t = sqrt(F)
(9.81 - 9.95)/sd_change(2.87, 2.56, r = 0.85) * sqrt(21)
# correlation of 0.85 required (not realistic)

# DLC
sqrt(0.238)  # t = sqrt(F)
(50.33 - 50.94)/sd_change(18.09, 15.80, r = 0.95) * sqrt(21)
# correlation of 0.95 required (not realistic)


# -------- checks related to Davis et al. 2013 --------
sd_change(0.78, 0.5, r = 0.7)
