p2t <- function(p, n, alternative = c("two.sided", "less", "greater")) {
  # transform a p-value back to a t-value
  alternative <- match.arg(alternative)
  if (alternative == "two.sided") {
    p <- p / 2
  }
  abs(qt(p, df = n - 1))
}

cor_rep <- function(mean1, mean2, sd1, sd2, n, t) {
  # find correlation of a two variables based on t-values
  -(n * (mean1 - mean2)^2 / t^2 - sd1^2 - sd2^2) / (2 * sd1 * sd2)
}

t_rep <- function(mean1, mean2, sd1, sd2, n, r) {
  # t-test statistic for 2 dependent groups; inverse of cor_rep
  (mean1 - mean2) / sqrt(sd1^2+sd2^2 - 2*sd1*sd2*r) * sqrt(n)
}

t_hom <- function(mean1, mean2, sd1, sd2, n1, n2) {
  # t-test statistic for 2 groups with homogenques variance
  (mean1-mean2) / sqrt((1/n1 + 1/n2) * 
                         ((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
}

# validate cor_rep
a <- rnorm(100)
b <- rnorm(100, mean = a)
(t <- t.test(a, b, paired = TRUE))
# is equal 
cor(a, b)
cor_rep(mean1 = mean(a), mean2 = mean(b), sd1 = sd(a), sd2 = sd(b), 
        n = 100, t = p2t(t$p.value, 100))

# Michalopulou 2014
subdata <- subset(cdata, study == "Michalopoulou (2014)")
(t <- p2t(c(0.014, 0.669, 0.632), n = 21))
(r <- cor_rep(mean1 = subdata$oxyMean_post, 
              mean2 = subdata$plaMean_post, 
              sd1 = subdata$oxySd_post, 
              sd2 = subdata$plaSd_post, 
              n = 21, t = t))
t_rep(mean1 = subdata$oxyMean_post, 
      mean2 = subdata$plaMean_post, 
      sd1 = subdata$oxySd_post, 
      sd2 = subdata$plaSd_post, 
      n = 21, r = r)


# Guastella 2015
subdata <- subset(cdata, study == "Guastella et al. (2015)")
(t <- p2t(c(0.53, 0.01, 0.07, 0.08, 0.79, 0.92, 0.02, 0.03), n = 10))
(r <- cor_rep(mean1 = subdata$oxyMean_post, 
              mean2 = subdata$plaMean_post, 
              sd1 = subdata$oxySd_post, 
              sd2 = subdata$plaSd_post, 
              n = 10, t = t))
# up to r = 0.985, not too realistic...
t_rep(mean1 = subdata$oxyMean_post, 
      mean2 = subdata$plaMean_post, 
      sd1 = subdata$oxySd_post, 
      sd2 = subdata$plaSd_post, 
      n = 10, r = r)

t_hom(mean1 = subdata$oxyMean_post, 
      mean2 = subdata$plaMean_post, 
      sd1 = subdata$oxySd_post, 
      sd2 = subdata$plaSd_post, 
      n1 = 10, n2 = 10)


# Woolley 2014
subdata <- subset(cdata, study == "Woolley et al. (2014)")
n <- subdata$oxyN[1]
(t <- p2t(c(0.92, 0.93, 0.80, 0.14, 0.01, 0.33, 0.06, 0.01), n = n))
(r <- cor_rep(mean1 = subdata$oxyMean_post, 
              mean2 = subdata$plaMean_post, 
              sd1 = subdata$oxySd_post, 
              sd2 = subdata$plaSd_post, 
              n = n, t = t))
t_rep(mean1 = subdata$oxyMean_post, 
      mean2 = subdata$plaMean_post, 
      sd1 = subdata$oxySd_post, 
      sd2 = subdata$plaSd_post, 
      n = n, r = r)

# Fischer-Shofty 2013
subdata <- subset(cdata, study == "Fischer-Shofty et al. (2013)")
n <- subdata$oxyN[1]
(t <- p2t(c(0.048, 0.325), n = c(34, 35)))
(r <- cor_rep(mean1 = subdata$oxyMean_post, 
              mean2 = subdata$plaMean_post, 
              sd1 = subdata$oxySd_post, 
              sd2 = subdata$plaSd_post, 
              n = n, t = t))
t_rep(mean1 = subdata$oxyMean_post, 
      mean2 = subdata$plaMean_post, 
      sd1 = subdata$oxySd_post, 
      sd2 = subdata$plaSd_post, 
      n = n, r = r)

# Feifel 2012
subdata <- subset(cdata, study == "Feifel et al. (2012)")
n <- subdata$oxyN[1]
(t <- p2t(c(0.027, 0.02, 0.032, NA, NA, NA), n = n))
(r <- cor_rep(mean1 = subdata$oxyMean_post, 
              mean2 = subdata$plaMean_post, 
              sd1 = subdata$oxySd_post, 
              sd2 = subdata$plaSd_post, 
              n = n, t = t))
t_rep(mean1 = subdata$oxyMean_post, 
      mean2 = subdata$plaMean_post, 
      sd1 = subdata$oxySd_post, 
      sd2 = subdata$plaSd_post, 
      n = n, r = r)

# Averbeck 2012
subdata <- subset(cdata, study == "Averbeck et al. (2012)")
n <- subdata$oxyN[1]
(t <- p2t(c(0.4, 0.553, 0.059, NA, 0.132, 0.748), n = n))
(r <- cor_rep(mean1 = subdata$oxyMean_post, 
              mean2 = subdata$plaMean_post, 
              sd1 = subdata$oxySd_post, 
              sd2 = subdata$plaSd_post, 
              n = n, t = t))
t_rep(mean1 = subdata$oxyMean_post, 
      mean2 = subdata$plaMean_post, 
      sd1 = subdata$oxySd_post, 
      sd2 = subdata$plaSd_post, 
      n = n, r = r)

