# RCT data from the National Supported Work Demonstration are
# available from Dehejia's website here:
https://users.nber.org/~rdehejia/nswdata2.html
# See the website for descriptions of variables.

# Download data from treated units:
trt <- read.table(file = "http://www.nber.org/~rdehejia/data/nsw_treated.txt")
ctr <- read.table(file = "http://www.nber.org/~rdehejia/data/nsw_control.txt")
lalonde <- rbind(trt, ctr)
names(lalonde) <- c("treat", "age", "yrs_educ", "black", "hisp", "married", 
                    "nodegree", "re75", "re78")
head(lalonde)

library(tidyverse)
lalonde %>% ggplot(aes(x = factor(treat), y = re78)) + 
  geom_boxplot()
lalonde %>% ggplot(aes(x = re78),) +
  geom_density(aes(color = factor(treat)))

# Means/vars by group
by(data = lalonde$re78, INDICES = lalonde$treat, FUN = mean)
by(data = lalonde$re78, INDICES = lalonde$treat, FUN = var)
library(car)
leveneTest(re78 ~ factor(treat), data = lalonde)

# Is this a significant increase? Did job training work?
# Run a regression.
lm1 <- lm(re78 ~ treat, data = lalonde)
summary(lm1)
# Treatment effect is estimated at $886.30 and is not
# significant (p = .061). 

# Run the two-sample t-test with Welch's df.
t.test(re78 ~ treat, data = lalonde, var.equal = FALSE)

# Run Kolmogorov-Smirnov test.
ks.test(x = lalonde$re78[lalonde$treat == 0],
        y = lalonde$re78[lalonde$treat == 1])
# Two-sided test is not significant (p = .091).

# Were there baseline differences in earnings?
by(data = lalonde$re75, INDICES = lalonde$treat, FUN = mean)
# Looks pretty balanced at baseline. Nevertheless, we can 
# often get better power to detect a treatment effect by
# controlling for a baseline measure of the outcome. One way 
# to do that is via regression.
lm2 <- lm(re78 ~ treat + re75, data = lalonde)
summary(lm2)

# It looks like the treatment group distribution may have a 
# heavier tail than the control group. 
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-whats-with-the-different-formulas-for-kurtosis/
# First, write a function to calculate the sample kurtosis.
# See the link above for the formula.
kurt <- function(vec) {
  n <- length(vec) # sample size
  s4 <- sum((vec - mean(vec))^4)
  kt <- ((n*(n+1))/((n-1)*(n-2)*(n-3)))*(s4/var(vec)^2) - 
    (3*(n-1)^2)/((n-2)*(n-3))
  return(kt) }
kurt(lalonde$re78[lalonde$treat == 1])
kurt(lalonde$re78[lalonde$treat == 0])

# One great strength of resampling methods is that we can 
# use them to learn about the sampling distributions of 
# statistics for which we do not have any analytical results.
# Suppose we wanted to test for a difference related to kurtosis
# instead of a difference in sample means. 

# Run an approximate permutation test to test the non-parametric
# null hypothesis that the treatment had no effect against the 
# alternative that the treatment did have an effect using the 
# sample kurtosis as the statistic.

# Write a function to sample from the permutation distribution
# using the boot() function in package boot. Given a data frame
# that has a treatment variable named treat and an outcome variable
# named re78, and permutation indexes named ind, the function will
# permute the outcome while holding the group labels fixed and then
# calculate and return sample kurtosis on the permuted data.
kurt_diff <- function(dat, ind) {
  trt <- dat$treat # group assignments are fixed
  re78 <- dat$re78[ind] # permute the outcome
  k0 <- kurt(re78[which(trt == 0)])
  k1 <- kurt(re78[which(trt == 1)])
  return(k1 - k0)
}

set.seed(3764)
boot_out <- boot(data = lalonde, 
                 statistic = kurt_diff, 
                 R = 1000, sim = "permutation")

# The statistics resulting from permutation are stored in 
# the output in a 1000 x 1 matrix called t.
hist(boot_out$t, 50)

# Calculate ASL (approximate significance level)
length(which(boot_out$t > boot_out$t0))/1000

# The p-value is 0.37, which is not significant. There is not 
# sufficient reason to conclude that the job training program 
# induced greater kurtosis (heavier tail) in the treatment
# group. Note that the reference distribution is bimodal and
# clearly not normal. 

# Ok, so this last test allowed us to test for a difference
# in kurtosis. But what if we believed that the treatment was
# likely to cause a change that both the sample mean and the 
# sample kurtosis should be sensitive to? Again, because of the
# flexibility of the permutation framework, we could construct
# a new statistic that combines the sample mean with the sample
# kurtosis in some way, perhaps by taking a weighted sum.

mean_kurt <- function(dat, ind) {
  trt <- dat$treat # group assignments are fixed
  re78 <- dat$re78[ind] # permute the outcome
  k0 <- kurt(re78[which(trt == 0)])
  k1 <- kurt(re78[which(trt == 1)])
  k_diff<- k1 - k0
  mn0 <- mean(re78[which(trt == 0)])
  mn1 <- mean(re78[which(trt == 1)])
  mn_diff <- mn1 - mn0
  out <- .7*mn_diff + .3*k_diff
}

set.seed(8593)
boot_out2 <- boot(data = lalonde, 
                 statistic = mean_kurt, 
                 R = 1000, sim = "permutation")

# The statistics resulting from permutation are stored in 
# the output in a 1000 x 1 matrix called t.
hist(boot_out2$t, 50)

# Calculate ASL (approximate significance level)
length(which(boot_out2$t > boot_out2$t0))/1000



# Your group work will involve the acupuncture data set. I've
# uploaded a csv file in the Misc folder in the Files section.

# Data for this group work come from a randomized experiment 
# to study the efficacy of acupuncture for treating headaches. 
# Results of the trial were published in the British Medical 
# Journal in 2004. You may view the paper at the following link:
# http://www.bmj.com/content/328/7442/744.full
# The data set includes 301 cases, 140 control (no acupuncture) 
# and 161 treated (acupuncture). Participants were randomly 
# assigned to groups. Variable names and descriptions are as follows:
# age; age in years
# sex; male = 0, female = 1
# migraine; diagnosis of migraines = 1, diagnosis of tension-type headaches = 0 
# chronicity; number of years of headache disorder at baseline
# acupuncturist; ID for acupuncture provider
# group; acupuncture treatment group = 1, control group = 0
# pk1; headache severity rating at baseline
# pk5; headache severity rating 1 year later

# Your task in group work today is to be done in three parts.

# Part 1 is to run the standard two-sample t-test to test
# if acupuncture significantly decreased headache pain in 
# study participants. Explore the assumptions of the t-test
# by examining the data through graphs.

df <- read.csv("acupuncture.csv")
head(df)



# Part 2 is to identify a non-standard test statistic that 
# your team suspects will also be sensitive to departures 
# from the null hypothesis of no treatment effect. 



# Part 3 is to use the non-standard test statistic in a 
# permutation framework to determine the approximate significance
# level (p-value).

# The task for homework is to write up the results of 1, 2, and 3.



