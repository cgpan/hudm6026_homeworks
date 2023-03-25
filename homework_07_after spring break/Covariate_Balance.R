# Load the acupuncture csv file
acupuncture <- read.csv(file = file.choose()) # browse for it, or call directly:
acupuncture <- read.csv(file = "~/Dropbox/Teaching/Linear Models and Experimental Design/2021 SPRING/09 ANCOVA/acupuncture.csv")

library(tidyverse)
acupuncture <- tibble(acupuncture)
acupuncture

# Convert group to a factor
str(acupuncture)
acupuncture$group <- factor(acupuncture$group,
                            levels = c(0, 1),
                            labels = c("C", "T"))
str(acupuncture)
levels(acupuncture$group)
acupuncture

# Some visualizations
boxplot(acupuncture$pk1 ~ acupuncture$group, 
        horizontal = TRUE, las = 1,
        xlab = "Pain Rating at Baseline",
        ylab = "Treatment Group",
        main = "Baseline Pain by Group")

boxplot(acupuncture$pk5 ~ acupuncture$group,
        xlab = "Pain Rating After 1 Year",
        ylab = "Treatment Group",
        main = "Posttest (1 yr) Pain by Group",
        horizontal = TRUE, las = 1)

# Assess baseline balance
# Check for baseline balance across treatment conditions on 
# age, sex, migraine, chronicity, and baseline severity to 
# see if randomization resulted in balance across groups,
# as expected.

# Calculate covariate means by treatment status
(mns_age <- by(data = acupuncture$age, INDICES = acupuncture$group, FUN = mean))
(mns_sex <- by(data = acupuncture$sex, INDICES = acupuncture$group, FUN = mean))
(mns_mig <- by(data = acupuncture$migraine, INDICES = acupuncture$group, FUN = mean))
(mns_chr <- by(data = acupuncture$chronicity, INDICES = acupuncture$group, FUN = mean))
(mns_pk1 <- by(data = acupuncture$pk1, INDICES = acupuncture$group, FUN = mean))

# Group sample sizes
ns <- table(acupuncture$group)

# Calculate covariate group variances by treatment status
(vars_age <- by(data = acupuncture$age, INDICES = acupuncture$group, FUN = var))
(vars_sex <- by(data = acupuncture$sex, INDICES = acupuncture$group, FUN = var))
(vars_mig <- by(data = acupuncture$migraine, INDICES = acupuncture$group, FUN = var))
(vars_chr <- by(data = acupuncture$chronicity, INDICES = acupuncture$group, FUN = var))
(vars_pk1 <- by(data = acupuncture$pk1, INDICES = acupuncture$group, FUN = var))

# Cohen's d
(d_age <- (mns_age[2] - mns_age[1]) / sqrt(((ns[2] - 1)*vars_age[2] + (ns[1] - 1)*vars_age[1]) / (ns[1] + ns[2] - 2)))
(d_sex <- (mns_sex[2] - mns_sex[1]) / sqrt(((ns[2] - 1)*vars_sex[2] + (ns[1] - 1)*vars_sex[1]) / (ns[1] + ns[2] - 2)))
(d_mig <- (mns_mig[2] - mns_mig[1]) / sqrt(((ns[2] - 1)*vars_mig[2] + (ns[1] - 1)*vars_mig[1]) / (ns[1] + ns[2] - 2)))
(d_chr <- (mns_chr[2] - mns_chr[1]) / sqrt(((ns[2] - 1)*vars_chr[2] + (ns[1] - 1)*vars_chr[1]) / (ns[1] + ns[2] - 2)))
(d_pk1 <- (mns_pk1[2] - mns_pk1[1]) / sqrt(((ns[2] - 1)*vars_pk1[2] + (ns[1] - 1)*vars_pk1[1]) / (ns[1] + ns[2] - 2)))

# Variance ratios]
(vr_age <- vars_age[2]/vars_age[1])
(vr_sex <- vars_sex[2]/vars_sex[1])
(vr_mig <- vars_mig[2]/vars_mig[1])
(vr_chr <- vars_chr[2]/vars_chr[1])
(vr_pk1 <- vars_pk1[2]/vars_pk1[1])

# This is a great example of when a function would be useful.
bal_check <- function(cv, gp) {
  # cv is the covariate
  # gp is the group vector with control group as refernce
  ns <- table(gp)
  mns <- by(cv, gp, mean, na.rm = TRUE)
  vars <- by(cv, gp, var, na.rm = TRUE)
  d <- (mns[2] - mns[1]) / sqrt(((ns[2] - 1)*vars[2] + (ns[1] - 1)*vars[1]) / (ns[1] + ns[2] - 2))
  r <- vars[2]/vars[1]
  out <- c(d, r); names(out) <- c("d", "r")
  out }

# Apply the function to test it
bal_check(cv = acupuncture$age, gp = acupuncture$group)
bal_check(cv = acupuncture$sex, gp = acupuncture$group)
bal_check(cv = acupuncture$migraine, gp = acupuncture$group)
bal_check(cv = acupuncture$chronicity, gp = acupuncture$group)
bal_check(cv = acupuncture$pk1, gp = acupuncture$group)

# Create a new function that uses the bal_check() function
# to make a plot
bal_plot <- function(dat, cov_names, gp) {
  # dat is the data frame where variables can be found
  # cov_names is the list of names of covariates
  # gp is group factor name
  out <- sapply(X = dat[, cov_names], FUN = bal_check, gp = dat[, gp])
  plot(t(out), pch = 19, 
       xlim = range(out[1,]) + c(-.1, .1),
       ylim = range(out[2,]) + c(-.2, .2),
       xlab = "Cohen's d (T - C)",
       ylab = "Variance Ratio (T/C)")
  abline(v = c(-.1, .1), lty = 2)
  abline(v = 0); abline(h = 1)
  abline(h = c(4/5, 5/4), lty = 2)
  text(t(out), labels = rownames(t(out)), pos = 1)
  out
}

# Use the function with our data
out <- bal_plot(dat = acupuncture,
         cov_names = c("age", "sex", "migraine", "chronicity", "pk1"),
         gp = "group")
out

# The baseline imbalance on pk1 is cause for concern.
d_pk1 # -0.14 (outside of [-0.1, 0.1])
vr_pk1 # 0.71 (outside of [0.8, 1.25])

# Check ANOVA assumptions; start by fitting
# the ANOVA full model
lm1 <- lm(pk5 ~ group, data = acupuncture)
summary(lm1)

# Constant residual variance
library(car)
leveneTest(lm1)
by(data = acupuncture$pk5, 
   INDICES = acupuncture$group,
   FUN = var)
(161/140)*(289/188) # 1.77 < 4

# Residual normality
qqPlot(lm1)
hist(acupuncture$pk5)

# Use a transformation log(pk5 + 1) to attempt to 
# fix violated normality assumption
lm2 <- lm(log(pk5 + 1) ~ group, data = acupuncture)
qqPlot(lm2)
hist(log(acupuncture$pk5 + 1))

# The transformation helped with constant variance
leveneTest(lm2)
by(data = log(acupuncture$pk5 + 1), 
   INDICES = acupuncture$group,
   FUN = var)
(161/140) * (.66/.53) # 1.43 < 4

# Was the intervention effective? Check
# using both the untransformed and transformed
# data
Anova(lm1, type = 3)
Anova(lm2, type = 3)

# Same conclusions so we will use the untransformed
# data because it is easier to interpret.

library(emmeans)
(emm1 <- emmeans(object = lm1, specs = ~ group))
pairs(emm1)
summary(lm1)

(emm2 <- emmeans(object = lm2, specs = ~ group))
pairs(emm2)

# Treatment effect estimated to be 6.1 unit improvement
# for acupuncture group at one-year follow-up.

############################
# ANCOVA
############################
# Consider controlling for baseline level of severity 
# (pk1) as a covariate. First check for a strong linear 
# relationship with the outcome, pk5.
cor(acupuncture$pk1, acupuncture$pk5)
plot(x = acupuncture$pk1, y = acupuncture$pk5)
cor.test(acupuncture$pk1, acupuncture$pk5)

# The test of the slope coefficient from a regression 
# of pk5 on pk1 will test if the linear relationship 
# is significant. As expected, it is highly significant 
# (p-value < .0001).
lm_slope <- lm(pk5 ~ pk1, data = acupuncture)
summary(lm_slope)

# The ANCOVA model in lm3, below, assumes no interaction
# between treatment group and the covariate. This can be 
# tested by running the full model that includes the 
# interaction term.
lm3 <- lm(pk5 ~ group + pk1 + group:pk1,
          data = acupuncture)
Anova(lm3, type = 3)

# The interaction is significant (p = .0017), which violates the 
# assumption of no treatment by covariate interaction
# in ANCOVA. We will continue with traditional ANCOVA
# only to demonstrate how it is fit and interpreted.
# After that, we will run a more appropriate model that 
# includes the interaction. The trouble with the traditional 
# ANCOVA model is that it assumes constant effect of
# treatment for all values of pretest (pk1). The presence of
# a significant interaction tells us that this is not the case.
# Instead, the size of the treatment effect varies with baseline
# pain, pk1.

# ANCOVA full model (not appropriate because of
# treatment group by covariate interaction):
lm3 <- lm(pk5 ~ group + pk1, data = acupuncture)
Anova(lm3, type = 3)

# Model parameters
summary(lm3)

# Fit emmeans here to get estimated marginal
# means for each group, controlling for the
# linear relationship between pk1 and pk5.
# By default, the emmeans() function will plug 
# in the means (averages) of any continuous
# covariates that are averaged over. Here, we
# ask it to average over pk1, so it uses the 
# mean of that variable by default.
emm3 <- emmeans(object = lm3,
                specs = ~ group)
emm3

# Treatment effect estimate is -4.59. That is, 
# acupuncture treatment reduced average pain rating
# by about 4.6 units, after controlling for the linear
# relationship between baseline pain and the outcome.
pairs(emm3, adjust = "none")

# We could explicitly include pk1 in the specifiction
# to see the means of pk1 that are plugged in.
emm4 <- emmeans(object = lm3,
                specs = ~ group + pk1)
emm4

# Note the values of 25.6 
mean(acupuncture$pk1)

# It is also possible to ask for calculations at 
# different values than the mean by taking the summary 
# of a call to the ref_grid() fucntion.
grid1 <- ref_grid(object = lm3, 
                  at = list(pk1 = 15))
summary(grid1)

# We can test for a significant difference at the value
# of pk1, as well, though the result will be the same
# because this model (lm3) models a constant treatment
# effect across all values of pk1.
pairs(grid1)

# This can be seen clearly from the plot based on lm3.
plot(pk5 ~ pk1,
     data = acupuncture, 
     xlab = "Baseline Severity Rating (pk1)",
     ylab = "1 Year Severity Rating (pk5)",
     pch = c(19, 0)[(as.character(acupuncture$group) == "T") + 1],
     col = (as.character(acupuncture$group) == "T") + 1,
     main = "Model Assuming No Interaction")
legend(x = "bottomright", 
       lwd = 2, col = 1:2, pch = c(19, 0), lty = 1:2,
       legend = c("Control", "Treatment"), seg.len = 4)

# Two models (by group) based on the ANCOVA model
summary(lm3)
# Control: pk5 = 1.16 + .71*pk1
abline(a = 1.16, b = .71, col = 1, lwd = 2)
# Treated: pk5 = 1.16 - 2.29 + .71*pk1 = -1.13 + .71*pk1
abline(a = -1.13, b = .71, col = 2, lwd = 2, lty = 2)

# More on handling the treatment by covariate interaction next time

#######
# Tasks for lab
#######
# 1. Create a plot of baseline chronicity on the x-axis vs
#    1 year severity rating (pk5) on the y-axis. (With good
#    labeling!)

# 2. Report on baseline balance via Cohen's d and the variance 
#    ratio for the chronicity variable. Was the variable 
#    sufficiently balanced at baseline or are you concerned
#    that it is imbalanced? Why?

# 3. Does baseline chronicity seem an appropriate choice
#    for an ANCOVA covariate? Support your answer by discussing
#    the strength of it's linear relationship with the outcome and
#    whether there is evidence that it interacts with treatment.

# 4. No matter what you conlcluded above, run the ANCOVA using
#    chronicity as baseline covariate and report your results
#    in context. I.e., was the acupuncture treatment effective 
#    or not and by how much?

