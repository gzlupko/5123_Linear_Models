#######
# 1. Import
#######


##### Lab Lecture

# Load package tidyverse
library(tidyverse)

# Import the data. This can be done via the "Files" tab
# in your plotting window pane in RStudio. Or, it can be
# one manually as follows, though, on your machine you 
# would need to enter the correct file path.
#dat <- read_csv(file = paste0("/Users/bryankeller/Dropbox",
                              "/Teaching/Linear Models and Experimental Design",
                              "/2021 FALL/00 Syllabus/smokingRCT.csv"))

dat <- read_csv("smokingRCT.csv") 

names(dat) # What are the names of the data columns?
dat # View the portion of the data that will fit in your console
print(dat, n = 20) # print 20 rows
print(dat, n = 10, width = Inf) # print 10 rows and 10 cols

#######
# 2. Clean
#######
# Variables to focus on:
# -Treatment group (Condition)
# -Age (Age)
# -Sex (Sex)
# -Depression at baseline (bdi_baseline)
# -Depression at the end of trial (bdi_EOT)
# -cigarette usage at baseline and eot

# Let's create a new data set that only has the variables 
# we are interested in working with
dat2 <- dat %>% 
  select(Condition, Age, Sex, 
         cigarettes_day_baseline, bdi_baseline, 
         bdi_EOT, Cigarettes_day_EOT)
dat2
# Rename some of the variables to have shorter names
names(dat2)
names(dat2)[4] <- "cig_base"
names(dat2)[5] <- "bdi_base"
names(dat2)[7] <- "cig_EOT"
names(dat2)
dat2

# Create factor versions of categorical variables: conditionF and sex 
table(dat2$Condition)
dat2$ConditionF <- factor(dat2$Condition, 
                          levels = c(1,2,3),
                          labels = c("SCB", "BA", "WL"))
dat2
table(dat2$ConditionF)

table(dat2$Sex)
dat2$SexF <- factor(dat2$Sex,
                    levels = c(1,2),
                    labels = c("Male", "Female"))
dat2
table(dat2$SexF)

#######
# 3a. Effect Size: Standardized Mean Difference
#######
# Although there are functions in packages that
# calculate Cohen's d automatically, none that I'm
# aware of will do this when a categorical factor 
# has more than two levels. So, we will do it "by
# hand" here.

# To calculate d we need the group means, the group
# variances, and the group sample sizes for the two
# groups involved. So let's begin by getting those 
# summary statistics.

# Group sample sizes can be taken from a table based
# on the treatment factor.
(t1 <- table(dat2$ConditionF))

# If we also want to know how many missing values 
# there are, we use 
table(dat2$ConditionF, useNA = "always")
table(dat2$cig_EOT, useNA = "always")
# There are no missing condition values, but there
# are missing values in the bdi_EOT scores.
dat2$cig_EOT

# There are a number of ways to handle missing data.
# Here we will take the approach of creating a sub-set
# of only the variables we need in a new data set 
# called dat3, and then we will eliminate all rows
# with missing data.
dat3 <- subset(x = dat2, 
               select = c("ConditionF", "cig_base", "cig_EOT"))
dim(dat3)
print(dat3, n = 20)

# Now eliminate rows with missing values (this is
# also called 'listwise deletion').
dat3 <- na.omit(dat3)
dim(dat3)
print(dat3, n = 20) # No more missing values.

# Now, let's make the table for group sample sizes again.
(ns <- table(dat3$ConditionF))

# Next, group sample means and variances.
(mns <- by(data = dat3$cig_EOT,
           INDICES = dat3$ConditionF,
           FUN = mean))
(vars <- by(data = dat3$cig_EOT,
            INDICES = dat3$ConditionF,
            FUN = var))

# Cohen's d 
# SCB vs BA
# below 'mns' gives us group mean per group, 'ns' gives us sample size 
(d12 <- (mns[1] - mns[2]) / sqrt(((ns[1] - 1)*vars[1] + (ns[2] - 1)*vars[2]) / (ns[1] + ns[2] - 2)))
# SCB vs WL
(d13 <- (mns[1] - mns[3]) / sqrt(((ns[1] - 1)*vars[1] + (ns[3] - 1)*vars[3]) / (ns[1] + ns[3] - 2)))
# BA vs Waitlist
(d23 <- (mns[2] - mns[3]) / sqrt(((ns[2] - 1)*vars[2] + (ns[3] - 1)*vars[3]) / (ns[2] + ns[3] - 2)))


#######
# 3b. Effect Size: Strength of Association
#######
# R-Squared from ANOVA output
lm_R <- lm(cig_EOT ~ 1, data = dat3)
lm_F <- lm(cig_EOT ~ ConditionF, data = dat3)
anova(lm_R, lm_F)

# lm() will use dummy coding by default 
# to change to effect coding use: 
# notice the values will 

options(constrasts = c("contr.sum", "contr.poly")) 
lm(bdi_EOT ~ ConditionF, data = dat3) %>% summary() 
# compare to dummy coding 


# R-Squared (correlation ratio)
(R_sq <- 427.84/(14398.96 + 427.84))

# Adjusted R-Squared
# adjusted R-Squared is subtantially different than R-Squared 
(R_sq_adj <- 1 - (219/217)*(1 - R_sq))

# Omega squared hat (see formulas on pp. 129)
(omega_sq_hat <- ((3 - 1)*(3.22 - 1)) / ((3 - 1)*(3.22 - 1) + 220))


#######
# 4. Checking Assumptions - Constant Variance
#######
# Four assumptions about the error term, the epsilon_i's, are 
# required to ensure the F statistic will have the stated
# F distribution under the null hypothesis.

# 4a. Constant variance assumption. For a continuous predictor, 
#     the assumption requires that the variance of the outcome
#     is identical across all values of the predictor.
# Generate data that satisfy the assumption with
# Y_i = 50 + .5X_i + epsilon_i where epsilon_i ~ N(0, 10^2)
set.seed(1234)
X <- rnorm(n = 200, mean = 100, sd = 15)
epsilon <- rnorm(n = 200, mean = 0, sd = 10)
Y <- 50 + .5*X + epsilon
plot(X,Y)
lm1 <- lm(Y ~ X)
plot(x = lm1$fitted.values, 
     y = lm1$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = 2, lwd = 2)


# Generate data that violate the assumption with
# Y_i = 50 + .5X_i + epsilon_i where epsilon_i ~ N(0, (10^2)*(X_i)
set.seed(1234)
X <- rnorm(n = 200, mean = 100, sd = 15)
epsilon <- rnorm(n = 200, mean = 0, sd = 20*(X - min(X))/diff(range(X)))
Y <- 50 + .5*X + epsilon
plot(X,Y)
lm2 <- lm(Y ~ X)
plot(x = lm2$fitted.values, 
     y = lm2$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = 2, lwd = 2)

# 4b. Constant variance for a categorical predictor. The meaning
#     is the same: the variance of the outcome variable must be 
#     identical within each and every level of the grouping factor.

# Three recommended practices to check the constant variance
# assumption.
# Practice 1: plot the data by group and examine spread.
boxplot(dat3$bdi_EOT ~ dat3$ConditionF)

# Practice 2: calculate variance ratios for each pair of
# groups. With small to moderate sample sizes, ratios larger
# than 3 or 4 are cause for concern.
by(data = dat3$cig_EOT, 
   INDICES = dat3$ConditionF, 
   FUN = var, 
   na.rm = TRUE)

# take the ratio of the largest variance to the smallest 
# Keller's rule of thumb is that if the ratio of the largest
# to the smallest is greater than 3 or 4 then the variances 
# are considerably unequal 
129.2/46.3 # = 2.79


# Practice 3: run a hypothesis test of the null hypothesis 
# that group variances are identical. Levene's test is one
# popular choice. Bartlett's test is also sometimes used
# but is much more sensitive to violation of the normality
# assumption than Levene's test, so Levene's is preferred.
library(car)
leveneTest(lm_F)

# Here, the variance in the control group is between 2 and
# three times larger than the variances in the treated groups.
# Although the Levene's test null is not rejected (p = .06), 
# based on the graphical evidence and the variance ratios, I'm
# concerned that the control group has the larger variance. To
# investigate a little bit further, I look at the group variances
# at baseline.
boxplot(dat3$bdi_base ~ dat3$ConditionF)
by(data = dat3$bdi_base, 
   INDICES = dat3$ConditionF, 
   FUN = var, 
   na.rm = TRUE)
lm_base <- lm(bdi_base ~ ConditionF, data = dat3)
leveneTest(lm_base)

# The evidence is very clear that constant variance was
# tenable at baseline. Thus, it appears that the act of 
# assigning participants to a treatment group suppressed
# the variance of the outcome when measured at EOT. Given
# that group sample sizes are unequal, and that the smallest 
# group (WL) has the largest variance, the F test will likely 
# be anti-conservative.

#######
# 5. Checking Assumptions - Normality
#######
# The idea with the normality assumption is that no matter
# the value of the covariate, the residuals are normally 
# distributed around the mean regression line. With categorical
# predictors, this means that residuals should be normally 
# distributed within groups.

# Three recommended practices to check normality.
# Practice 1: visually assess the via histograms 
# or boxplots by group.
ggplot(data = dat3, mapping = aes(x = cig_EOT)) +
  geom_histogram(bins = 10, color = "black", fill = "white") + 
  facet_wrap(~ConditionF) + ylab("Cigarettes Per Day") + 
  xlab("Condition") 
ggplot(data = dat3, mapping = aes(x = cig_EOT)) +
  geom_histogram(bins = 20, color = "black", fill = "white") + 
  facet_wrap(~ConditionF) + ylab("Cigarettes Per Day") + 
  xlab("Condition") 


# You may play with the number of bins 
#ggplot(data = dat3, mapping = aes(x = bdi_EOT)) +
  geom_histogram(bins = 20, color = "black", fill = "white") + 
  facet_wrap(~ConditionF)
#ggplot(data = dat3, mapping = aes(x = bdi_EOT)) +
  geom_histogram(bins = 30, color = "black", fill = "white") + 
  facet_wrap(~ConditionF)

# Boxplots by group are also helpful in visually assessing 
# skewness and excess kurtosis
ggplot(data = dat3, mapping = aes(x = ConditionF, y = cig_EOT)) +
  geom_boxplot(color = "black", fill = "white") + 
  xlab("Condition") + ylab("Cigarettes Per Day")

# Practice 2: create and examine a QQ-plot.
# A quantile-quantile plot (or QQ plot) is a graphical method 
# for comparing two distributions. To the extent that the points 
# correspond, they will lie along a 45 degree line. To the extent 
# that the residuals differ from the theoretical expectation, they 
# will fall off of the line. The blue dashed curves are 95% confidence
# envelopes based on bootstrap resampling.
library(car)
qqPlot(lm_F)

# Practice 3: run a statistical test of the null hypothesis
# that the distribution from which data in each group were
# generated is normal. The Shapiro-Wilk test is one such 
# test. A small p-value represents evidence against the 
# null hypothesis that the generating distribution is normal.
?shapiro.test
by(data = dat3$cig_EOT, 
   INDICES = dat3$ConditionF,
   FUN = shapiro.test)


#######
# 6. Your tasks
#######
# 1. Estimate pairwise values of Cohen’s d for the cigarette 
#    usage outcome. Be sure to handle missing data appropriately
#    in determining the group sample sizes. Report and interpret 
#    your results in terms of number of pooled SDs and comment on
#    the magnitude of the effects based on Cohen’s (1988) guidelines.


# 2. Calculate R^2, adjusted R^2, and omega_hat^2 for the cigarette 
#    outcome. Comment on which you prefer to interpret and why. 
#    Interpret the version you select in terms of variance explained 
#    and comment on the magnitude of the effect based on Cohen’s 
#    (1988) guidelines for R^2, which you may find online.

 
# 3. Create boxplots and histograms of the cigarette outcome by
#    group and comment on the tenability of the assumptions of
#    constant variance and normality based on the plots.
# don't need to do hypotheses tests just the EDA 





