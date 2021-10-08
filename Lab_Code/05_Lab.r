#######
# 1. Import
#######

# Import the data. This can be done via the "Files" tab
# in your plotting window pane in RStudio. Or, it can be
# one manually as follows, though, on your machine you 
# would need to enter the correct file path.
library(tidyverse)



dat <- read_csv("smokingRCT.csv") 
names(dat) 

#######
# 2. Clean
#######
# Variables to focus on:
# -Treatment group (Condition)
# -Age (Age)
# -Sex (Sex)
# -Baseline smoking (cigarettes_day_baseline)
# -Number of years smoker (Number_years_smoking)
# -Depression at baseline (bdi_baseline)
# -Depression at the end of trial (bdi_EOT)
# -Smoking at end of trial (Cigarettes_day_EOT)

# Let's create a new data set that only has the variables 
# we are interested in working with
dat2 <- subset(x = dat, 
               select = c("Condition", "Age", "Sex", 
                          "cigarettes_day_baseline",
                          "Number_years_smoking", "bdi_baseline", 
                          "bdi_EOT", "Cigarettes_day_EOT"))
head(dat2)
# Rename some of the variables to have shorter names
names(dat2)
names(dat2)[4] <- "cig_base"
names(dat2)[5] <- "yrs_smoke"
names(dat2)[6] <- "bdi_base"
names(dat2)[8] <- "cig_EOT"
names(dat2)
head(dat2)

# Check structure of data to see class of variables
str(dat2)

# Create factor versions of categorical variables
table(dat2$Condition)
dat2$ConditionF <- factor(dat2$Condition, 
                          levels = c(1,2,3),
                          labels = c("SCBSCT", "SCBSCT-BA", "WL"))
table(dat2$ConditionF)

table(dat2$Sex)
dat2$SexF <- factor(dat2$Sex,
                    levels = c(1,2),
                    labels = c("Male", "Female"))
table(dat2$SexF)

# Examine the first 6 rows after cleaning
head(dat2)

#######
# 3. One-way ANOVA
#######
# Check the constant variance assumption. This
# assumption asserts that the group variances 
# in the population are identical for each of the
# experimental groups. We will check this assumption
# in general by three methods. 1. Compute estimated
# group variances and compute the ratio of the 
# largest to the smallest. 2. Examine IQRs via 
# parallel boxplots. 3. Run a statistical test
# such as Levene's test of the null hypothesis
# that the group variances are identical.

# 0. Fit the full and restricted models.
lmF <- lm(cig_EOT ~ ConditionF, data = dat2)
lmR <- lm(cig_EOT ~ 1, data = dat2)

# 1. Compute estimated group variances.
by(data = dat2$cig_EOT,
   INDICES = dat2$ConditionF,
   FUN = var, na.rm = TRUE)

# Compare to rule of thumb from p. 155 in MDK.
# Need group sample sizes.
table(dat2$Condition)

dat3 <- select(dat2, ConditionF, cig_EOT)

dat3 <- na.omit(dat3) 

table(dat3$ConditionF) 


(mdk_ratio <- (46.16/17.79)*(96/46))
# Since the value of the ratio is larger than 4, 
# the rule of thumb suggests heterogeneity of 
# variance is problematic here.

# 2. Examine the boxplot of outcome by group.
boxplot(dat3$cig_EOT ~ dat3$ConditionF,
        xlab = "Condition",
        ylab = "Cig at EOT")

# 3. Levene's test
library(car)
leveneTest(lmF)

# Check the normality assumption. This assumption 
# asserts that the distribution of the outcome (or, 
# identically, the residuals) is normal within each
# experimental group and that the residuals, in 
# aggregate, are normally distributed. We can check 
# this by making group-specific histograms and by 
# examining a QQ plot of residuals.

# 1. Group-specific histograms.
# In base R:
# Now put all in same plot.
par(mfrow = c(1, 3))
hist(dat2$cig_EOT[which(dat2ConditionF == "SCBSCT")],
     breaks = 20)
hist(dat2$cig_EOT[which(dat2$ConditionF == "SCBSCT-BA")],
     breaks = 20)
hist(dat2$cig_EOT[which(dat2$ConditionF == "WL")],
     breaks = 20)
par(mfrow = c(1, 1))

# With ggplot2
ggplot(dat2, aes(x = cig_EOT)) + 
   geom_histogram(bins = 10, color = "black", fill = "white") + 
   facet_wrap(~ConditionF)

# Create a QQ plot using the qqPlot() function in package car.
qqPlot(lmF)


# shapiro wilke test 
by(dat3$cig_EOT, 
   INDICES = dat3$ConditionF, 
   FUN = shapiro.test)   



# In this case, it seems like both the constant variance
# and normality assumptions are violated. Recall, that 
# in cases where there are unequal group sizes, if the 
# smaller group is paired with the larger variance, the 
# ANOVA F test p-value will be too small. Here, we should
# not trust the p-value based on the one-way ANOVA because 
# of this pattern of violation.

# Run the ANOVA omnibus test via general linear F test
# through the anova() function.
anova(lmR, lmF)

# Can implement the Welch correction by using the 
# oneway.test function with var.equal = FALSE.
oneway.test(formula = cig_EOT ~ ConditionF,
            data = dat2, 
            na.action = "na.omit",
            var.equal = FALSE)

# Note that when using Welch's test, the p-value is .14, 
# which is about 3 times as large as the p-value from the
# unadjusted one-way ANOVA.

#######
# 4. Pairwise Contrasts
#######
# To run pairwise contrasts, we need to set up
# and test contrasts of the following form:
# psi1 = 1 mu1 - 1 mu2 + 0 mu3
# psi2 = 1 mu1 + 0 mu2 - 1 mu3
# psi3 = 0 mu1 + 1 mu2 - 1 mu3

# This can be done automatically using the emmeans 
# package. Install it if you don't have it already.
# emmeans function wants two arguments - a model and 
# the specification; 
# in our case we want the model to be broken out by treatment group 
library(emmeans)
emm1 <- emmeans(object = lmF,
                specs = ~ ConditionF)
emm1 # outputs the means and CI for each mean 
pairs(emm1, adjust = "none")

# we find that there is a non-sig diff between treatments
# but there is a sig diff between the treatment groups and the weight list (control)

# caveat here is that the test we ran assumes constant variance across groups
# caveat is also that we did not correct for multiple testing (which we will cover next week) 

# Let's double check the calculations "by hand" for the
# second comparison of SCBSCT vs WL.
mns <- by(dat3$cig_EOT, 
          INDICES = dat3$ConditionF, 
          FUN = mean, na.rm = T) 
(psi_2_hat <- 1*mns[1] + 0*mns[2] - 1*mns[3])

# Get group sample sizes after deleting missing data.
table(dat2$ConditionF, is.na(dat2$cig_EOT))

# Calculate the F statistic (and t statistic)
(F_obs <- (psi_2_hat^2 / (1^2/79 + (0)^2/95 + (-1)^2/46)) / 66.35)
(t_obs <- (psi_2_hat / sqrt((1^2/79 + (0)^2/95 + (-1)^2/46))) / sqrt(66.35)) 

# Determine the p-value
(p_valF <- pf(q = F_obs, df1 = 1, df2 = 217, lower.tail = FALSE))
2*(p_valt <- pt(q = t_obs, df = 217, lower.tail = TRUE))

#######
# 5. Complex Contrasts
#######
# Test the null hypothesis that the average of the two 
# CBT groups have a lower mean BDI score at EOT as compared
# with the WL control group.
# psi4 = 1/2 mu1 + 1/2 mu2 - 1 mu3

# Again, this may be tested automatically in emmeans.
# use the contrast() function 
# you input a model and then post the coefficients
psi4 <- contrast(emm1, 
                 list(psi4 = c(1/2, 1/2, -1)))
psi4    
pt(-2.497, df = 217, lower.tail = TRUE)
# Check "by hand".
(psi_4_hat <- 0.5*mns[1] + 0.5*mns[2] - 1*mns[3])

# t statistic
(t_obs4 <- (psi_4_hat / sqrt((.5^2/79 + (.5)^2/95 + (-1)^2/46))) / sqrt(66.35))

# Determine the p-value
# The alternative hypothesis here is that the average BDI score
# of CBT groups is LESS THAN the mean WL control group score.
# Thus, we want the area in the lower tail for our p-value.
(p_val4 <- pt(q = t_obs4, df = 217, lower.tail = TRUE))

#######
# 6. The Welch-Satterthwhaite Correction
#######
# First, create a smaller data set and eliminate 
# missing variables.
dat3 <- select(dat2, cig_EOT, ConditionF)
dim(dat3)
dat3 <- na.omit(dat3)
dim(dat3)
# Then use gls() function in package nlme to fit the full model. 

library(nlme)
gls1 <- gls(cig_EOT ~ ConditionF, 
            data = dat3,
            na.action = na.omit, # drop cases with missing values
            weights = varIdent(form = ~1 | ConditionF))
emm2 <- emmeans(gls1, specs = ~ ConditionF, mode = "sat") # specifies a Satterthwhaite correction 
emm2

pairs(emm2, adjust = "none")

contrast(emm2, list(psi_1 = c(1, -1, 0)))  
contrast(emm2, list(psi_2 = c(1, 0, -1))) 
contrast(emm2, list(psi_3 = c(0, 1, -1)))


(psi_2_hat <- 1*mns[1] + 0*mns[2] - 1*mns[3])




#######
# 7. SPSS
#######
# One-way ANOVA omnibus test:
# 1. Import the original .csv file to SPSS.
# 2. Go to ANALYZE ==> COMPARE MEANS ==> ONE-WAY ANOVA
# 3. Move bdi_EOT to the dependent list and Condition
#    to the factor list.
# 4. Click OPTIONS and select WELCH TEST and continue.
# 5. Click OK to run.

# Pairwise contrast testing:
# 1. Click CONTRASTS from the One-Way ANOVA screen.
# 2. Enter coefficients 1, -1, and 0, hitting ADD after
#    each one. When all three are entered, hit NEXT.
#    Then enter 1, 0, -1 and hit NEXT. Finally, 0, 1, -1.
# 3. Hit CONTINUE and then OK to run. 

# The process for the complex contrast is identical.
# Reset and then only enter the one contrast with 
# coefficients 1/2, 1/2, -1.

#######
# 8. Your tasks
#######
# 1. Use and report results from the three methods to 
#    check constant variance and normality assumptions
#    for the cigarette EOT outcome data. Based on your 
#    analyses, are tests of the omnibus null hypothesis 
#    that assume equal group variances more or less 
#    likely to find spurious results than they should be?


# 2. Run and report on all pairwise comparisons for 
#    the cigarette smoking outcome at EOT using dfs 
#    that have been adjusted via the Welch-Satterthwaite
#    equation. Run these both in R and SPSS and confirm
#    that your results agree.