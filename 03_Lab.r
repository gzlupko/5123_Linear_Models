#######
# 0. Preliminaries
#######

# Install the tidyverse package by going to the
# "Packages" tab in the Plotting pane.

# Load the package
library(tidyverse)

#######
# 1. Import
#######

# Import the data. This can be done via the "Files" tab
# in your plotting window pane in RStudio. Or, it can be
# one manually as follows.

# Because the data are in csv format, we will use read_csv()
dat <- read_csv(file = paste0("/Users/bryankeller/Dropbox",
                              "/Teaching/Linear Models and Experimental Design",
                              "/2021 SPRING/00 Syllabus/smokingRCT.csv"))
names(dat) # What are the names of the data columns?
dat # View the portion of the data that will fit in your console
print(dat, n = 20) # print 20 rows
print(dat, n = 10, width = 10) # print 10 rows and 10 cols

#######
# 2. Clean
#######
# Variables to focus on:
# -Treatment group (Condition)
# -Age (Age)
# -Sex (Sex)
# -Depression at baseline (bdi_baseline)
# -Depression at the end of trial (bdi_EOT)

# Let's create a new data set that only has the variables 
# we are interested in working with
dat2 <- dat %>% 
   select(Condition, Age, Sex, 
          cigarettes_day_baseline, bdi_baseline, 
          bdi_EOT, Cigarettes_day_EOT)
head(dat2)
# Rename some of the variables to have shorter names
names(dat2)
names(dat2)[4] <- "cig_base"
names(dat2)[5] <- "bdi_base"
names(dat2)[7] <- "cig_EOT"
names(dat2)
head(dat2)

# Create factor versions of categorical variables
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
# 3. Examine balance on baseline variables
#######
# Graphically examine the depression scores by
# group at BASELINE. Is there balance?
boxplot(bdi_base ~ ConditionF,
        data = dat2,
        xlab = "Experimental Condition",
        ylab = "Beck Depression Score",
        main = "Baseline Balance Plot")

# Or in ggplot2, as follows.
ggplot(data = dat2, mapping = aes(x = ConditionF, y = bdi_base)) + 
   geom_boxplot() + 
   xlab("Experimental Condition") + 
   ylab("Beck Depression Score") + 
   ggtitle("Baseline Balance Plot")

# Also check baseline balance on cigarette
# smoking.
boxplot(cig_base ~ ConditionF,
        data = dat2,
        xlab = "Experimental Condition",
        ylab = "Cigarettes Per Day",
        main = "Baseline Balance Plot")

# Or in ggplot2, as follows.
ggplot(data = dat2, mapping = aes(x = ConditionF, y = cig_base)) + 
   geom_boxplot() + 
   xlab("Experimental Condition") + 
   ylab("Cigarettes Per Day") + 
   ggtitle("Baseline Balance Plot")

# Note that the medians (center lines), and
# inter-quartile ranges (IQRs) are about the
# same across the three conditions for both
# BDI and cig.

# BDI: like the medians, the group means are close.
by(data = dat2$bdi_base,
   INDICES = dat2$ConditionF,
   FUN = mean, na.rm = TRUE)
# BDI: group variances are somewhat close.
by(data = dat2$bdi_base,
   INDICES = dat2$ConditionF,
   FUN = var, na.rm = TRUE)

# cig: like the medians, the group means are close.
by(data = dat2$cig_base,
   INDICES = dat2$ConditionF,
   FUN = mean, na.rm = TRUE)
# cig: group variances are very close.
by(data = dat2$cig_base,
   INDICES = dat2$ConditionF,
   FUN = var, na.rm = TRUE)

# In all, good evidence for balance across the 
# groups at baseline, which suggests randomization
# was successful.

#######
# 4. Explore the treatment effect on depression.
#######
# Now switch our attention to the BDI outcome 
# data at the EOT.

# Means by group (base R)
boxplot(bdi_EOT ~ ConditionF, 
        data = dat2,
        xlab = "Experimental Condition",
        ylab = "Beck Depression Score",
        main = "Depression at End of Trial")

# The means have diverged with the two treatment groups
# having less depression than the WL control group.
by(data = dat2$bdi_EOT,
   INDICES = dat2$ConditionF,
   FUN = mean, na.rm = TRUE)

# With tidyverse (no piping)
summarize(group_by(dat2, ConditionF), mean(bdi_EOT, na.rm = TRUE))
# With tidyverse (with piping)
dat2 %>% group_by(ConditionF) %>% summarize(mean(bdi_EOT, na.rm = TRUE))

# Group SDs have diverged as well. If this difference 
# in group variances is real at EOT, it violates the
# constant variance assumption. In particular, we know 
# that if a larger group variance is associated with 
# a group with smaller sample size, the p-values for
# ANOVA tend to be smaller than they should be. That is,
# the null hypothesis will be rejected more often
# than 5% of the time. If, on the other hand, a larger
# group variance is associated with a group with a large
# sample size, the p-values will be conservative; that is,
# they will be larger than they should be.
table(dat2$ConditionF)
by(data = dat2$bdi_EOT,
   INDICES = dat2$ConditionF,
   FUN = var, na.rm = TRUE)

# With tidyverse (dplyr)
dat2 %>% group_by(ConditionF) %>% summarize(n(), var(bdi_EOT, na.rm = TRUE))

# Here, the group with the smaller sample size (WL) is
# associated with the largest variance.
table(dat2$ConditionF)

# Thus, we should be suspicious about interpreting the
# p-value from ANOVA at face value.

# There is a statistical test for the constant variance
# hypothesis called Levene's test. It's available in
# package car. Download the package from the plotting pane
# packages tab or install using the following code:
# install.packages("car")
library(car)

# The null hypothesis for the test is that the group
# variances are identical. The alternative hypothesis
# is that at least one of the group variances differs.
leveneTest(y = dat2$bdi_EOT, 
           group = dat2$ConditionF)

# Another test of the same null hypothesis that is 
# typically more powerful is called Bartlett's test.
bartlett.test(x = dat2$bdi_EOT,
              g = dat2$ConditionF)

# Indeed, Bartlett's test rejects the null hypothesis
# of equal variances here. Thus, we should be cautious
# about interpreting the results of one-way ANOVA.
# Nevertheless, we will continue because, after all, 
# the goal of today is to run one-way ANOVA.

#######
# 5. Test for a treatment effect
#######
# One-way between-subjects ANOVA is called for to test
# the null hypothesis that the treatment had no effect.
# Today we will cover fitting linear regression models
# and the general linear F test.

# The full model for one-way ANOVA:
# The option for default treatment of contrasts has
# two specifications: the first for unordered factors
# and the second for ordered factors. We will primarily
# be interested in the first (for unordered factors).
# The name for dummy coding is "contr.treatment"
options(contrasts = c("contr.treatment", "contr.poly"))
lmF_dummy <- lm(formula = bdi_EOT ~ ConditionF,
                data = dat2)
summary(lmF_dummy)

# Effect coding
options(contrasts = c("contr.sum", "contr.poly"))
lmF_effect <- lm(formula = bdi_EOT ~ ConditionF,
                 data = dat2)
summary(lmF_effect)

# The reduced model is an intercept-only model with
# no predictor for group (so factor coding doesn't matter).
lmR <- lm(formula = bdi_EOT ~ 1,
          data = dat2)
summary(lmR)

# The anova() function runs the incremental F test for
# nested regression models.
anova(lmR, lmF_dummy)
anova(lmR, lmF_effect)

# The formula for the incremental (i.e., general linear) 
# F test statistic:
# F = [(SSR - SSF)/(dfR - dfF)] / [SSF/dfF]

# Let's calculate these quantities "by hand" for better
# understanding.

# SSR are the residuals from the restricted model.
set.seed(1234)
plot(x = jitter(dat2$Condition), y = dat2$bdi_EOT,
     xlab = "Condition", ylab = "BDI at EOT")
# Predicted values from the restricted model are all 
# based on the intercept.
summary(lmR)
lmR$fitted.values # all 8.4
abline(a = 8.4, b = 0)
(SSR <- sum(lmR$residuals^2)) # 14826.8

# How many total participants?
table(dat2$ConditionF, useNA = "always")
sum(table(dat2$ConditionF, useNA = "always"))

# How many participants with missing data on bdi_EOT?
table(dat2$bdi_EOT, useNA = "always") # 55
dfR <- 275 - 55 - 1 # 55 missing and 1 parameter for intercept

# SSF are residuals from full model.
set.seed(1234)
plot(x = jitter(dat2$Condition), y = dat2$bdi_EOT,
     xlab = "Condition", ylab = "BDI at EOT")
summary(lmF_dummy)
segments(x0 = .75, y0 = 7.3, x1 = 1.25, y1 = 7.3, col = 2, lwd = 2)
segments(x0 = 1.75, y0 = 7.3 + .7, x1 = 2.25, y1 = 7.3 + .7, col = 3, lwd = 2)
segments(x0 = 2.75, y0 = 7.3 + 3.7, x1 = 3.25, y1 = 7.3 + 3.7, col = 4, lwd = 2)
(SSF <- sum(lmF_dummy$residuals^2))
dfF <- 275 - 55 - 3

# Calculate F statistic and p-value
(F_stat <- ((SSR - SSF)/(dfR - dfF)) / (SSF/dfF))
(p_val <- pf(q = F_stat, df1 = dfR - dfF, df2 = dfF, lower.tail = FALSE))

#######
# 6. Your tasks
#######
# 1. Check baseline balance on the Age variable using the methods
#    we used here (i.e., boxplot, sample means, and sample variances)
#    and state what the evidence says about the success of randomization
#    in providing balance on this variable.

# 2. In the lab today we looked to see if there is evidence of a treatment 
#    effect on depression. Your task is to test for a treatment effect 
#    on smoking quantity at end of trial.

