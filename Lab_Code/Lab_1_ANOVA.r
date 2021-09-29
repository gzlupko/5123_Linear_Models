# SETUP
# Load the package
library(tidyverse)

# Import the data
library(readr)
dat <- read_csv("~/Desktop/Class/Fall 2021/Linear Models & Experimental Design/R/smokingRCT.csv")

# Isolate relevant variables
dat2 <- dat %>% 
  select(Condition, Age, Sex, 
         cigarettes_day_baseline, bdi_baseline, 
         bdi_EOT, Cigarettes_day_EOT)

# Rename some variables
names(dat2)[4] <- "cig_base"
names(dat2)[5] <- "bdi_base"
names(dat2)[7] <- "cig_EOT"
names(dat2)

# Make categorical variables into factors
dat2$ConditionF <- factor(dat2$Condition,
                          levels = c(1,2,3),
                          labels = c("SCB", "BA", "WL"))

dat2$SexF <- factor(dat2$Sex,
                    levels = c(1,2),
                    labels = c("Male", "Female"))

# Task 1: EXAMINE BALANCE OF AGE
# Using boxplot
boxplot(Age ~ ConditionF,
        data = dat2,
        xlab = "Experimental Condition",
        ylab = "Age",
        main = "Baseline Balance Plot")

### Medians and quartiles appear similar across groups

# Sample means & variances
by(data = dat2$Age,
   INDICES = dat2$ConditionF,
   FUN = mean, na.rm = TRUE)

by(data = dat2$Age,
   INDICES = dat2$ConditionF,
   FUN = var, na.rm = TRUE)

## Means and variances of groups also appear similar
## "Good evidence for balance across the groups at baseline, suggesting
# randomization was successful" (original)





# Task 2: TEST FOR AN EFFECT ON SMOKING QUANTITY AT EOT
# 2a: EXAMINE ASSUMPTIONS

# Look at daily cigarettes at EOT by group
boxplot(cig_EOT ~ ConditionF,
        data = dat2,
        xlab = "Experimental Condition",
        ylab = "Daily Cigarettes",
        main = "Daily Cigarettes at End of Trial")

by(data = dat2$cig_EOT,
   INDICES = dat2$ConditionF,
   FUN = mean, na.rm = TRUE)

by(data = dat2$cig_EOT,
   INDICES = dat2$ConditionF,
   FUN = var, na.rm = TRUE)

### Both treatment groups have much lower medians and means
### Group variances differ a lot

table(dat2$ConditionF)

### Largest group has smallest variance and vice versa
### This violates the assumption of constant variance
### Especially concerning because the variances are negatively related to n
### P-value is likely to be smaller than it should be
### (i.e., increased Type 1 error)

# Try Levene's test for constant variance

library(car)

leveneTest(y = dat2$cig_EOT,
           group = dat2$ConditionF)

### Reject null hypothesis of equal group variances

# Try Bartlett's test too, which is more powerful

bartlett.test(x = dat2$cig_EOT,
              g = dat2$ConditionF)

### Again reject the null; group variances are not equal
### Interpret ANOVA results with caution


# 2b: RUN THE ANOVA

# Dummy coded full model
options(contrasts = c("contr.treatment", "contr.poly"))
lmF_dummy <- lm(formula = cig_EOT ~ ConditionF,
                data = dat2)
summary(lmF_dummy)

# Effect coded full model
options(contrasts = c("contr.sum", "contr.poly"))
lmF_effect <- lm(formula = cig_EOT ~ ConditionF,
                 data = dat2)
summary(lmF_effect)

# Reduced model (requires no factor coding)
lmR <- lm(formula = cig_EOT ~ 1,
          data = dat2)
summary(lmR)


# Compare models (incremental F test)
anova(lmR, lmF_dummy)
anova(lmR, lmF_effect)



# The formula for the incremental (i.e., general linear) 
# F test statistic:
# F = [(SSR - SSF)/(dfR - dfF)] / [SSF/dfF]






# Word processing doc of some sort
# Paragraph form response for each question
# Include figures as well
# Include an ANOVA table!
# One line for "Condition"/"Treatment" and one for "Error"
# Note: RSS(1) is not ER
# Must subtract RSS(2) from RSS(1) to get ER (or look under "Sum of Sq")
# Similarly, df(1) should be 2
# Should also include mean squares in table
# Column between df and F
# They are num and denom of F-stat - divide each SS by its rows df


#######
# 1. Check baseline balance on the Age variable using the methods
#    we used here (i.e., boxplot, sample means, and sample variances)
#    and state what the evidence says about the success of randomization
#    in providing balance on this variable.

# 2. In the lab today we looked to see if there is evidence of a treatment 
#    effect on depression. Your task is to test for a treatment effect 
#    on smoking quantity at end of trial.
