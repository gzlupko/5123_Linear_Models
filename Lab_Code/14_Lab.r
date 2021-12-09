library(AMCP)
library(tidyverse) 

# The one-way within-subjects design
data(C11T5)
dat1 <- C11T5

# Note the data are in wide format. Good for estimating
# the var/cov matrix.
round(var(dat1), 0)
round(cor(dat1), 2) 


# add id column with mutate() 
dat1 <- dat1 %>%
  mutate(id = row_number()) %>%
  relocate(id)


# Convert to long for plotting
# Convert to long format for an interaction plot
dat1_l <- dat1 %>%
  pivot_longer(cols = -id, 
               names_to = "Months", 
               names_prefix = "Months", 
               values_to = "Score")  

# Convert Months to a factor
dat1_l$Months <- factor(dat1_l$Months)

# Plot
boxplot(Score ~ Months, data = dat1_l)

dat1_l %>%
  ggplot(aes(x = Months, y = Score)) + geom_boxplot()

# Group means
tapply(X = dat1_l$Score, INDEX = dat1_l$Months, FUN = mean)

dat1_l %>% 
  group_by(Months) %>%
  summarize(mean(Score)) 


# Fit the multivariate model
# here we only have a one factor design
# we use the wide format data here; we put in long format to graph them but not needed in long here for lm() 
mvm1 <- lm(as.matrix(dat1[, -1]) ~ 1, data = dat1) 
library(car)
# capital 'A' Anova() function in car library is used for... 
wsd1 <- Anova(mod = mvm1, 
              idata = data.frame(Months = factor(c(30,36,42,48))), 
              idesign = ~ Months, 
              type = 3)
summary(wsd1, multivariate = FALSE) # multivariate = F gets us epsilon corrections
# sig and small p-value for Mauchly test suggests that spherecity is not satisified 
# so therefore we should rely on the Greenhouse-Geisser correction; and the results suggest no longer sig


# Follow-up with a contrast test for a linear trend
psi1 <- c(-3/4, -1/4, 1/4, 3/4)

# Apply the contrast to each row of repeated-measures data
(psi_vals <- as.matrix(dat1[ ,-1]) %*% psi1) 
(psi_bar <- mean(psi_vals))
(s_psi_bar <- sd(psi_vals)/sqrt(12))
(t_psi_test <- psi_bar / s_psi_bar)
2 * pt(q = abs(t_psi_test), df = 11, lower.tail = FALSE)

#####################
# Split-plot design #
#####################
# The split-plot design has one factor that is within-subjects
# and one that is between-subject; we have subjects over (within) and age groups (between)
data(C12T15)
dat_old <- C12T15
dat_old
# we are going to combine the data frames as the authors have yet to 
data(C12T7)
dat_young <- C12T7
dat_young

# Put the data for young and old together by
# stacking them vertically with rbind()
dat3 <- rbind(dat_young, dat_old)
dat3

# Add a between-subjects factor for age category
dat3 <- cbind("Age" = rep(c("Y", "O"), each = 10), dat3)
dat3$Age <- factor(dat3$Age)
dat3

# Add an id variable
dat3 <- dat3 %>% mutate(id = row_number()) %>% relocate(id)

write_csv(x = dat3, path = "/Users/gianzlupko/Desktop/dat3.csv")  

# Convert to long format for an interaction plot
dat3_l <- dat3 %>% pivot_longer(cols = -c(id, Age),
                                names_to = "Angle",
                                names_prefix = "Angle",
                                values_to = "Reaction")
dat3_l


# Convert Noise, Angle, and id to factors
dat3_l$Age <- factor(dat3_l$Age)
dat3_l$Angle <- factor(dat3_l$Angle)
dat3_l$id <- factor(dat3_l$id)

# Visualize the data
boxplot(dat3_l$Reaction ~ dat3_l$Angle * dat3_l$Age,
        xlab = "Angle.Age", ylab = "Reaction Time")
boxplot(dat3_l$Reaction ~ dat3_l$Age * dat3_l$Angle,
        xlab = "Age.Angle", ylab = "Reaction Time")
dat3_l %>% ggplot(aes(y = Reaction, x = Angle, fill = Age)) + 
  geom_boxplot()
dat3_l %>% ggplot(aes(y = Reaction, x = Age, fill = Angle)) + 
  geom_boxplot()

# Compute sample means by design cell
tapply(X = dat3_l$Reaction, INDEX = list(dat3_l$Age, dat3_l$Angle), FUN = mean)

interaction.plot(x.factor = dat3_l$Angle,
                 trace.factor = dat3_l$Age,
                 response = dat3_l$Reaction, 
                 trace.label = "Age",
                 xlab = "Angle", ylab = "Mean RT",
                 col = c(2,4), lwd = 3)

# Run the multivariate regression model, but now regress
# the repeated-measures outcomes on the B-S factor, Age.
# turn the DVs into a matrix so that it can be used in lm() 
mvm10 <- lm(as.matrix(dat3[,3:5]) ~ Age, data = dat3) 
library(car)
wsd10 <- Anova(mod = mvm10, 
              idata = data.frame(Angle = factor(c(0,4,8))), 
              idesign = ~ Angle, 
              type = 3)
summary(wsd10, multivariate = FALSE) # multivariate = F gets us epsilon corrections

# There is a significant interaction here. Let's follow
# up with simple effects. We can look at the simple effects
# of Age at each of the three levels of Angle. These will
# be one-way between-subjects designs. We can also look
# at the simple effects of Angle at both levels of Age. These
# will be one-way W-S designs.

# Within-subjects effects at a fixed level of the between-
# subjects factor (i.e., testing the effect of Angle at
# the two-levels of Age)

# Testing the Angle effect at Age = young
mvm11 <- lm(as.matrix(dat3[1:10, 3:5]) ~ 1) 
wsd11 <- Anova(mod = mvm11, 
               idata = data.frame(Angle = factor(c(0,4,8))), 
               idesign = ~ Angle, 
               type = 3)
summary(wsd11, multivariate = FALSE) # multivariate = F gets us epsilon corrections

# Testing the Angle effect at Age = old
mvm12 <- lm(as.matrix(dat3[11:20, 3:5]) ~ 1) 
wsd12 <- Anova(mod = mvm12, 
               idata = data.frame(Angle = factor(c(0,4,8))), 
               idesign = ~ Angle, 
               type = 3)
summary(wsd12, multivariate = FALSE) # multivariate = F gets us epsilon corrections

# Follow up with contrasts as needed.
# Suppose I want to test avg of angle 0 and angle 4 means vs 
# angle 8 mean (at the Age = young or old level of age).
psi_test <- c(-1/2, -1/2, 1) 
(psi_test_10 <- as.matrix(dat3[11:20, 3:5]) %*% psi_test) # old group
(t_psi_test <- mean(psi_test_10) / (sd(psi_test_10)/ sqrt(10)))
2 * pt(q = abs(t_psi_test), df = 9, lower.tail = FALSE)

# also follow up with young level... 
# Could also do pairwise comparisons or whatever...

psi2 <- c(-1, 1, 0)
psi3 <- c(-1, 0, 1) 
psi4 

# Holm-Bonferroni correction 


# Between-subjects effects at each level of the
# W-S factor (i.e., testing the Age effect at 
# each of three levels of Angle)

# Testing the Age effect at Angle = 0
lm13 <- lm(Angle0 ~ Age, data = dat3)
Anova(lm13, type = 3)

# Testing the Age effect at Angle = 4
lm14 <- lm(Angle4 ~ Age, data = dat3)
Anova(lm14, type = 3)

# Testing the Age effect at Angle = 8
lm15 <- lm(Angle8 ~ Age, data = dat3)
Anova(lm15, type = 3)

# Follow up with contrasts as needed.
# Suppose I'm looking for pairwise comparison of levels
# age at the Angle = 0 level of Angle. Here, obviously, 
# the pairwise comparisons will be redundant with the 
# omnibus test because there are only two levels of Age.


# Interest may center on testing an interaction contrast.
# For example, is there a difference across ages in the 
# average of angles 0 and 4 to angle 8 comparisons?
psi4 <- c(1/2, 1/2, -1, -1/2, -1/2, 1)
(psi4_10 <- as.matrix(cbind(dat3[1:10, 2:4], dat3[11:20, 2:4])) %*% psi4)
(psi4_hat <- mean(psi4_10))
SS_psi4 <- (psi4_hat^2) / ((1/2)^2/10 + (1/2)^2/10 + (-1)^2/10 + (-1/2)^2/10 + (-1/2)^2/10 + (1)^2/10)

F_psi4 <- SS_psi4 / (54420 / 36) # Denominator is MS_{B x A | S} (See MDK, p. 701-703 for discussion)
pf(q = F_psi4, df1 = 1, df2 = 36, lower.tail = FALSE)



psi6 <- c(1/2, 1/2, -1, -1/2, -1/2, 1, 0, 0, 0)
psi7 <- c(1/2, 1/2, -1, 0, 0, 0, -1/2, -1/2, 1)
psi8 <- c(0, 0, 0, 1/2, 1/2, -1, -1/2, -1/2, 1)

#############################################
# TWO-WAY COMPLETELY WITHIN-SUBJECTS DESIGN #
#############################################
# The two-way within-subjects design (both 
# factors are within-subjects factors).
library(tidyverse)
library(AMCP)
data(C12T1)
dat2 <- C12T1
dat2
round(cor(dat2[,1:3]), 2)
round(cor(dat2[,4:6]), 2)
# Here angle is a W-S factor with three levels:
# 0 deg, 4 deg, and 8 deg. Noise is also a W-S
# factor that has two levels: absent and present.
# There were 10 participants in the study.

# Convert from wide to long.
# First add a separator to column names
names(dat2) <- c("Absent_0", "Absent_4", "Absent_8", 
                 "Present_0", "Present_4", "Present_8")
# Then add an id column
dat2 <- dat2 %>% mutate(id = row_number()) %>% relocate(id)
dat2_l <- 
  dat2 %>% pivot_longer(cols = -id, # ?tidyr_tidy_select
                        names_to = c("Noise", "Angle"), # names of the new factors
                        names_sep = "_", 
                        values_to = "Reaction") # name of the stacked variable
dat2_l
dat2_l <- dat2_l %>% arrange(id, Noise, Angle)
print(dat2_l, n = 30)

# Convert Noise, Angle, and id to factors
dat2_l$Noise <- factor(dat2_l$Noise)
dat2_l$Angle <- factor(dat2_l$Angle)
dat2_l$id <- factor(dat2_l$id)

# Visualize the data
boxplot(dat2_l$Reaction ~ dat2_l$Angle * dat2_l$Noise,
        xlab = "Angle.Noise", ylab = "Reaction Time")
boxplot(dat2_l$Reaction ~ dat2_l$Noise * dat2_l$Angle,
        xlab = "Noise.Angle", ylab = "Reaction Time")
dat2_l %>% ggplot(aes(y = Reaction, x = Angle, fill = Noise)) + 
  geom_boxplot()
dat2_l %>% ggplot(aes(y = Reaction, x = Noise, fill = Angle)) + 
  geom_boxplot()


# Compute sample means by design cell
tapply(X = dat2_l$Reaction, INDEX = list(dat2_l$Noise, dat2_l$Angle), FUN = mean)

interaction.plot(x.factor = dat2_l$Angle,
                 trace.factor = dat2_l$Noise,
                 response = dat2_l$Reaction, 
                 trace.label = "Noise",
                 xlab = "Angle", ylab = "Mean RT",
                 col = c(2,4), lwd = 3)

# Set contrasts up for deviation coding
options(contrasts = c("contr.sum", "contr.poly"))

# Fit the multivariate model (back to dat2)
mvm2 <- lm(as.matrix(dat2[,2:7]) ~ 1)

# Make the data frame that will be passed along as 
# idata in the Anova() specification. Make sure that 
# the factor levels line up with the order in dat2.
design_df <- data.frame(Noise = factor(c(0,0,0,1,1,1)), 
                        Angle = factor(c(0,4,8,0,4,8)))
design_df

library(car)
wsd2 <- Anova(mod = mvm2, 
              idata = design_df,
              idesign = ~ Angle * Noise, 
              type = 3)
summary(wsd2, multivariate = FALSE)
# The multivariate = FALSE option is used to suppress
# MANOVA-style output and produce epsilon corrections.

# The interaction is significant so we will follow up with
# simple effects. We will analyze the effect of Noise at
# all three levels of Angle and the effect of Angle at both
# levels of Noise.

# The effect of Noise at Angle = 0.
mvm3 <- lm(as.matrix(dat2[,c(1,4)]) ~ 1)
wsd3 <- Anova(mod = mvm3, 
              idata = data.frame(Noise = factor(c("Absent", "Present"))),
              idesign = ~ Noise, 
              type = 3)
summary(wsd3, multivariate = FALSE) # No epsilon corrections b/c only 2 levels

# The effect of Noise at Angle = 4.
mvm4 <- lm(as.matrix(dat2[,c(2,5)]) ~ 1)
wsd4 <- Anova(mod = mvm4, 
              idata = data.frame(Noise = factor(c("Absent", "Present"))),
              idesign = ~ Noise, 
              type = 3)
summary(wsd4, multivariate = FALSE) # No epsilon corrections b/c only 2 levels

# The effect of Noise at Angle = 8.
mvm5 <- lm(as.matrix(dat2[,c(3,6)]) ~ 1)
wsd5 <- Anova(mod = mvm5, 
              idata = data.frame(Noise = factor(c("Absent", "Present"))),
              idesign = ~ Noise, 
              type = 3)
summary(wsd5, multivariate = FALSE) # No epsilon corrections b/c only 2 levels

# No need to do follow up tests here because each omnibus test 
# has only two levels. In any case, the paired t test would look
# like this for Absent vs Present at Angle = 0 deg.:
t.test(x = dat2[,1], y = dat2[,4], alternative = "two.sided", paired = TRUE)

# The effect of Angle at Noise = Absent.
mvm6 <- lm(as.matrix(dat2[,1:3]) ~ 1)
wsd6 <- Anova(mod = mvm6, 
              idata = data.frame(Angle = factor(c(0, 4, 8))),
              idesign = ~ Angle, 
              type = 3)
summary(wsd6, multivariate = FALSE)

# The effect of Angle at Noise = Present.
mvm7 <- lm(as.matrix(dat2[,4:6]) ~ 1)
wsd7 <- Anova(mod = mvm7, 
              idata = data.frame(Angle = factor(c(0, 4, 8))),
              idesign = ~ Angle, 
              type = 3)
summary(wsd7, multivariate = FALSE)

# Could follow up with pairwise t-tests or contrasts as in 
# the one-way W-S design.

# Follow up with angle = 0 vs avg of other angles when Noise = Present.
psi_s <- c(-1, 1/2, 1/2)
(psi_s_10 <- as.matrix(dat2[,4:6]) %*% psi_s)
(t_psi_s <- mean(psi_s_10) / (sd(psi_s_10) / sqrt(10)))
2 * pt(q = abs(t_psi_s), df = 9, lower.tail = FALSE)

# Another way to follow-up the significant two-way interaction
# is with interaction contrasts. For example, suppose we wish to 
# know if the difference in response times when Angle = 0 vs Angle = 8
# differs across noise conditions.

# To get the sum of squares for an interaction contrast, we
# see the data as a one-way design and fit the appropriate contrast
# coefficients.
psi3 <- c(1, 0, -1, -1, 0, 1) 
(psi3_10 <- as.matrix(dat2) %*% psi3)
(t_psi3 <- mean(psi3_10) / (sd(psi3_10) / sqrt(10)))
2 * pt(q = abs(t_psi3), df = 9, lower.tail = FALSE)

# What about if the interaction was not significant and we had
# wanted to analyze main effects? In that case, we could proceed 
# by collapsing the design by averaging over the relevant factor.

# Let's look at the main effect of angle, averaging over noise.
# First, average over noise to create a new data set.
dat2
dat2_avg_over_noise <- (dat2[,1:3] + dat2[,4:6]) / 2
dat2_avg_over_noise

# Then, follow up as if it were a one-way W-S design
# with contrasts, as desired. Note that this F and p-value
# agrees with the one from the overall analysis above.
mvm9 <- lm(as.matrix(dat2_avg_over_noise) ~ 1)
wsd9 <- Anova(mod = mvm9, 
              idata = data.frame(Angle = factor(c(0, 4, 8))),
              idesign = ~ Angle, 
              type = 3)
summary(wsd9, multivariate = FALSE)

