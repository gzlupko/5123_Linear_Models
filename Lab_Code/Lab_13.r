# Here we will work through Exercise 17 in MDK (p. 660)

# A developmental psychologist is interested in the role 
# of the sound of a mother’s heartbeat in the growth of 
# newborn babies. Fourteen babies were placed in a nursery 
# where they were constantly exposed to a rhythmic heartbeat 
# sound piped in over the PA system. Infants were weighed 
# at the same time of day for 4 consecutive days, yielding 
# the following data (weight is measured in ounces):
library(AMCP)
data("C11E17")
dat <- C11E17
dat

# These data are in wide format. As a first step, we need to
# practice going back and forth from wide to long format.
library(tidyverse)
# Add an id number
(dat_long <- dat %>% mutate(id = row_number()) %>% relocate(id))
# Transform to long format
dat_long <- 
  dat_long %>% pivot_longer(cols = Day1:Day4, # ?tidyr_tidy_select
                            names_to = "Day", # name of the new factor
                            names_prefix = "Day", # characters to drop from levels
                            values_to = "Weight") # name of the stacked variable
dat_long
print(dat_long, n = Inf)

# Now transform back to wide format.
dat_wide <- dat_long %>% pivot_wider(id_cols = id,
                                     names_from = "Day",
                                     names_prefix = "Day",
                                     values_from = "Weight")
dat_wide

# It's useful to have repeated-measures (within-subjects)
# data in wide format for calculation of var/cov and correlation
# matrices. 
dat_wide[, 2:5]
var(dat_wide[, 2:5]) # 
round(var(dat_wide[, 2:5]), 1)
cor(dat_wide[, 2:5])
round(cor(dat_wide[, 2:5]), 2) #shows auto-regressive pattern 

########
# Explore the data graphically and with summary statistics
########
dat_long %>% ggplot(mapping = aes(x = Day, y = Weight)) + 
  geom_boxplot()
dat_long %>% group_by(Day) %>% summarize(mean(Weight), var(Weight))
# To print by decimal places instead of significant digits
print.data.frame(dat_long %>% group_by(Day) %>% summarize(mean(Weight), var(Weight)))

########
# Is the assumption of sphericity tenable here?
########
round(var(dat_wide[, 2:5]), 1)
round(cor(dat_wide[, 2:5]), 2)
mlm1 <- lm(as.matrix(dat_wide[,2:5]) ~ 1) # 4 columns regressed on intercept  
mauchly.test(object = mlm1, X = ~1) # Not tenable; small p-value, rejected null hypothesis that sphereicity holds
# if sphereicity doesn't hold we need greenhouse correction 


# How about normality? Check at each time point.
dat_long %>% ggplot(mapping = aes(y = Weight)) + 
  geom_histogram(bins = 6) + 
  facet_wrap(~Day) + 
  coord_flip() + ggtitle(label = "Weight by Day")

########
# 17(a)
########
# Test the omnibus null hypothesis that the population mean 
# weight is the same for all 4 days, using the unadjusted 
# mixed-model approach (i.e., with subjects as a random factor,
# day as a fixed factor, and no epsilon correction. 3 different test examples of same omnibus test of day below
dat_long$Day <- factor(dat_long$Day) # convert to factor
dat_long$id <- factor(dat_long$id)
aov0 <- aov(Weight ~ Day*id, data = dat_long)
aov0
pf(q = ((22.48/3) / (159.27/39)), df1 = 3, df2 = 39, lower.tail = FALSE)

# Can do this with epsilon corrections in package car; we technically don't need above as our results did not come out as significant
library(car)
# First, fit the multivariate regression model (we can use mlm1 
# from above)
mlm1 <- lm(as.matrix(dat_wide[,2:5]) ~ 1)
# Second, create a dataframe called idata that has factors in 
# it with the right level combinations. Here, we only have a 
# single fixed factor called "Day" that has four levels.
idata <- data.frame(Day = factor(1:4))
# Third, pass this information to the Anova() function along
# with the design, which here is only one-way on Day.
car1 <- Anova(mlm1, idata = idata, idesign = ~ Day)
# Fourth, take the summary of the output to see results with
# and without epsilon corrections.
summary(car1)

# Can do this in a mixed-effects framework (with no epsilon corrections)
library(lme4)
library(lmerTest)
lmer1 <- lmer(Weight ~ Day + (1|id), data = dat_long) #random intercept model; we fit a random intercept model based on subject id 
anova(lmer1)

# Save data as .csv for SPSS
#write_csv(x = dat_wide, file = "/Users/bryankeller/Desktop/dat_wide.csv")
write_csv(x = dat_wide, file = "/Users/gianzlupko/Desktop/dat_wide.csv")
# Replicate analysis in SPSS
