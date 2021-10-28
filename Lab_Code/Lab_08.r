#######
# 1. Import
#######
# The paper and data are available here:
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0214252

# Import the data. This can be done via the "Files" tab
# in your plotting window pane in RStudio. Or, it can be
# one manually as follows, though, on your machine you 
# would need to enter the correct file path.
dat <- read.csv(file = paste0("/Users/bryankeller/Dropbox",
                              "/Teaching/Linear Models and Experimental Design",
                              "/2021 SPRING/00 Syllabus/smokingRCT.csv"))

library(tidyverse) 
dat <- read_csv("smokingRCT.csv") 
# dat <- read.csv(file = file.choose())
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
# -Working status
# -Education level
# -Marital status

# NEW VARIABLES FOR TWO-WAY ANOVA
# -Working status (Working_status)
# -Education (Education)
# -Marital Status (Marital_status)

# Let's create a new data set that only has the variables 
# we are interested in working with
dat2 <- subset(x = dat, 
               select = c("Condition", "Age", "Sex", 
                          "cigarettes_day_baseline",
                          "Number_years_smoking", "bdi_baseline", 
                          "bdi_EOT", "Cigarettes_day_EOT",
                          "Working_status", "Education",
                          "Marital_status"))
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

# Examine the new baseline covariates we just brought in.
table(dat2$Working_status)
table(dat2$Education)
table(dat2$Marital_status)

# Consult the SPSS (available at PLOSONE link above) file for
# category labels. Find the following:
# For the working status variable, 
#  1 = working; 2 = unemployed; 3 = sick leave; 4 = retirement; 5 = other
# Let's create an indicator 0/1 variable where 1 is working and 0 is 
# anything else.
dat2$working <- ifelse(dat2$Working_status == 1, 1, 0)
(dat2$Working_status == 1)*1

# For the education variable, 
#  1 = < HS diploma; 2 = HS or GED; 3 = College/technical; 4 = no education
# Let's create a 0/1 indicator where 1 = college and 0 otherwise.
dat2$college <- ifelse(dat2$Education == 3, 1, 0)

# For the marital status variable,
#  1 = single; 2 = married or live with partner; 3 = divorced/separated/widowed
# Let's create a 0/1 indicator where 1 = married and 0 = otherwise
dat2$married <- ifelse(dat2$Marital_status == 2, 1, 0)

str(dat2)

# Create factor versions of categorical variables
table(dat2$Condition)
dat2$ConditionF <- factor(dat2$Condition, 
                          levels = c(1,2,3),
                          labels = c("SCBSCT", "SCBSCT-BA", "WL"))
table(dat2$ConditionF)
dat2$ConditionF <- relevel(dat2$ConditionF, ref = "WL")
table(dat2$ConditionF) # Now WL is first (reference)

table(dat2$Sex)
dat2$SexF <- factor(dat2$Sex,
                    levels = c(1,2),
                    labels = c("Male", "Female"))
table(dat2$SexF)

table(dat2$working)
dat2$workingF <- factor(dat2$working,
                    levels = c(0,1),
                    labels = c("not working", "working"))
table(dat2$workingF)

table(dat2$college)
dat2$collegeF <- factor(dat2$college,
                        levels = c(0,1),
                        labels = c("no college", "college"))
table(dat2$collegeF)

table(dat2$married)
dat2$marriedF <- factor(dat2$married,
                        levels = c(0,1),
                        labels = c("not married", "married"))
table(dat2$marriedF)
table(dat2$marriedF, dat2$ConditionF)

# Examine the first 6 rows after cleaning
head(dat2)
# Examine rows for WL condition
dat2[which(dat2$ConditionF == "WL"),]

library(tidyverse)
# make dat2 a tibble
# tibbles are a tidyverse-type df that is useful when printing out ddf
dat2 <- as_tibble(dat2)
dat2


#######
# 3. Two-way analysis of variance with work status
#######
# Working with the Beck depression inventory outcome, use Type
# 3 sums of squares to test:
# 1. Is there any evidence of a differential effect of treatment 
#    on BDI at EOT based on working status?
# 2. If so, follow up with simple effects analyses.
# 3. If not, follow up with main effects analyses.

# We will skip checking assumptions here (see last lab
# for a detailed look at this).

# Set contrast option to deviation coding for unordered
# factors.
options(contrasts = c("contr.sum", "contr.poly"))

# Fit the full model.
# the asterisk automatically creates the full factorial solution
# which gives use the main effects in addition to interaction
lm1 <- lm(bdi_EOT ~ ConditionF*workingF, data = dat2)

lm1 %>%
  summary() 
# Run the two ANOVA with type 3 SS via Anova() in pakcage car.
library(car)
Anova(lm1, type = 3)


# example for the condition of sum of squares 
dat3 <- na.omit(dat2) 
lmF3 <- lm(bdi_EOT ~ ConditionF + workingF + ConditionF:workingF, data = dat3) 

design_mat3 <-model.matrix(lmF3, ~ ConditionF:workingF) 

design_mat3[, -(2:3)] 



lmR3 <- lm(dat3$bdi_EOT ~ design_mat3[ ,-(2:3)], dat = dat3) 


anova(lmR3, lmF3) 
# The test of interaction is not significant 
# (F(2,14) = 2.37; p = .095; eta^2_partial = 304.8/(13758.9 + 304.8) = 0.022). 
# The test of main effect of working status is significant
# (F(1, 214) = 8.46; p = 0.004; eta^2_partial = 544.2/(13758.9 + 544.2) = 0.038). 
# and the test of main effect of condition is significant 
# (F(2, 214) = 5.22; p = 0.006; eta^2_partial = 671.6/(13758.9 + 671.6) = 0.047).

# Create plot of means and follow up on main effects.
# uses emmmip() to create an emmeans interaction plot 
library(emmeans)
emm1 <- emmeans(lm1, ~ ConditionF:workingF)
emmip(object = emm1, formula = workingF ~ ConditionF, CIs = TRUE)
emmip(object = emm1, formula = ConditionF ~ workingF, CIs = TRUE)

###########
# 4. Follow up with main effects contrasts
###########
# First check for condition differences, averaging over
# the levels of working status.
emm2 <- emmeans(object = lm1, 
                specs = ~ ConditionF,
                adjust = "none")
# Note the warning. 
pairs(emm2, adjust = "Tukey")

# Next, check for differences by working status, averaging
# over the levels of condition.
emm3 <- emmeans(object = lm1, 
                specs = ~ workingF,
                adjust = "none")
pairs(emm3, adjust = "none")

# The mean depression score of participants in the waitlist
# control group is 4.32 points higher than the mean depression 
# score in either the CBT group (t(214) = 2.85; p = .013) and 
# 3.71 points higher than for those in the BA group 
# (t(214) = 2.52; p = 0.033). Furthermore, the mean depression 
# score for those working was 3.28 points lower than for those
# not working (t(214) = 2.831; p = 0.005).

#######
# 5. What if the interaction had been significant?
#######
# Suppose the interaction had been significant or that we
# decided to analyze simple effects based on the graphical
# evidence. There are two ways to analyze the data:
# 1. Simple effects of condition at each level of working stats.
# 2. Simple effects of working status at each level of condition.
# We will do both.

emmip(object = emm1, formula = ConditionF ~ workingF, CIs = TRUE)

# 1. Analyze simple effects of condition at 2 levels of working status.
#    There are two one-way omnibus tests of condition (three levels).
# conditions on working condition to exam levels of treatment by the working status 
(jt1 <- joint_tests(object = emm1, by = "workingF"))

# Multiple p-values by 2 to make a Bonferroni correction.
jt1$p.value * 2 

# Only the first joint test is significant. Follow up with
# simple pairwise comparisons. 
emm4 <- emmeans(lm1, ~ ConditionF:workingF)
prs1 <- pairs(x = emm4,
              simple = "ConditionF", # Simple effects of condition, conditioning on working status
              adjust = "Tukey")     # Tukey HSD for multiplicity correction
prs1
# We are concentrating on the simple pairwise comparisons of means
# for conditions, given that participants were not working (i.e., the top
# set). Multiply those p-values by two to carry Bonferroni correction through.
2*.0118 # .0236
2*.0086 # .0172

# For those not working at time of enrollment in the study, we find
# that depression was 6.79 points lower for those in CBT group as compared
# with WL control (t(214) = 2.35; p = .024), and depression was 6.90 
# points lower for those in the BA group as compared with WL control
# (t(214) = 2.30; p = .017).

# Analyze (2), simple effects of working status at each level of condition.
(jt2 <- joint_tests(object = emm1, by = "ConditionF"))

# Multiple p-values by 3 to make a Bonferroni correction.
jt2$p.value * 3 

# Only the first joint test is significant. There is no need to
# follow up with pairwise comparisons because there are only two
# groups (working and not) at each level of condition. Anyway, 
# here they are.
pairs(x = emm4,
      simple = "workingF", # Simple effects of working status, given condition
      adjust = "Tukey")     # No multiplicity adjustments

# Same result as above, as expected.

#######
# 6. Assignment
#######
# No lab due; study for exam or work on finding a data set for 
# your project.



# simple vs. complex contrast 

emm4
# 1 , -0.5, -0.5 ; to contrast WL with CBT and BA

psi <- constrast(emm4, 
                 list(psa = c(1, -.5, -.5, -1, .5, .5)))
