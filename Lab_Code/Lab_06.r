###################
# 1. The Data
###################
# The paper is called "Do Information Technologies Improve
#  Teenagers’ Sexual Education? Evidence from a Randomized 
#  Evaluation in Colombia" and is available here:
# http://documents.worldbank.org/curated/en/936401555505751901/pdf/Do-Information-Technologies-Improve-Teenagers-Sexual-Education-Evidence-from-a-Randomized-Evaluation-in-Colombia.pdf

# Download the data from 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/H82SFI
# You will get a zip folder. Inside the folder, go to the subfolder called 
# "Data". Inside, find the data file called "Colombia_Sex_Ed.dta".

###################
# 2. Cleaning
###################
# The data are saved as a Stata data file (.dta). Need to use a package 
# such as haven to import it. Install the haven package with
# install.packages("haven"), then load it. 

library(haven)
library(tidyverse) 
dat <- read_dta("~/Dropbox/Teaching/Linear Models and Experimental Design/2021 SPRING/06 Mult Comp/Colombia_Sex_Ed.dta")
#dat <- read_dta(file.choose())
dat <- read_dta("Colombia_Sex_Ed.dta")  
dim(dat) # 1430 variables!

# There are a lot of variables here. We will only focus on the 
# categorical treatment status variable, called, "treatmentstatus" and
# the continuous outcome variable called, 
names(dat)

# The grep() function searches for a character pattern in x.
grep(pattern = "treatmentstatus", x = names(dat))

# The output from the grep() call above tells us that the 
# treatment status variable is the 798th variable in the data
# set.
table(dat$treatmentstatus)

# The file called "MainTables.do" contains the Stata code to 
# replicate the tables in the paper. The code for creating 
# Table 3 begins on line 125. There we see that the coded names
# for the outcome variables are know_std_prev, know_sexual_violence,
# know_std_prevmeth, know_preg_prev, know_condom_use, and know. These
# six variable names correspond with the outcomes presented in Table
# 3 in the World Bank paper.

# We will use the first variable, know_std_prev, which measures 
# knowledge of symptoms and causes of STIs. See Table 3 in the paper 
# for definitions.
# The argument value = TRUE will cause grep() to print the value 
# of the pattern match found instead of its position.
grep(pattern = "know_std_prev", x = names(dat), value = FALSE)
grep(pattern = "know_std_prev", x = names(dat), value = TRUE)

# This returns any patterns that *contain* the string "know_std_prev", 
# but we only want those that match the string *exactly*. One solution
# is to use regular expressions (https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf)
# to specify that the pattern should begin and end at the first and
# last characteres in our string. ^ specifies the start of a string and
# $ specifies the end. The strings we want, however, begin with r1_, r2_, 
# or r3_. Here, we can use the period to denote "any character".
grep(pattern = "^r._know_sexual_violence$", x = names(dat), value = TRUE)

# Ok, now that we know we have the three variables we need, let's get
# their positions.
grep(pattern = "^r._know_std_prev$", x = names(dat), value = FALSE)

# r1, r2, and r3 represent the different time points at which data
# was collected. r1 is baseline, r2 is one week after intervention, 
# and r3 is six months after intervention.

# Here we will examine the efficacy of the intervention at one
# week after it was administered (i.e., we will use r2 as outcome).

# First, let's create a data set that only contains the variables we
# need.
#dat2 <- subset(dat, select = c("treatmentstatus", "r1_know_std_prev", 
                               "r2_know_std_prev"))

dat2 <- dat %>%
  select(treatmentstatus, r1_know_sexual_violence, r2_know_sexual_violence) 

im(dat2)
head(dat2)

# Next, delete rows with missing data.
dat2 <- na.omit(dat2)
dim(dat2)
table(dat2$treatmentstatus)

# Note that this number, 4373, matches the sample size given
# in the first column of Table 3, which corresponds with this
# analysis.

# Let's also create a categorical factor for treatment status
# called treatmentF.
dat2$treatmentF <- factor(x = dat2$treatmentstatus, 
                          levels = c(0, 1, 2),
                          labels = c("Control", "Spillover", "Treatment"))
table(dat2$treatmentF)


###################
# 3. Analysis
###################
# Assess baseline balance on the outcome variable using r1.
boxplot(formula = r1_know_sexual_violence ~ treatmentF,
        data = dat2,
        xlab = "Treatment Group", 
        ylab = "Knowledge",
        main = "Knowledge of Sexual Violence by Group \n at Baseline")
(mns <- by(data = dat2$r1_know_sexual_violence, 
           INDICES = dat2$treatmentF,
           FUN = mean, na.rm = TRUE))
(vars <- by(data = dat2$r1_know_sexual_violence, 
            INDICES = dat2$treatmentF,
            FUN = var, na.rm = TRUE))
(ns <- table(dat2$treatmentF))

# Cohen's ds at baseline
# 1 vs 2
(d12 <- (mns[1] - mns[2]) / sqrt(((ns[1] - 1)*vars[1] + (ns[2] - 1)*vars[2]) / (ns[1] + ns[2] - 2)))
# 1 vs 3
(d13 <- (mns[1] - mns[3]) / sqrt(((ns[1] - 1)*vars[1] + (ns[3] - 1)*vars[3]) / (ns[1] + ns[3] - 2)))
# 2 vs 3
(d23 <- (mns[2] - mns[3]) / sqrt(((ns[2] - 1)*vars[2] + (ns[3] - 1)*vars[3]) / (ns[2] + ns[3] - 2)))

# Assess at r2 (one week post intervention).
boxplot(formula = r2_know_sexual_violence ~ treatmentF,
        data = dat2,
        xlab = "Treatment Group", 
        ylab = "Knowledge",
        main = "Knowledge of Sexual Violence at Post Intervention")
(mns_p <- by(data = dat2$r2_know_sexual_violence, 
           INDICES = dat2$treatmentF,
           FUN = mean, na.rm = TRUE))
(vars_p <- by(data = dat2$r2_know_sexual_violence, 
            INDICES = dat2$treatmentF,
            FUN = var, na.rm = TRUE))

# Cohen's ds at 1 wk post intervention
# 1 vs 2
(d12_post <- (mns_p[1] - mns_p[2]) / sqrt(((ns[1] - 1)*vars_p[1] + (ns[2] - 1)*vars_p[2]) / (ns[1] + ns[2] - 2)))
# 1 vs 3
(d13_post <- (mns_p[1] - mns_p[3]) / sqrt(((ns[1] - 1)*vars_p[1] + (ns[3] - 1)*vars_p[3]) / (ns[1] + ns[3] - 2)))
# 2 vs 3
(d23_post <- (mns_p[2] - mns_p[3]) / sqrt(((ns[2] - 1)*vars_p[2] + (ns[3] - 1)*vars_p[3]) / (ns[2] + ns[3] - 2)))

# Assess constant variance assumption
# 1. Examine the boxplot - IQRs look reasonably similar.
# 2. Calculate max/min variance ratio. 
vars_p
ns
(ns[3]/ns[1]) * (vars_p[1]/vars_p[3]) # No problem here.
# 3. Levene's test.
library(car)
leveneTest(r2_know_sexual_violence ~ treatmentF, data = dat2) # Not significant.
# Based on the available evidence, the assumption of constant variance 
# appears tenable.

# One-way between-subjects ANOVA with 1 wk post intervention scores.
# Fit the full model.
lmF <- lm(r2_know_sexual_violence ~ treatmentF,
          data = dat2)
summary(lmF)

# Assess the normality assumption (load package car if not already).
qqPlot(lmF)
# No major concerns here. Proceed.

# Assess independence assumption. Here we have reason to be concerned.
# The data were collected from classrooms and schools. Our simple 
# ANOVA model does not account for this so we need to exercise caution
# in interpreting our results. 

# Note that the summary of lmF above gives the coefficients for the
# dummy-coded indicators with "Control" held out as reference.
levels(dat2$treatmentF) # Note "Control" is the first level.


# for constrasts coding 
#options(contrasts = c("contr.sum", "contr.poly")) 
# to change back to dummy coding (which is default) 
#options(contrasts = c("contr.treatment.", "contr.poly")) 


# According to the summary of lmF, the mean of the control group at
# 1 week after intervention was -.077. The mean of the spillover was
# -.077 + .064 = -.013. And the mean of the treamtent group was
# -.077 + .310 = .233

Anova(lmF, type = 3)

# The treatment factor had a significant effect on STI knowledge.

# Now follow up with pairwise comparisons.
library(emmeans)
emm1 <- emmeans(object = lmF,
                specs = ~ treatmentF)
emm1 # Note that these estimated marginal means line up with our 
     # calculations from above.

pairs(x = emm1, 
      adjust = "None") # Inappropriate because no adjustment for 
                       # multiple comparisons.
pairs(x = emm1, 
      adjust = "Tukey")  # Tukey-adjusted. Ok to interpret.
pairs(x = emm1, 
      adjust = "Bonferroni")  # Bonferroni-adjusted. Ok to interpret.
pairs(x = emm1, 
      adjust = "Holm")  # Holm-adjusted. Ok to interpret. Note, 
                        # Holm is always at least as powerful as Bonf.
pairs(x = emm1,
      adjust = "Scheffe") # Scheffe-adjusted. Ok to interpret.
pairs(x = emm1,
      adjust = "fdr") # Benjamini-Hochberg FDR-adjusted. I prefer to 
                      # use a strategy here that controls familywise
                      # error rate.

# TASKS
# 1. Repeat the analyses done here with the sexual violence 
#    knowledge outcome at one week post intervention. Report 
#    and interpret the results of the ANOVA and pairwise 
#    comparisons.

grep(pattern = "know_sexual_violence", x = names(dat), value = TRUE)

dat3 <- dat %>%
  select(treatmentstatus, 
         r1_know_sexual_violence, 
         r2_know_sexual_violence) 

dat3$treatmentstatus <- factor(dat3$treatmentstatus, 
                                levels = c("control", "spillover", 
                                           "treatment")) 




