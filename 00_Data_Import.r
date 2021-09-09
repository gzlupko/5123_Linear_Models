#######
# 1. Import
#######

# Import the data. This can be done via the "Files" tab
# in your plotting window pane in RStudio. Or, it can be
# one manually as follows, though, on your machine you 
# would need to enter the correct file path.
#dat <- read.csv(file = "/Users/bryankeller/Dropbox/Teaching/Linear Models and Experimental Design/2021 SPRING/00 Syllabus/smokingRCT.csv")


# GZ additions to Keller's code below 

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
# Rename some of the variables to have shorter names
names(dat2)
names(dat2)[4] <- "cig_base"
names(dat2)[5] <- "yrs_smoke"
names(dat2)[6] <- "bdi_base"
names(dat2)[8] <- "cig_EOT"
names(dat2)
head(dat2)

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

#######
# 3. Check Balance
#######

# Check baseline balance on cigarette usage,
# sex, and age
with(data = dat2, 
     expr = by(cig_base, Condition, mean, na.rm = TRUE))
with(data = dat2,
     expr = by(Age, Condition, mean, na.rm = TRUE))

# Make parallel boxplots to graphically examine balance 
# for numeric baseline data
boxplot(bdi_base ~ ConditionF, data = dat2,
        xlab = "Condition", 
        ylab = "BDI",
        main = "Baseline BDI by Group")
boxplot(cig_base ~ ConditionF, data = dat2,
        xlab = "Condition", 
        ylab = "Number of Cigarettes",
        main = "Baseline Cig Usage by Group")
boxplot(Age ~ ConditionF, data = dat2,
        xlab = "Condition", 
        ylab = "Age (yrs)",
        main = "Baseline Age by Group")

# When both variables are categorical (as in condition
# and sex), create a table instead of calculating group
# means.
t1 <- table(dat2$ConditionF, dat2$SexF)
t1

# Examine proportions of males and females across the 
# three groups.
t2 <- prop.table(x = t1, margin = 1)
t2

# When both variables are categorical, create a barplot
# instead of boxplots.
barplot(t2) # The barplot does not have the orientation
# we are looking for so transpose the table. The function 
# to transpose a table is t().
t(t2)
# Now plot the transposed table and add labels and legend.
barplot(t(t2), ylab = "Proportion", 
        main = "Proportion Male/Female by Group",
        legend = TRUE)

#######
# 4. Examine Relationships with Outcome
#######

# Let's look at the cigarette usage variable and
# try to get a sense about whether the intervention
# had any effect on cigarette useage at the end of 
# the trial.

# First, is there a relationship between baseline
# and end-of-trial smoking frequency?
plot(x = dat2$cig_base,
     y = dat2$cig_EOT,
     xlab = "Baseline Cigarette Usage",
     ylab = "EOT Cigarette Usage",
     main = "Cigarette Usage at Baseline and EOT")

# Fit a simple linear regression model to quantify
# the strength of the relationship.
lm1 <- lm(formula = bdi_EOT ~ bdi_base, 
          data = dat2)
summary(lm1)
abline(lm1, col = "blue", lwd = 2)

# Let's look at the cig usage means at EOT by group
with(data = dat2,
     expr = by(cig_EOT, ConditionF, mean, na.rm = TRUE))

# Boxplot of BDI means at EOT
boxplot(cig_EOT ~ ConditionF,
        data = dat2, 
        xlab = "Condition",
        ylab = "Cigarette Usage",
        main = "Cigarette Usage by Group at EOT")

# Convert the data from wide to long format
# to create an interaction plot.
datL <- reshape(data = dat2,
                varying = list(c("cig_base", "cig_EOT"),  # which columns should be combined?
                               c("bdi_base", "bdi_EOT")), # separate list entries for cig/bdi 
                v.names = c("cig", "bdi"),                # name of new data columns?
                timevar = c("Time"),                      # name of new factor column?
                times = c("base", "EOT"),                 # names of timevar elements?
                direction = "long")                       # go from wide->long or long->wide?
head(datL)
dim(datL)
row.names(datL) <- 1:550
head(datL)

with(data = na.omit(datL), expr = 
       interaction.plot(x.factor = Time,
                 trace.factor = ConditionF,
                 response = cig, 
                 type = "l",
                 lwd = 2,
                 col = 2:4,
                 ylab = "Cigarette Usage",
                 xlab = "Time",
                 main = "Time by Group Interaction Plot\nfor Cigarette Usage"))

