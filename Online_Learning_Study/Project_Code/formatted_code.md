

```{r Setup, include=FALSE}
# load libraries and set path 
library(tidyverse) 
library(haven) 
library(psych) 
library(ggridges) 
library(car)
library(apaTables) 
library(emmeans) 
setwd("/Users/gianzlupko/Desktop/5123 Linear Models/5123_Linear_Models/Online_Learning_Study")   
```

Import data and view fields
```{r Import-Data, include = FALSE}
learning <- read_dta("online_assessment.dta") 
head(learning) 
```


```{r Data-Cleaning}

# rename columns
learn2 <- learning %>%
  rename(course_grade = falsexam) 

# create column that stores character string identifying course type: 
learn2 <- learn2 %>%
  mutate(format_type = 
           ifelse(format_ol == 1, "online", 
                  ifelse(format_blended == 1, "hybrid", 
                         ifelse(format_ol == 0 & format_blended == 0, 
                                "traditional", "NA")))) 

# store format type as factor: 
learn2$format_type <- as.factor(learn2$format_type) 

```


Review of Missing Data
The following code chunk calculates descriptive statistics regarding missingness of data that are presented in Appendix B
```{r Missing-Data}

# attrition by instructional mode: 
# note, Alpert et al. (2016) coded drop out data 
# 1 = students given permission at outset of semester but did not sign up
# 2 = students who did enroll that later dropped out
# 3 = students who completed the course 

attrition <- learn2 %>%
  group_by(enroll_count) %>%
  select(format_type, enroll_count) %>%
  table() %>%
  data.frame()

#write.csv(attrition, "attrition_by_condition.csv") 
online_attrition = (29 + 50)
online_attrition/172

# store new data with only those who completed the course 

completed_course <- learn2 %>%
  filter(enroll_count == 3) 

# count n-size for course completion across experimental conditions 
completed_course %>%
  group_by(format_type) %>% 
  count(format_type) 


# check missigness of GPA (covariate): 
# for those that completed coourse, 48 GPA records missing total
gpa_missing <- completed_course %>%
  select(format_type, gpa) %>%
  group_by(format_type) %>%
  filter(is.na(gpa)) %>%
  count(format_type)

# filter data set for subset with complete prior GPA records 
completed <- completed_course %>% 
  filter(!is.na(gpa)) 


```


Check Outliers 

```{r outliers, warning = F}

# identify course outliers 
completed %>%
  select(course_grade, format_type) %>%
  filter(course_grade < 50)

# there are two final grades of zero recorded in the online sample 
# this may be a data quality issue; perhaps these students actually dropped the class 

# below, will perform a statistical test of outlier 
# Grubbs test is a statistical test for outliers that tests the null hypothesis that 
# the highest/lowest (in this case lowest) value is not an outlier 
library(outliers)
test <- grubbs.test(completed$course_grade) 
test

# test is significant for the two values in the online format that received a zero
# will remove them and test again to see if the score of 48 in the hybrid format is an outlier 

completed <- completed %>%
   filter(course_grade > 0) 

test1 <- grubbs.test(completed$course_grade) 
test1
# Grubbs test finds that p-value is not signficant so the score of 48 is not an outlier

```




Assumptions for ANOVA

```{r ANOVA-Assumptions-Check}

# constant variance - Maxwell et al. (2017)'s variance ratio: 
# step 1: calculate variance by group

group_variance <- completed %>%
  group_by(format_type) %>%
  summarise(variance = var(course_grade))
group_n <- completed %>%
  group_by(format_type) %>%
  count(format_type)
  
maxwell_ratio <- (96*229.705) / (85*79.31)
maxwell_ratio
(maxwell_ratio < 4) 
# true; variance ratio is less than 4, so we have evidence for constant variance

# next, conduct Levene's test of homogeneity of variance 
lm1 <- lm(course_grade ~ format_type, data = completed) 
library(car)
leveneTest(lm1)

# visualize group variances with boxplot 
ggplot(data = completed, 
       aes(format_type, course_grade, fill = format_type)) + 
  geom_boxplot() + coord_flip() + ylab(label = "Final Course Grade") + 
  xlab("Instructional Format")+ labs(fill = "Class Format") +  
  theme(legend.position = "none")


# test assumption of normality with QQ-Plot 
qqPlot(lm1)

# outliers in data set 
completed %>%
  select(course_grade) %>%
  arrange(course_grade)

# test assumption of normality with histograms or distribution plots 

ggplot(data = completed, aes(x = course_grade)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(format_type ~ .) + xlab("Final Course Grade") + 
  ylab("Count")


```


Univariate Graphical Plots

The folowing code chunk generated a series of plots that were used to look at variable distributions, primarily at the project proposal stage. 

```{r Univariate-Plots}

# boxplot 
ggplot(data = completed_course, 
       aes(format_type, course_grade, fill = format_type)) + 
  geom_boxplot() + coord_flip() + ylab(label = "Final Grade") + 
  xlab("Class Format")+ labs(fill = "Class Format") +
  scale_x_discrete(labels=c("Traditional", "Online", "Hybrid")) 


ggplot(data = completed_course, 
       aes(course_grade, fill = format_type)) + geom_histogram()

# final grades by group histogram 
ggplot(data = completed_course, aes(x = course_grade)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(format_type ~ .) + xlab("Final Grade") + 
  ylab("Count")


# GPA by group 
ggplot(data = completed_course, aes(x = gpa)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(format_type ~ .) + xlab("GPA") + 
  ylab("Count") + stat_bin(bins = 8) 

# QQ plot of grades to test normality assumption:

lm1 <- lm(course_grade ~ format_type, data = completed_course)
qqPlot(lm1)


```

One-way ANOVA 

```{r one-way-ANOVA}

# one-way ANOVA
lm1 <- lm(course_grade ~ format_type, data = completed) 

lm1 %>%
  summary() 

# The ANOVA Source Table - write to file on local in APA format 
#apa.aov.table(lm1, filename = "ANOVA_APA.rtf", table.number = 2)

```


Assumptions for covariate

```{r covariate-assumptions}

# 1. demonstrate strong correlation b/w covariate and outcome 
# and that they are significantly related 
lm_cov <- lm(course_grade ~ gpa, data = completed)
summary(lm_cov) 

# Next, ensure that the cov-outcome relationship is linear 
# To do so, visually inspect a scatterplot and confirm whether it indicates a linear relationship 
# using a limited view of x-axis; there were a few extreme outliers
# zooming in on a more limited view allows us to more closely visually inspect that relationship is linear 
plot(x = completed$course_grade, y = completed$gpa)
ggplot(data = completed, aes(x = course_grade, y = gpa)) + geom_point() + 
  scale_x_continuous(limits = c(50, 100)) + xlab("Course Grade") + ylab("Prior GPA")

# 2. covariate balance 

ggplot(data = completed, 
       aes(format_type, gpa, fill = format_type)) + 
  geom_boxplot() + coord_flip() + ylab(label = "Prior GPA") + 
  xlab("Class Format")+ labs(fill = "Class Format") +theme(legend.position = "none") 

# show variance for prior GPA across groups and use Maxwell et al. (2017) formula 

completed %>%
  group_by(format_type) %>%
  count(format_type) 

completed %>%
  group_by(format_type) %>%
  summarise(variance = var(gpa)) 


maxwell_ratio <- (96*.346) / (83*.251)
maxwell_ratio
(maxwell_ratio < 4) 

# 3. treatment by covariate interaction 
# ANCOVA also assumes no treatment by covariate interaction 
# to test for this, add an interaction term to regression model 
# ensure that interaction is not significant (e.g. constant effect of gpa for all values of course grade)

lm_cov_interaction <- lm(course_grade ~ format_type + gpa + gpa:format_type, 
                         data = completed)

summary(lm_cov_interaction) 
apa.aov.table(lm_cov_interaction, filename = "ANCOVA_Interaction_APA.rtf", table.number = 2)
# plot treatment by covariate interaction


completed %>%
  ggplot(aes(x = gpa, y = course_grade, color = format_type)) + 
  geom_point() + geom_smooth(method = "lm", se = FALSE) + 
  scale_y_continuous(limits = c(30, 100)) + 
   xlab("GPA") + ylab("Final Course Grade") 

```



Final ANCOVA Model 
All assumptions for covariate inclusion were satisfied 

```{r final-ANCOVA}

lm_cov_2 <- lm(course_grade ~ format_type + gpa, 
               data = completed) 

summary(lm_cov_2) 

# expport ANCOVA table in APA format to local 
#apa.aov.table(lm_cov_2, filename = "Final_ANCOVA_Table.rtf") 


```



Pairswise constrasts for adjusted group means 

```{r pairwise-contrasts}

# Calculate ajusted group means conditioned on prior GPA for each group  
emm1 <- emmeans(object = lm_cov_2,
                specs = ~ format_type)
emm1

# the below code shows the same results as the first contrast but in the emmeans() function the GPA covariate is explicitly listed for documentation/presentation's sake 
emm2 <- emmeans(object = lm_cov_2,
                specs = ~ format_type + gpa)
emm2

# Calculate pairwise constrast to test for differences between adjusted group means
# Use Tukey adjustment to control for Type I familywise error rate 
pairs(emm1) 

# plot showing differences between instructional format and course grade while controlling for GPA
# notice, plot shows that students assigned to the traditional instruction performed, on average, the best 
p <- ggplot(data = completed, (aes(x=gpa, y=course_grade, color= format_type))) + 
  theme_classic() +ylab("Final Course Grade") + xlab("GPA") +
  labs(color= "Group")   

p + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

p + geom_point() + geom_smooth(method = lm, se = FALSE) 


```
