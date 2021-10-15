# 5123_Linear_Models
This repository contains shared R code for Group 1 lab assignments, HUDM 5123 Linear Models & Experimental Design, Fall 2021. 



## Table of Contents 
* [Course Overview](#Course-Overview) 
* [Labs](#Labs)
* [Sample Data Visualizations](#Sample-Data-Visualizations)
* [Sample Code](#Sample-Code)


### Course Overview

This course provides an overview of experimental design and analysis from the perspective of
the general linear modeling framework. Topics include theory of experimental design, single and multiple factor experiments, between-subject and within-subject designs, factorial and nested designs, random effects, analysis of covariance, and blocking. The course includes lab time devoted to computer applications.

### Labs 
Below are the list of lab assignments and topics covered
* Lab 1: Analysis of Variance (ANOVA) using GLM Framework 
* Lab 2: Measures of Effect, Association, and Assumptions for ANOVA
* Lab 3: Pairwise and Complex Contrasts in One-Way ANOVA
* Lab 4: Planned Pairwise Comparisons and Corrections for Type I Error Rate

### Sample Data Visualizations



#### Lab 1 & 2 - ANOVA and Assumptions



![alt text](https://github.com/gzlupko/5123_Linear_Models/blob/main/Visualizations/cig_eot_boxplot.jpeg) 


Normal Q-Q Plot 

![alt text](https://github.com/gzlupko/5123_Linear_Models/blob/main/Visualizations/qq-polot.jpeg) 





![alt text](https://github.com/gzlupko/5123_Linear_Models/blob/main/Visualizations/cig_EOT_hist.jpeg) 






### Sample Code 

##### Common Functions and Tasks 


###### Create Factors: 


```
dat2$treatmentF <- factor(x = dat2$treatmentstatus, 
                          levels = c(0, 1, 2),
                          labels = c("Control", "Spillover", "Treatment"))
```

##### Checking Assumptions

###### Homoscedasticity of Variance: 


```
# examine variance by group
(vars_p <- by(data = dat2$r2_know_sexual_violence, 
            INDICES = dat2$treatmentF,
            FUN = var, na.rm = TRUE))

# Levene's Test
library(car)
leveneTest(r2_know_sexual_violence ~ treatmentF, data = dat2)
```


###### Normality: 

```
# generate full model and use model in Q-Q Plot
lmF <- lm(r2_know_sexual_violence ~ treatmentF,
          data = dat2)
qqPlot(lmF)
```







##### Omnibus Test & Pairwise Comparisons 

```
# Omnibus test of the null with one-way ANOVA 
lmF <- lm(r2_know_sexual_violence ~ treatmentF,
          data = dat2)

# Follow up ombnibus with pairwise comparisons
library(emmeans)
emm1 <- emmeans(object = lmF,
                specs = ~ treatmentF)

# for multiple comparisons, control for Type I error rate
# using relevant correction
pairs(x = emm1, 
      adjust = "Tukey")              
                

```


