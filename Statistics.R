#Setting up R
library(tidyverse)
library(haven)
library(car)
library(ggplot2)
library(sjlabelled)

# Video 9 : Chi-squared test of independence

# A chi-squared test of independence was carried out to determine if there was a
# relationship between the curent brand of smartphones being used by people and 
# their preferred brand using the sample data named "phone_brand" and the following 
# parameters:

# Null Hypothesis: No relationship exist
# Alternative Hypothesis: A relationship exist
# Significance Level: 5%

# loading the dataset
phone_brand <- read_sav("datasets//Example_data//phone_brands.sav")

#converting preferred and current from value to labels
phone_brand$preferred <- as_label(phone_brand$preferred)
phone_brand$current <- as_label(phone_brand$current)


# Creating a contingency table to represent the number of cases that fall into
# each combination of categorical variables
pb_contbl <- table(phone_brand$current, phone_brand$preferred)

#viewing contingency table
pb_contbl

#carrying out the chi-square test
chisq.test(pb_contbl)



# Video 12: One Sample T-test

# One Sample T-test was carried out to see if the average size of herrings have
# reduced over time from 400grams using the sample data "herrings". The test was 
# carried with the following parameters:

# Null Hypothesis: Sample mean is equal to 400g

# Apparent Hypothesis: Sample mean is less than 400g

# Significance Level: 5%

# loading the dataset
herrings <- read_sav("datasets//EXample_data//herrings.sav") 

# Converting variable to a continuous variable
herrings$body_weight<- as.numeric(herrings$body_weight)

# To check if the data satisfies all the assumptions needed to carry out the test

# Checking for normality of variable using the Shapiro's test
shapiro.test(herrings$body_weight)

# To check for outliers in the dataset
boxplot(herrings$body_weight)

# To carry out the t-test
t.test(x = herrings$body_weight, mu = 400, alternative = "less",
       conf.level = 0.95)


# Video 15: Independent Sample T-test

# An independent sample T-test was carried out to see if women and men spend the
# same amount of money on clothing. 30 male and female respondents were asked 
# their monthly expenditure on clothing, resulting into the clothing_expenses 
# data set. The test was carried out with the following parameters:

# Null Hypothesis: Men and women spend the same amount of money on clothing**
  
# Alternative Hypothesis: Men and women do not spend the same amount of money on
# clothing
  
# Significance Level: 5%

# loading the dataset
clothing_expenses <- read_sav("datasets//Example_data//clothing_expenses.sav")

# converting the categorical variable to label from value
clothing_expenses$gender <- as_label(clothing_expenses$gender)

# To check if the data satisfies all the assumptions needed to carry out the test
#checking number of categorical groups
clothing_expenses %>% distinct(gender)

#using a box plot to check for outliers
boxplot(clothing_expenses$amount_spent)

#to check for normal distribution using shapiro test
shapiro.test(clothing_expenses$amount_spent)

#to check for equality of variances using the Levene's test
leveneTest(amount_spent ~ gender, clothing_expenses)

# To carry out the independent T-test
t.test(amount_spent ~ gender, clothing_expenses, var.equal = TRUE)



# Video 18: Dependent (Paired) Sample T test

# A Paired T-test was carried out to test if drinking a single glass of beer 
# affects reaction time. The test was carried out with the following parameters:

# Null Hypothesis: A single glass of beer does not affect reaction time

# Alternative Hypothesis: A single glass of beer does affect reaction time

# Significance Level: 5%

#loading the dataset
reaction_times <- read_sav("datasets//Example_data//reaction_times.sav")

# To check if the data satisfies all the assumptions needed to carry out the test
# checking for outliers using boxplots
boxplot(reaction_times$reac_1)
boxplot(reaction_times$reac_2)

# To carry out the paired t-test
t.test(reaction_times$reac_1, reaction_times$reac_2, alternative = 'two.sided',
       paired = 'TRUE')


# Video 21: Correlation Analysis

# The Pearson's correlation coefficient test was used to test whether the age 
# and net monthly income are related in any way using the data gotten from 
# asking 30 respondents, resulting in the age_income.sav dataset

#importing the dataset
age_income <- read_sav("datasets//Example_data//age_income.sav")

# To check if the data satisfies all the assumptions needed to carry out the test
#checking for normal distribution of age variable using Shapiro's test
shapiro.test(age_income$age)
shapiro.test(age_income$income)

#To check for the presence of a linear relationship between the variable using a scatterplot
ggplot(age_income, aes(age, income)) + geom_point()

#To check for the presence of outliers using boxplots
boxplot(age_income$age)
boxplot(age_income$income)

# To carry out the Pearson's test
cor(age_income$age, age_income$income, method = "pearson")



# Video 24: Simple Linear Regression Analysis Overview

# A linear regression model was created to attempt to determine exam scores in 
# the mid-semester exam from the hours spent studying using the study_time.sav 
# dataset which contains records of hour spent studying and exam scores of 20 
# students sampled. The test was carried out with the following parameters:

# Null Hypothesis: The more time you study doesn't mean the higher exam score

# Alternative Hypothesis: The more time you study, the higher your exam score

# Confidence level: 95%

# import dataset
study_time <-  read_sav("datasets//Example_data//study_time.sav")
attach(study_time)

# To create the linear model
model <- lm(Exam_scores ~ Hours)

# To check if the data satisfies all the assumptions needed to carry out the test
#checking for linear relationship between variables
scatter.smooth(Exam_scores, Hours)

#checking for outliers using a boxplot
boxplot(Exam_scores)

#to check for independence of the observation using the durbin-watson test
durbinWatsonTest(model)

#to check for homoscedasticity using the scale-location plot
plot(model, 3)

# To view the result of the regression model
summary(model)

# Another way to test for linearity of the variables and homoscedsticity
#Linearity
plot(fitted(model), resid(model))
abline(0,0)

#homoscedasticity
qqnorm(resid(model))
qqline(resid(model))

# To generate the anova table
anova(model)


# Video 27: Multiple Regression Model
# Multiple regression model was created using the Examerevision.sav dataset to 
# attempt to predict examscores from hours spent revising, anxiety and A-level 
# entry point. With the following parameters:

# Null Hypothesis: hours spent revising, anxiety and A-level entry point have no
# effect on exam score

# Alternative Hypothesis: hours spent revising, anxiety and A-level entry point 
# have an effect on exam score

# Significance level: 0.05

#loading the dataset
exam_revision <-  read_sav("datasets//Example_data//examrevision.sav")

#view dataset
exam_revision

#data summary
summary(exam_revision)

# To build the regression model
model2 <- lm(SCORE ~ HOURS + ANXIETY + A_POINTS, exam_revision)

## To check if the data satisfies all the assumptions needed to carry out the test

#checking for outliers
attach(exam_revision)
boxplot(HOURS)
boxplot(SCORE)
boxplot(ANXIETY)
boxplot(A_POINTS)

#test for linearity
qqnorm(resid(model2))
qqline(resid(model2))

#checking for homoscedasticity of the data
plot(fitted(model2), resid(model2))
abline(0,0)

#test for multicollinearity
vif(model2)

# To view the result of the regression model
summary(model2)


# Video 30: One-way analysis of variance test 

# A one-way ANOVA test was carried out to test whether the weight of parsely is
# inflenced byfetilizer and uses data collected by a farmer which contains weight
# of plants planted with biological fertilizers, chemical fertilizers and no 
# fertilizers at all. With the following parameters:
  
# Null Hypothesis: fertilizer does not affect parsley weight

# Apparent Hypothesis: fertilizer does affect parsley weight
  
# Significance level: 5%
  
#loading the dataset
parsley <-  read_sav("datasets//Example_data//parsley.sav")
parsley$fertilizer <- as_label(parsley$fertilizer)

#descriptive analysis of dataset
summary(parsley)

#descriptive analysis for dataset for each categorical groups
parsley %>% group_by(fertilizer) %>% summarise(count= n(),mean = mean(weight),
                                               sd = sd(weight), min = min(weight),
                                               max = max(weight))

# To check if the data satisfies all the assumptions needed to carry out the test
#checking for outliers
ggplot(parsley, aes(fertilizer, weight)) + geom_boxplot()

#checking for normal disribution for each categorical group
ggplot(parsley, aes(weight, color = fertilizer)) + geom_density() + 
  facet_wrap(~fertilizer)

#checking for homogeneity of variance using Levene's test
leveneTest(weight ~ fertilizer, parsley)

# Carrying out the test
parsley.aov <- aov(weight ~ fertilizer, parsley)

#To view the result of the test
summary(parsley.aov)

# Performing a post hoc test
#recreating the tuckey test
parsley.tukey <- TukeyHSD(parsley.aov)
parsley.tukey



















