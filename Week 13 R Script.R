

# Logisitc Regression

# Logistic Regression is a multiple regression with a categorical outcome variable (1,0)
# and predictor variables that are categorical or continuous.

# The aim of the model is to predict which of the two categories a case is likely to 
# fall in.

# Assumptions

# Linearity - There is a linear relationship between any continuous predictors and the logit
# of the variable outcome. 

# Independence of Errors - Same as for ordinary regression. Cases of data should not be
# related.

# Multicollinearity - Predictors should not be too highly correlated. We can check these
# with VIF statistics.

# -------------------

# Incomplete information - unused categories 
# complete seperation - Perfect prediction - this sucks! 

# ====================================================================

# Let's load our libraries and read in some data:

# install.packages("mlogit")

library(car)
library(mlogit)
library(readr)

admissions <- read_csv("~/To File 20170817/LIS 590 BAO/Slides/Week 13 R Script/Week 13 R Script/College_Admissions.csv")
View(admissions)


# =====================================================================

# Admissions Model

# We can always view our data, but we can also use the head function to return the first six rows:

head(admissions)

# -------------

admissions$rank <- factor(admissions$rank)


model1 <- glm(admit ~ gre + gpa + rank,
              data = admissions,
              family = binomial())
  
summary(model1)

  
# Assessing the model

# -------------

# The Deviance Statistic

# The deviance statistic determines the overall fit of a logistic regression model.
# Larger values of the deviance statistic indicate badly fit models.

# We first look at the Null deviance which is 499.98, this is the deviance of the model
# with no predictors. We want to see a decrease in size in the difference between the NULL
# and residual deviances. 

# In this case the Residual Deviance is 459.44. The decrease in size indicates that 
# the model is better at predicitng whether someone was admitted after the predictor
# variables were added. Yay!

# -------------

# Model Chi-Square Statistic

# The model chi-Square can be used to determine how much better it is at predicting an
# outcome with the predictors added than with only the constant. 

# To get the chi-square statistic we can subtract the deviance from the null deviance
# as follows:

model1_chi <- model1$null.deviance - model1$deviance

model1_chi

# We next need to get the difference in the degrees of freedom between the model and the
# null model:

model1_chi_df <- model1$df.null - model1$df.residual 

model1_chi_df
  
# finally, we need to calculate the probability associated with the Chi-Square Statistic:

model1_chisq.prob <- 1 - pchisq(model1_chi, model1_chi_df)
  
model1_chisq.prob

# We get a value of .000000008 - this probability is less than .05, 
# therefore we can reject the null hypothesis that the model is not better 
# than chance at predicting the outcome.

# In other words - adding the predictor variables produced a significant 
# improvement in the fit of our model.

# -------------

# Coefficients

# The beta values for the coefficients can be found under the 'Estimate'
# column. 

# These represent the change in hte logit of the outcome variable associated with a one-unit
# change in the predictor variable - i.e. the natural logarithm of the odds of Y (admissions)
# occuring. 

# To determine if a coefficient is signifcantly different from zero - meaning it is making
# a contribution to the model - it will be indicated in the Z value probability (Pr(>|z|).
# In this case all of our three variables are significant. 

# -------------

# Pseudo-R2

# In a logistic regression we don't have an R2 like we would in a linear or muli-variate
# Regression, there are however several analoguous tests that we can do to generate what
# are referred to as Pseudo-R2. 

# Andy provides a nice function which I've reproduced below to calculate three different
# Pseudo-R^2's. For full details on what they are doing I would refer you back to page 334.

logistic_r2 <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev /nullDev
  R.cs <- 1 - exp(-(nullDev - dev)/modelN)
  R.n <- R.cs / (1 - (exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l,3), "\n")
  cat("Cox and Snell R^2       ", round(R.cs,3), "\n")
  cat("Nagelkerke R^2          ", round(R.n,3), "\n")
}
  
logistic_r2(model1)  
  
# As you can see, by all three measures the effect size of the model is small - 
# meaning we can't explain much of the variability in the outcome with just the
# variables we have. 
  
# -------------

# The Odds Ratio

# If the odds ratio is greater than 1 it means that as the predictor increases, the odds of the 
# outcome occuring increase.
# A value less than 1 means that as the predictor increases, the odds of the outcome occuring 
# decrease.

exp(model1$coefficients)

# So, for every 1 point increase in a students GPA, the odds of their being admitted increase
# by 2.2 times.

# -------------

# Confidence Intervals

# Again, like with the other forms of regression we have looked at: 
# the improtant thing is that our ratios don't cross 1. 

exp(confint(model1))

# -------------

# Checking Residuals

# It is important to check for outliers in your residuals just like you did
# with linear regression:

admissions$predicted_probabilities <- fitted(model1) # Here we are getting the model predictions.
admissions$standardized_residuals <- rstandard(model1)
admissions$studentized_residuals <- rstudent(model1)
admissions$dfbeta <- dfbeta(model1)
admissions$dffit <- dffits(model1)
admissions$leverage <- hatvalues(model1)


View(admissions)


# we want 95% of our cases to have standardized residuals within plus or minus 2, in a set of 400 cases we'd expect 
# roughly 380 to be around 2, or roughly 20 cases outside the limit. Let's see"

admissions$standardized_residuals > 2 | admissions$standardized_residuals < -2

# We want to store this data in our dataframe:

admissions$large_residual <- admissions$standardized_residuals > 2 | admissions$standardized_residuals < -2

View(admissions)

sum(admissions$large_residual)

admissions[admissions$large_residual,c("admit","gre","gpa","rank","standardized_residuals")]

# Those over 4 merit attention. There are none here so we are good!

# You'll notice that those which stand out are individuals that have
# been admitted with low GRE scores, LOW GPA's, and from poorly ranked schools.

# -------------

# DFBeta should be less than 1.

# -------------

# Testing for multicollinearity

# If the largest VIF is greater than 10 then there is a problem.
# If the average VIF is substantially greater than 1 then there may be bias in the model.
# If the the tolerence is below .1 that indicates a serious problem.
# Tolerence below .2 indicates a potential problem.

vif(model1)
1/vif(model1) # Tolerence
mean(vif(model1)) # Average

# It doesn't look like there is any collinearity in 
# our model.

# -------------

# Linearity of the Logit

# First we need to create the variables we will use for the test:
# We exclude rank as it is a factor and you can't take the log of a factor.

admissions$logGRE <- log(admissions$gre)*admissions$gre
admissions$logGPA <- log(admissions$gpa)*admissions$gpa


View(admissions)

model1_test <- glm(admit ~ gre + gpa  + logGRE + logGPA,
                   data = admissions,
                   family = binomial())

summary(model1_test)

# What we are looking for here is for all interactions to have Pr(>|z|) greater than .05
# In other words we don't want them to be significant.
# They aren't so we are good to go.



# =====================================================================
# =====================================================================


# Titanic Model


titanic <- read_csv("~/To File 20170817/LIS 590 BAO/Slides/Week 13 R Script/Week 13 R Script/titanic_manifest.csv")
View(titanic)

# We can always view our data, but we can also use the head function to return the first six rows:

head(titanic)

# -------------

# Build Model

model2 <- glm(Survived ~ Age + Fare,
              data = titanic,
              family = binomial())

summary(model2)


# Assessing the model

# -------------

# The Deviance Statistic

# Remember, the deviance statistic determines the overall fit of a logistic regression model.
# Larger values of the deciance statistic indicate badly fit models.

# We first look at the Null deviance which is 960.90, this is the deviance of the model
# with no predictors. We want to see a decrease in size in the difference between the NULL
# and residual deviances. 

# In this case the Residual Deviance is 888.68 The decrease in size indicates that 
# the model is better at predicitng whether someone was admitted after the predictor
# variables were added. 

# -------------

# Model Chi-Square Statistic

# The model chi-Square can be used to determine how much better it is at predicting an
# outcome with the predictors added than with only the constant. 

# To get the chi-square statistic we first subtract the deviance from the null deviance
# as follows:

model2_chi <- model2$null.deviance - model2$deviance

model2_chi

# We next need to get the difference in the degrees of freedom between the model and the
# null model:

model2_chi_df <- model2$df.null - model2$df.residual 

model2_chi_df

# finally, we need to calculate the probability associated with the Chi-Square Statistic:

model2_chisq.prob <- 1 - pchisq(model2_chi, model2_chi_df)

model2_chisq.prob


# If we get tired of typing this out each time, we can simply create a new function
# to handle it for us:

chi_squared <- function(LogModel){
  model2_chi <- model2$null.deviance - model2$deviance
  model2_chi
  model2_chi_df <- model2$df.null - model2$df.residual 
  model2_chisq.prob <- 1 - pchisq(model2_chi, model2_chi_df)
  cat("Chi_Squared ", model2_chi, "\n")
  cat("Probability ", model2_chisq.prob, "\n")
}

chi_squared(model2)

# We get a value of .0000000000000002 - this probability is less than .05, 
# therefore we can reject the null hypothesis that the model is not better 
# than chance at predicting the outcome.

# In other words - adding the predictor variables produced a significant 
# improvement in the fit of our model.

# -------------

# Coefficients

# The beta values for the coefficients can be found under the 'Estimate'
# column. 

# These represent the change in the logit of the outcome variable associated with a one-unit
# change in the predictor variable - i.e. the natural logarithm of the odds of Y (Survival)
# occuring. 

# To determine if a coefficient is signifcantly different from zero - meaning it is making
# a contribution to the model - it will be indicated in the Z value probability (Pr(>|z|).
# In this case both of our variables are significant. 

# -------------

# Pseudo-R2

# In a logistic regression we don't have an R2 like we would in a linear or muli-variate
# Regression, there are however several analoguous tests that we can do to generate what
# are referred to as Pseudo-R2. 

# Andy provides a nice function which I've reproduced below to calculate three different
# Pseudo-R^2's. For full details on what they are doing I would refer you back to page 334.

logistic_r2 <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev /nullDev
  R.cs <- 1 - exp(-(nullDev - dev)/modelN)
  R.n <- R.cs / (1 - (exp(-(nullDev/modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l,3), "\n")
  cat("Cox and Snell R^2       ", round(R.cs,3), "\n")
  cat("Nagelkerke R^2          ", round(R.n,3), "\n")
}

logistic_r2(model2)  

# As you can see, by all three measures the effect size of the model is small - 
# meaning we can't explain much of the variability in the outcome with just the
# variables we have. 

# -------------

# The Odds Ratio

# If the odds ratio is greater than 1 it means that as the predictor increases, the odds of the 
# outcome occuring increase.
# A value less than 1 means that as the predictor increases, the odds of the outcome occuring 
# decrease.

exp(model2$coefficients)

# So, for every 1 dollar increase in a passenger's fare, the odds of their surviving increase
# by 1.02 times. For every additional year in a passenger's age, the odds of their survival decrease
# by .99 times.

# -------------

# Confidence Intervals

# Again, like with the other forms of regression we have looked at: 
# the improtant thing is that our ratios don't cross 1. 

exp(confint(model2))

# -------------

# Checking Residuals

# It is important to check for outliers in your residuals just like you did
# with linear regression:

titanic$predicted_probabilities <- fitted(model2) # Here we are getting the model predictions.
titanic$standardized_residuals <- rstandard(model2)
titanic$studentized_residuals <- rstudent(model2)
titanic$dfbeta <- dfbeta(model2)
titanic$dffit <- dffits(model2)
titanic$leverage <- hatvalues(model2)


View(titanic)


# we want 95% of our cases to have standardized residuals within plus or minus 2, in a set of 712 cases we'd expect 
# roughly 676 to be around 2, or roughly 36 cases outside the limit. Let's see"

titanic$standardized_residuals > 2 | titanic$standardized_residuals < -2

# We want to store this data in our dataframe:

titanic$large_residual <- titanic$standardized_residuals > 2 | titanic$standardized_residuals < -2

# View(titanic)

sum(titanic$large_residual)

titanic[titanic$large_residual,c("Survived","Age","Fare","standardized_residuals")]

# Those over 5 merit attention. There are none here so we are good!

# You'll notice that those which stand out are individuals that have
# either a really young age or a high fare.

# -------------

# DFBeta should be less than 1.

# -------------

# Testing for multicollinearity

# If the largest VIF is greater than 10 then there is a problem.
# If the average VIF is substantially greater than 1 then there may be bias in the model.
# If the the tolerence is below .1 that indicates a serious problem.
# Tolerence below .2 indicates a potential problem.

vif(model2)
1/vif(model2) # Tolerence
mean(vif(model2)) # Average

# It doesn't look like there is any collinearity in 
# our model.

# -------------

# Linearity of the Logit

# First we need to create the variables we will use for the test:
# We exclude rank as it is a factor and you can't take the log of a factor.

titanic$logAge <- log(titanic$Age)*titanic$Age
titanic$logFare <- log(titanic$Fare)*titanic$Fare


View(titanic)

model2_test <- glm(Survived ~ Age + Fare  + logAge + logFare,
                   data = titanic,
                   family = binomial())

summary(model2_test)

# What we are looking for here is for all interactions to have Pr(>|z|) greater than .05
# In other words we don't want them to be significant.
# Fare seems to have broken this assumption - In this case it might be better to try a different
# modeling approach - Decision Trees or Random Forests might be a good choice: 

# Trevor Stephens Tutorial: http://trevorstephens.com/kaggle-titanic-tutorial/r-part-3-decision-trees/
# Interpreting Odds Ratio: http://www.statisticssolutions.com/theres-nothing-odd-about-the-odds-ratio-interpreting-binary-logistic-regression/ 



