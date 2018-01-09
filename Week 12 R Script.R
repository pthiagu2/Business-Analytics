

# Regression

# Simple Regression - A way of prediciting an outcome variable from one predictor variable.
# Multiple Regression - A way of predicting an outcome variable from several perdictor variables.

# The equation takes the form of outcome = (model) + error (for the ith observation).

# I'll leave you to read about the math - today we're interested in understanding the assmuptions of the model
# fitting it, and assessing the goodness of that fit.

# ============================================================
# ============================================================

# Andy's Simple Regression Example:

# Load the album data file (change the directory to where this is on your own machine):


library(readr)

album_sales <- read_delim("~/To File 20170817/LIS 590 BAO/Field Data Files/Album Sales 1.dat", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)
View(album_sales)

model1 <- lm(album_sales$sales ~ album_sales$adverts)

# the ~ (tilde) means predicted from.
# To get a summary of the model use the following command:

summary(model1)

# R^2

# Starting by looking at the R-Squared (Remember this tells us how much of the variability 
# in sales can be explained by adverts)
# We find this as 'Multiple R-Squared' which has a value of .3346 - meaning our model explains
# 33.46% of the variability.

# F-Ratio

# The F-Ratio is a measure of how much the model has improved the prediction
# of the outcome compared to the level of inaccuracy in the model.
# F is 99.59 with p < .001 - meaning that there is less than a .1% chance that an F-ratio this large would
# occur if the Null Hypothesis were true (that the amount spent on adverts had no effect on sales.)

# Coefficients

# The (Intercept) is 1.34 - meaning that the model predicts that when no money is spent (the intercept is kept 
# at zero) that 134,100 albums will be sold.
# album_sales$adverts is set at .0962 This is the slope of the line and can be interpreted to be the change in 
# the outcome associated with a unit change in the predictor; meaning that everytime the advert budget is increased
# by 1 then the model predicts and increase of .096 records. (i.e. for every $1000 in advertizing we should expect 
# to sell 96 additional records).

# |t|

# This tells us the probability that the observed value of |t| would appear if the value of b in the 
# population were zero (had no effect). Here the probabilities for both are < 2e-16 so we can say that 
# the b's are different from zero and that the advertizing budget makes a significant contribution (p < .001).

# ============================================================

# Deploying the Model

# Now that we have the equation we can make informed business decisions using the model:

# album sales = 134.14 + (0.096 * advertising budget)

# How many albums should we expect to sell if we spend $1M on advertizing?

album_sales <- 134.14 + (0.096 * 1000)
album_sales * 1000

# ============================================================
# ============================================================

# Let's look at another example using our Diamonds Data set:

library(plotly)
View(diamonds)

# Let's look at carat and compare that to price.
# Price will be our dependent variable and carat will be our independent variable:

model2 <- lm(diamonds$price ~ diamonds$carat)

summary(model2)

# R^2

# Our 'Multiple R-Squared' has a value of .8493 - meaning our model explains
# 84.93% of the variability. In diamonds size really does matter!

# F-Ratio

# Remember that the F-Ratio is a measure of how much the model has improved the prediction
# of the outcome compared to the level of inaccuracy in the model.
# F is 304,100 with p < .001 - meaning that there is less than a .1% chance that an F-ratio this large would
# occur if the Null Hypothesis were true (that carat size had no effect on sale price.)

# Coefficients

# The (Intercept) is -2256.36 - meaning that the model predicts that with a carat size of 0 (the intercept is kept 
# at zero) that the price will be -$2,256. Clearly this is a limitation of the reality we are trying to describe as
# it is impossible to have a diamond with a carat size of absolute zero.


# diamonds$carat is set at 7,756.43. Remeber this is the slope of the line and can be interpreted to be the change in 
# the outcome associated with a unit change in the predictor; meaning that everytime the carat size is increased
# by 1 then the model predicts the price will increase by $7,756.43. 

# |t|

# This tells us the probability that the observed value of |t| would appear if the value of b in the 
# population were zero (had no effect). Here the probabilities for both are < 2e-16 so we can say that 
# the b's are different from zero and that carat size makes a significant contribution (p < .001).

# ============================================================

# Let's deploy our model


# diamond_price = -2256.36 + (7756.43 * carats)

# How much should we expect to spend for a 1 carat diamond?

diamond_price = -2256.36 + (7756.43 * 1)
diamond_price

# Let's check against the current market (very loose Google method)

one_carat_prices <- c(4480,	5996,	8593,	4255,	10481, 3170,	5125,	4430,	5410,	
                      5460,	5840,	6940,	6980,	6620,	4950,	6880,	4880,	3900,	4420)

mean(one_carat_prices)

# Not too bad! 

# ============================================================
# ============================================================

# Multiple Regression

# Remember that Multiple Regression is just a way of predicting an 
# outcome variable from several perdictor variables.


# Mutliple Regression is a parametric and non-robust method.
# In order to work properly several assumptions have to be met by the data.
# It is a good place to start and is a proven work horse but may not be the best
# solution for every situation.

# Assumptions:

# Variable Types: All predictors should be quantitative or categorical (With two categories - R will do this automatically)
# Non-zero Variance : The predictors should have some variation in value.
# No perfect mulicollinearity : There should be no perfect linear relationship between two or more predictors.
# Predictors are uncorrelated with external variables: There should be no external variables that correlate with
#   with any of the variables included in the regression model.
# Homoscedasticity: residuals at each level of the predictor should have the same variance. Where this is not the case,
#   we say that heteroscedasticity exists.
# Independent Errors; For any two observations, the residual terms should be uncorrelated.
# Normally Distributed Errors: It is assumed that the residuals in the model are random, normally distributed variables
#   with a mean of zero.
# Independence : It is assumed that all values of the outcome variable are independent. 
# Linearity : The mean value of the outcome variable for each increment of the predictor(s) lie along a straight line.

# When all of these assumptions are met the model can be accurately applied to the population of interest.

# Let's first look at Andy's example to cover the basics:

album_sales2 <- read_delim("~/To File 20170817/LIS 590 BAO/Field Data Files/Album Sales 2.dat", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)
View(album_sales2)

model3 <- lm(sales ~ adverts + airplay + attract, data = album_sales2)

summary(model3)

# R^2

# Our 'Multiple R-Squared' has a value of .6647 - meaning our model explains
# 66.47% of the variability. 

# Adjusted R^2

# The Adjusted R-Squared is a measure indicating how well the model generalizes to a population.
# We want it to be close to or the same as the multiple R-squared. Here it is close. 

# Stein's Formula 

# This can be used to see how well R^2 changes according to different sample sizes:

n <- 200     # The number of participants.
r <- .665    # The un-adjusted or multiple R^2.
k <- 3       # The number of predictors in the model.

adjusted_r <- 1 - ((((n-1)/(n-k-1)) * ((n-2)/(n-k-2)) * ((n+1)/(n))) * (1 - r))
adjusted_r

# The value is very close to our observed R^2 indicating that our model has good cross validity.
# meaning that it genearlizes well.

# Coefficients or b values

# Remember that positive or negative values denote positive or negative relationships between the predictors
# and album sales. The coefficients show the degree to which each predictor affects the outcome of the effects of 
# all the other predictors are held constant. 

#   Advertising: (b = .085) as the advertising budget increases by one unit, album sales increase by .085 units. 
#     Remember our unit of measure is 1000 so; for every $1000 spent on advertising, 85 albums are sold.

#   Airplay: (b = 3.367) Here we are looking at the number of plays on the radio compared to sales. Every 
#     additional play of a song on the radio leads to an additional 3367 albums being sold.

#   Attractiveness: (b = 11.086) This is a measure of how attractive the band is. Every one unit increase in 
#     band attractiveness leads to an additional 11,086 albums being sold.

# If you were a record company executive what action would you take to increase sales?


# |t|

# In multiple regression this value tells us whether the predictor is making a signficant contribution to the model.
# If the the t-test is signficant then the predictor is making a significant contribution to the model. The smaller the
# value of Pr and the larger the value of t the greater the contribution of the predictor.

# Which is the least impactful predictor of record sales?

# Standardized Betas

# standarized betas show the number of standard deviations by which the outcome will change as a result of one
# standard deviation change in the predictor. They are not dependent on units of measure and are directly comparable
# to each other. This helps us know the importance of the predictors in relation to each other.

# install.packages('QuantPsyc')

library(QuantPsyc)

lm.beta(model3)

# adverts and airplay are pretty identical so this confirms what the t-statistics reported.

# Let's look at the standard deviation for advertizing budget and sales:

sd_adverts <- sd(album_sales2$adverts)
sd_sales <- sd(album_sales2$sales)

sd_adverts
sd_sales

# This can be read as $485,655 (remember we're dealing with units of 1000) and 80,699. 

# Let's multiply the sd for sales by the standardized beta for adverts:

(sd_sales * 1000) * .511

# Meaning that for every $485,655 spent, an additional 41,240 records are sold.

# Confidence Intervals

# The important thing to remember here is that you want a small confidence interval - meaning
# that this model represents the population well and is representative of the results you 
# would expect to see from other samples from the same population. The positive and negative
# relfect the direction of the relationship.

# If your confidence interval crosses zero and has both a postive and negative number you
# probably have a bad model.

confint(model3)

# Outliers and Influential Cases

# Models can be influence by outliers or other cases (rows) that have unbalanced influence on the model.
# Andy does a good job of discussing how to put the different test statistics into a dataframe and how to identify
# cases exerting undue influence (pages 288-292). 

# Durbin Watson

# Here we are looking at the assumption of Independence. You want a DWT close to 2 anything less than 1 or greater than
# 3 suggests this assumption has not been met.

library(car)
durbinWatsonTest(model3)

# Here we are close to 2 and the p-value is not significant - this assumption has been met.

# MultiColinearity

# If the largest VIF is greater than 10 then there is a problem.
# If the average VIF is substantially greater than 1 then there may be bias in the model.
# If the the tolerence is below .1 that indicates a serious problem.
# Tolerence below .2 indicates a potential problem.

vif(model3)
1/vif(model3) # Tolerence
mean(vif(model3)) # Average

# All of our tests for the assumption of no multicollinearity check out.

# Check Residual Assumptions

# Here we are looking to ensure that the assumptions of random errors and homoscedasticity
# have been met.

plot(model3)

# We are looking for the second plot 'Residuals vs Fitted'; we want to see a random distribution.
# This is indicative of the fact that the assumptions of linearity, homoscedasticity, and randomness have been met.
# Here we are good.

# The third plot is the Normal Q-Q plot. We want to see the dots line up along the diagonal line (which represents a
# normal distribution.) Again, things line up pretty well here so no need to worry.

# We can also check that the residuals don't deviate from a normal distribution. This is done by generating a 
# histogram:

hist(rstudent(model3))

# Again, we have a roughly normal distribution so we can say that our data meet the assumption of normality.

# What if I violate the assumptions of linearity or otherwise?
# We can attempt a robust regression using bootstrapping.
# Andy gives a good example of this so I won't replicate it here.

# Let's deploy the model:

adverts = 100 # remember this is units of $1000
airplay = 500
attract = 10

sales <- (-26.6 + (.085 * adverts) + (3.37 * airplay) + (11.08 * attract))
sales*1000 # multiply by 1000 again because our base unit is 1000

# ============================================================
# ============================================================

# Affairs

library(readr)
affairs <- read_csv("~/To File 20170817/LIS 590 BAO/Slides/Week 12 - R Script/Week 12 - R Script/affairs.csv")
View(affairs)

model4 <- lm(nbaffairs ~ age + ym + religious + education + occupation + rate, data = affairs)
summary(model4)

# R^2

# Our 'Multiple R-Squared' has a value of .1315 - meaning our model explains
# 13.15% of the variability. Clearly this is a complex issue.

# Adjusted R^2

# The Adjusted R-Squared is a measure indicating how well the model generalizes to a population.
# We want it to be close to or the same as the multiple R-squared. Here it is close. 


# Coefficients or b values

# Remember that positive or negative values denote positive or negative relationships between the predictors
# and album sales. The coefficients show the degree to which each predictor affects the outcome of the effects of 
# all the other predictors are held constant. 

#   age: (b = -.05) As a person's age increases by one year, the number of likely affairs decreases by .05 
#     This has a small standard error and a small t value but it is significant at 
#     p > .01.

#   ym: (b = .16) As the number of years a person is married increases by 1, the number of likely affairs
#     increases by .16. (1 affair in 6.25 years). This has a large t value and is significant.

#   religious: (b = -.48) As a person's rating of religiousity increases by one, the number of likely affairs
#     decreases by .47. Again this has a relatively large t value and is significant.

#   education & occupation are not significant and can be ignored - meaning the level of one's education and their
#     occupation have no bearing on whether or not they are more or less likely to have an affair.

#   rate: (b = -.708) As a person's happiness in marriage rating increases by 1 the number of likely affairs
#     decreases by .7. This also has a large t value and is significant.

# At this point I generally clean up the model:

model5 <- lm(nbaffairs ~ age + ym + religious + rate, data = affairs)
summary(model5)


# |t|

# By looking at the t-values, the way in which a person rates the happiness of their marriage has the largest impact
# on the number of likely affairs they are to have had, followed closely by how religious they regard themselves to be
# and the number of years they have been married. Finally how old someone is also seems to have some impact, but to a 
# lesser degree.

# Standardized Betas

# standarized betas show the number of standard deviations by which the outcome will change as a result of one
# standard deviation change in the predictor. They are not dependent on units of measure and are directly comparable
# to each other. This helps us know the importance of the predictors in relation to each other.

# install.packages('QuantPsyc')

library(QuantPsyc)

lm.beta(model5)

# What stands out is the impact of ym and rating - before regligious and ym seemed pretty close as to their
# influence, but now it looks like ym and the rating of happiness seem most impactful

sd_ym <- sd(affairs$ym)
sd_rate <- sd(affairs$rate)
sd_nbaffairs <- sd(affairs$nbaffairs)

sd_ym
sd_rate
sd_nbaffairs

# Let's multiply the sd for nbaffairs by the standardized beta for ym:

sd_nbaffairs * .262

# Meaning that for every 5.5 years married, the number of likely affairs increases by .86.

# likewise, for every increase of roughly one in a persons rating of their marriage, the 
# number of likely affairs decreases by:

sd_nbaffairs * -.236

# Roughly .7 - It's worthwhile to make sure you maintain a happy marriage!

# Confidence Intervals

# Remember - the important thing to remember here is that you want a small confidence interval - meaning
# that this model represents the population well and is representative of the results you 
# would expect to see from other samples from the same population. The positive and negative
# relfect the direction of the relationship.

# If your confidence interval crosses zero and has both a postive and negative number you
# probably have a bad model.

confint(model5)

# Our intervals are tight and none of them cross zero. Phew!

# Outliers

# Add variables to our dataframe for the test statistics:

affairs$residual <- resid(model5)
affairs$standardized.residual <- rstandard(model5)
affairs$studentized.residual <- rstudent(model5)
affairs$cooks.distance <- cooks.distance(model5)
affairs$dfbeta <- dfbeta(model5)
affairs$dffit <- dffits(model5)
affairs$leverage <- hatvalues(model5)
affairs$covariance.ratios <- covratio(model5)

View(affairs)

# we want 95% of our cases to have standardized residuals within plus or minus 2, in a set of 601 cases we'd expect 
# roughly 571 to be around 2, or roughly 30 cases outside the limit. Let's see"

affairs$standardized.residual > 2 | affairs$standardized.residual < -2

# We want to store this data in our dataframe:

affairs$large.residual <- affairs$standardized.residual > 2 | affairs$standardized.residual < -2

View(affairs)

sum(affairs$large.residual)

affairs[affairs$large.residual,c("age","ym","religious","rate","nbaffairs","standardized.residual")]

# Those over 3 merit attention. 

# If we look at those we see these are individuals who have either rated themselves as being very religious or
# as having happy marriages but who also report a high number of affairs - almost one a month for the last
# 12 months. If I were to take this further, I would want to investigate each of these cases as there may be an
# additional variable that we are missing that would explain their behavior.

# I'll let you read Andy on the rest of these outlier and influential cases metrics, suffice it to say there
# are some things we need to look at in this model:

affairs[affairs$large.residual, c("cooks.distance","leverage","covariance.ratios")]

# Durbin Watson

# Here we are looking at the assumption of Independence. You want a DWT close to 2 anything less than 1 or greater than
# 3 suggests this assumption has not been met.

library(car)
durbinWatsonTest(model5)

# Here we are close to 1 and the p-value is significant - so this assumption has not been met.

# MultiColinearity

# If the largest VIF is greater than 10 then there is a problem.
# If the average VIF is substantially greater than 1 then there may be bias in the model.
# If the the tolerence is below .1 that indicates a serious problem.
# Tolerence below .2 indicates a potential problem.

vif(model5)
1/vif(model5) # Tolerence
mean(vif(model5)) # Average

# All of our tests for the assumption of no multicollinearity check out.

# Check Residual Assumptions

# Here we are looking to ensure that the assumptions of random errors and homoscedasticity
# have been met.

plot(model5)

# We are looking for the second plot 'Residuals vs Fitted'; we want to see a random distribution.
# This is indicative of the fact that the assumptions of linearity, homoscedasticity, and randomness have been met.
# Here we do not see a random plot so our assumptions have not been met.

# The third plot is the Normal Q-Q plot. We want to see the dots line up along the diagonal line (which represents a
# normal distribution.) Again, things don't line up well here indicating potential skew in the distribution.

# We can also check that the residuals don't deviate from a normal distribution. This is done by generating a 
# histogram:

hist(rstudent(model5))

# Again, we have skew in our distribution so we can say that our data does not meet the assumption of normality.

# Again, What if I violate the assumptions of linearity or otherwise?
# We can attempt a robust regression using bootstrapping, or we can try a different robust method like logisitic regression.

# Here we haven't met several assumptions so we cannot say that this model will generalize to any person. 
# Again - this is a complex behavior and we probably are missing some variables.






