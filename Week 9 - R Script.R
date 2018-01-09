
# Week 9 - R Script: Assumptions & Parametric Statistics

# The basic stats we are covering in this class and which
# Field addresses in his book are called parametric stats.

# Parametric stats operate on a set of assumptions about the
# data being used to build them:

#   1. Normal Distribution - The assumption that 
#       either the errors or the sampling 
#       distribution are normally distributed.

#   2. Homogeneity of Variance - The idea that
#       variance should be the same throughout the
#       the data.

#   3. Intervals - A continuous variable where equal 
#       intervals on the scale represent equal differences
#       in the property being measured.

#   4. Indepedence - principally means that 
#       the behavior of our test subjects are 
#       not influenced by one another.

# =======================================================

# The Assumption of Normality:

# Import Field's data file

library(readr)
data1 <- read_delim("~/To File 20170817/LIS 590 BAO/Field Data Files/DownloadFestival(No Outlier).dat", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)
View(data1)

library(ggplot2)

hist.day1 <- ggplot(data1, aes(day1)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  labs(x = "Hygiene score on day 1", y = "Density")

hist.day1
            
# Andy adds a normal curve using the standard deviation 
# found in the data:

hist.day1 + stat_function(fun = dnorm, args = list(mean = mean(data1$day1, na.rm = TRUE),
                                                   sd = sd(data1$day1, na.rm = TRUE)), colour = "black", size = 1)

# We can also do this using a QQ plot (quantile-quantile plot):

# A QQ plot plots the cumulative values in the data against
# The cumulative values of a particular distribution (in this case a normal distribution).

# we are looking for a diagonal line which would indicated a normal
# distribution.

qqplot.day1 <- qplot(sample = data1$day1, stat = "qq")

qqplot.day1

# =======================================================

# Let's look at some of our other datasets used thus far:

View(diamonds)

hist.carat <- ggplot(diamonds, aes(carat)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "Carat Weight", y = "Density")

hist.carat

# Let's add a normal curve:

hist.carat + stat_function(fun = dnorm, args = list(mean = mean(diamonds$carat, na.rm = TRUE),
                                                   sd = sd(diamonds$carat, na.rm = TRUE)), colour = "black", size = 1)

# What we have here is a positively skewed distribution. 
# Let's check this observation with a QQ Plot:

qqplot.carat <- qplot(sample = diamonds$carat, stat = "qq")
qqplot.carat

# You'll notice that we do not have a nice diagonal plot 
# We can be confident that we don't have a normal distribution.

# Let's see what this looks like for a width(y):


hist.width <- ggplot(diamonds, aes(y)) +
  geom_histogram(aes(y = ..density..), binwidth = .5, colour = "black", fill = "white") +
  labs(x = "Width", y = "Density") +
  scale_x_continuous(breaks = round(seq(min(diamonds$y), max(diamonds$y), by = 2),0)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(diamonds$y, na.rm = TRUE),sd = sd(diamonds$y, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.width

# Again, let's add a QQ plot of width:

qqplot.width <- qplot(sample = diamonds$y, stat = "qq")
qqplot.width

# Again, you'll notice that we don't have a nice diagonal line - meaning that this too
# is probably not a normal distribution. 

# =======================================================

# We can also quantify - as Field explains - the shape of a distribution so that we
# don't have to rely soley on visualizations:

# install.packages("psych")

library(psych)

describe(data1$day1)

# install.packages("pastecs")

library(pastecs)

stat.desc(data1$day1, basic = FALSE, norm = TRUE)

# Remember, in a normal distribution the values of skew and kurtosis should be 0. 

# SKEW
# Positive values of skew indicate a pile up on the left, and negative indicate a pile
# up on the right.

# KURTOSIS 

# Positive values of kurtosis indicate a pointy and heavy tailed distribution. Negative
# values indicate a flat and light-tailed distribution. Again, the further the value
# is from 0 the less likely it is normally distributed.

# We can convert these values to z-scores to determine if either the skew 
# or kurtosis are significant. 

# Remember, in Z-Scores significance is as follows:

#   - Absolute value > 1.96 is significant at p < .05 
#   - Absolute Value > 2.58 is significant at p < .01
#   - Absolute Value > 3.29 is significant at p < .001

# A rule of thumb: 
# For large samples the criterion should be set to 2.58
# For very large samples (>= 200 cases) it is best just to look at
# the visualisations as large samples are subject to give significant
# values for even small deviations.

# skew.2SE and kurt.2SE already give us our z-scores with
# the following values:

#   - Absolute value > 1 is significant at p < .05
#   - Absolute value > 1.29 is significant at p < .01
#   - Absolute value > 1.65 is significant at p < .001

stat.desc(data1$day1, basic = FALSE, norm = TRUE)

# For day 1: 

#   skew.2SE = -0.025 
#   kurt.2SE = -1.22

# No significant skew but significant kurtosis; though this is a large
# sample so the charts are a better indicator.

# Let's look at diamond weight:
# We will first need to reduce our sample size as stat.desc can only
# handle up to 5,000 cases:

diamonds1 <- diamonds[sample(nrow(diamonds), 500), ]

stat.desc(diamonds1$y, basic = FALSE, norm = TRUE)

# We have significant skew and kurtosis at the p < .001 level
# for both.

# Since our sample is rather large still, let's double check the charts:


hist.width1 <- ggplot(diamonds1, aes(y)) +
  geom_histogram(aes(y = ..density..), binwidth = .5, colour = "black", fill = "white") +
  labs(x = "Width", y = "Density") +
  scale_x_continuous(breaks = round(seq(min(diamonds$y), max(diamonds$y), by = 2),0)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(diamonds$y, na.rm = TRUE),sd = sd(diamonds$y, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.width1

# Again, let's add a QQ plot of width:

qqplot.width1 <- qplot(sample = diamonds1$y, stat = "qq")
qqplot.width1


# =======================================================

# Examples borrowed from Sean Kross for the sake of illustration: http://seankross.com/ 

# ================

# NORMAL DISTRIBUTION

# Draw two plots next to each other : https://www.statmethods.net/advgraphs/layout.html 
par(mfrow = c(1, 2))

# normal_density are the y-values for the normal curve
# zs are the x-values for the normal curve
n <- 100
normal_density <- dnorm(seq(-4, 4, 0.01))
zs <- seq(-4, 4, 0.01)

# Add some spice to the default histogram function
hist_ <- function(x, ...){
  hist(x, breaks = 30, xlab = "Z", ylab = "",  yaxt='n', freq = FALSE, ...)
  lines(zs, normal_density, type = "l", col = "red", lwd = 2)
}

# Gaussian Normal
# rnorm() generates random numbers from a normal distribution
# gaussian_rv is the dataset that will be compared to the Gaussian distribution
gaussian_rv <- rnorm(n)

# Draw the histogram
hist_(gaussian_rv, main = "Gaussian Distribution")

# Draw the Q-Q plot
qqnorm(gaussian_rv)
qqline(gaussian_rv, col = "blue", lwd = 2)

# Quantify skew and Kurtosis:
stat.desc(gaussian_rv, basic = FALSE, norm = TRUE)

# ================

# SKEWED DISTRIBUTIONS


# Skewed Right
# skew_right is the dataset that will be compared to the Gaussian distribution
skew_right <- c(gaussian_rv[gaussian_rv > 0] * 2.5, gaussian_rv)

hist_(skew_right, main = "Skewed Right - Positive Skew", ylim = c(0, max(normal_density)))

qqnorm(skew_right)
qqline(skew_right, col = "blue", lwd = 2)

stat.desc(skew_right, basic = FALSE, norm = TRUE)

# Skewed Left
# skew_left is the dataset that will be compared to the Gaussian distribution
skew_left <- c(gaussian_rv[gaussian_rv < 0]*2.5, gaussian_rv)

hist_(skew_left, main = "Skewed Left - Negative Skew", ylim = c(0, max(normal_density)))

qqnorm(skew_left)
qqline(skew_left, col = "blue", lwd = 2)

stat.desc(skew_left, basic = FALSE, norm = TRUE)

# ================

# KURTOSIS


# Fat Tails
fat_tails <- c(gaussian_rv*2.5, gaussian_rv)

hist_(fat_tails, main = "Fat Tails", ylim = c(0, max(normal_density)), xlim = c(-10, 10))

qqnorm(fat_tails)
qqline(fat_tails, col = "blue", lwd = 2)

stat.desc(fat_tails, basic = FALSE, norm = TRUE)

# Thin Tails
thin_tails <- rnorm(n, sd = .7)

hist_(thin_tails, main = "Thin Tails")

qqnorm(thin_tails)
qqline(thin_tails, col = "blue", lwd = 2)

stat.desc(thin_tails, basic = FALSE, norm = TRUE)


# =======================================================


# EXPLORING GROUPS OF DATA

# Let's go back to our diamond data and look at price:


hist.price <- ggplot(diamonds1, aes(diamonds1$price)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, colour = "black", fill = "white") +
  labs(x = "Price", y = "Density") +
  scale_x_continuous(breaks = round(seq(min(diamonds1$price), max(diamonds1$price), by = 1000),0)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(diamonds1$price, na.rm = TRUE),sd = sd(diamonds1$price, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.price

qqplot.price <- qplot(sample = diamonds1$price, stat = "qq")
qqplot.price

stat.desc(diamonds1$price, basic = FALSE, norm = TRUE)

# Clearly we have a skewed distribution with significant kurtosis with a flat, light-tailed distribution.
# We can break this apart and look at the distribution by a factor - for example, if we wanted to 
# see the individual price distributions by cut we can use the by() function:

# but first, a quick word on by() and subset()

# by() allows you to get multiple stats for each data point in a sample:

by(data = diamonds1, INDICES = diamonds1$cut, FUN = describe)

by(data = diamonds1, INDICES = diamonds1$cut, FUN = stat.desc, basic = FALSE, norm = TRUE)

# or for just specific attributes:

by(diamonds1[, c("price","y")], diamonds1$cut, stat.desc, basic = FALSE, norm = TRUE)

# subset allows you to create historgrams from new datasets based on factors, let's look
# at Ideal vs Fair cuts:

fair_data <- subset(diamonds1, diamonds1$cut == "Very Good")
ideal_data <- subset(diamonds1, diamonds1$cut == "Ideal")

hist.price_fair <- ggplot(fair_data, aes(fair_data$price)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, colour = "black", fill = "white") +
  labs(x = "Price", y = "Density") +
  scale_x_continuous(breaks = round(seq(min(fair_data$price), max(fair_data$price), by = 1000),0)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(fair_data$price, na.rm = TRUE),sd = sd(fair_data$price, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.price_fair

hist.price_ideal <- ggplot(ideal_data, aes(ideal_data$price)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, colour = "black", fill = "white") +
  labs(x = "Price", y = "Density") +
  scale_x_continuous(breaks = round(seq(min(ideal_data$price), max(ideal_data$price), by = 1000),0)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ideal_data$price, na.rm = TRUE),sd = sd(ideal_data$price, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.price_ideal

# It's obcious that neither of these are normal distributions, but just to be sure
# we could also use a Shaprio-Wilkes test as follows:

library(stats)

shapiro.test(fair_data$price)

# Notice our p-value is highly significant inidicating that the distribution is not normal.


# =======================================================

# HOMOGENETIY OF VARIANCE:

# homogeneity is the idea of having very little variance
# between levels of a variable.

# install.packages("car")

library(car)

# Using Levene's test we can check for homogeneity:

leveneTest(diamonds1$price, diamonds1$cut)

# Here the pr(>f) is > .05

# Levene's test can be influenced by large sample sizes.

# =======================================================
# =======================================================

#  HOW TO FIX FIX NON-NORMALITY AND UNEQUAL VARIANCES

#  Since this is an introductory course we won't talk much about it apart
#  from a quick demonstration of a log transformation - this is useful
#  because it helps bring the tails down for skewed distributions. 

#  Log away that outliers can cause error in parametric tests - we'll talk
#  more about these when we get to regression.


hist.price_ideal <- ggplot(ideal_data, aes(ideal_data$price)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, colour = "black", fill = "white") +
  labs(x = "Price", y = "Density") +
  scale_x_continuous(breaks = round(seq(min(ideal_data$price), max(ideal_data$price), by = 1000),0)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ideal_data$price, na.rm = TRUE),sd = sd(ideal_data$price, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.price_ideal

qqplot.price <- qplot(sample = ideal_data$price, stat = "qq")
qqplot.price


# We can log transform the data:

ideal_data$logprice <- log(ideal_data$price+1)
View(ideal_data)

# Now let's plot it:

hist.price_ideal <- ggplot(ideal_data, aes(ideal_data$logprice)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, colour = "black", fill = "white") +
  labs(x = "Price", y = "Density") +
  scale_x_continuous(breaks = round(seq(min(ideal_data$logprice), max(ideal_data$logprice), by = 1000),0)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ideal_data$logprice, na.rm = TRUE),sd = sd(ideal_data$logprice, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.price_ideal

qqplot.price <- qplot(sample = ideal_data$logprice, stat = "qq")
qqplot.price

stat.desc(ideal_data$logprice, basic = FALSE, norm = TRUE)
shapiro.test(ideal_data$logprice)


# Notice the distribution is a bit better, but it is still not normal. 
# Transformations come at a cost - you are better of trying to see if there is a more
# robust method when this is the case.

# =============================================================================

# Let's try it again on another data point, this time using carat:


hist.carat <- ggplot(diamonds1, aes(carat)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "Carat Weight", y = "Density")

hist.carat

# Let's log transform the variable:

diamonds1$logcarat <- log(diamonds1$carat + 1)
# View(diamonds)

# Let's plot our new variable:

hist.carat_log <- ggplot(diamonds1, aes(diamonds1$logcarat)) +
  geom_histogram(aes(y = ..density..), binwidth = .09, colour = "black", fill = "white") +
  labs(x = "Price", y = "Density") +
  scale_x_continuous(breaks = round(seq(min(diamonds1$logcarat), max(diamonds1$logcarat), by = 1000),0)) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(diamonds1$logcarat, na.rm = TRUE),sd = sd(diamonds1$logcarat, na.rm = TRUE)), 
                colour = "black", size = 1)
hist.carat_log

qqplot.price <- qplot(sample = diamonds1$logcarat, stat = "qq")
qqplot.price

stat.desc(diamonds1$logcarat, basic = FALSE, norm = TRUE)
shapiro.test(diamonds1$logcarat)



