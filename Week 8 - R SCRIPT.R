
# Week 8 - Exporing Data with Graphs in R

# ============================================

# A quick word on visualizing data:

# http://viz.wtf/ 

# https://www.buzzfeed.com/peteraldhous/spies-in-the-skies?utm_term=.vv5VKbX3B2#.tueBx4QKP9 

# ============================================

# Datasets for this script are saved to this weeks
# Moodle module.

# Most of the data used in today's lecture
# was sourced from the following:

# Source : Fair, R. (1977) “A note on the computation of the tobit estimator”, Econometrica, 45, 1723-1727. 
# http://cowles.yale.edu/sites/default/files/files/pub/d04/d0434.pdf

# VARIABLE DESCRIPTIONS:

# sex - A FACTOR WITH TWO LEVELS (M,F)
# age - AGE
# ym - number of years married
# child - yes/no
# religious - how religious, from 1 to 5.
# education - level of education in years
# occupation - occupation from 1 to 7 - based on Hollingshead: https://dictionary.fitbir.nih.gov/portal/publicData/dataElementAction!view.action?dataElementName=HollingsheadJobClassCat&publicArea=true 
#   1 - Farm Laborer
#   2 - Unskilled worker
#   3 - Machine Operator
#   4 - Skilled Manual worker
#   5 - Clerical/Sales
#   6 - Technician
#   7 - Small Business Owner
# rate - self rating of marriage; from 1 very unhappy to 5 very happy
# nbaffairs - number of affairs in past year


# A good ggplot resource:
# http://ggplot2.tidyverse.org/index.html

# ============================================

# Field uses ggplot2 alot. If you haven't already
# downloaded it, be sure to do so in order to follow
# along.

#install.packages("ggplot2")

# Remember in R that to use a package we
# need to call it using the library() function:

library(ggplot2)

# Let's also pull in some data to play with:

library(readr)

affairs <- read_csv("~/To File 20170817/LIS 590 BAO/Slides/Week 8 - R SCRIPT/affairs.csv")
View(affairs)

# This dataset from 1978 contains information about politicians having affairs. 
# The goal of the dataset is to find characteristics that make it more plausable 
# for a politician to have an affair. Does the number of children matter? 
# How religious the politician is? Or education/marriage rating?

# How large is this dataset?

# Let's check our columns and see what we have to work with:

names(affairs)

# Now let's explore this data!

# =======================================================================================

# Let's compare a politician's age to their years of marriage:

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym)) 
graph1 + geom_point()


# Let's compare that to the plain old plot function:

plot(affairs$age, affairs$ym)

# Let's add some color to it:

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym, colour = affairs$sex)) 
graph1 + geom_point()

# obviously these are not all 601 observations
# There is some overlap (overplotting) - we can highlight this using 
# geom_jitter 

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym, colour = affairs$sex)) 
graph1 + geom_point() + geom_jitter()

# Let's add a title to it:

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym,
                              colour = affairs$sex)) + 
  ggtitle("Years Married by Age")
graph1 + geom_point() + geom_jitter()

# Did you try opts() and get an error when trying to add a title to your plots?
# Did you try and find it using help()?

help(opts)

# opts() has been deprecated - meaning it has been superceded with another feature
# in this case - ggtitle(). 

# R packages are dynamic - this can cause problems with legacy code.

# Let's center the title - ggtitle defaults left to accomodate subtitles:

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym,
                              colour = affairs$sex)) + 
  ggtitle("Years Married by Age") +
  theme(plot.title = element_text(hjust = 0.5))

graph1 + geom_point() + geom_jitter()

# Suppose we want a subtitle - let's move the title back left
# and add a subtitle. We can use labs() to add both a title
# and a subtitle and change the legend title:

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym,
                              colour = affairs$sex)) + 
  labs(title = "Years Married by Age", 
       subtitle = "Political Affairs Dataset",
       x = "Age",
       y = "Years Married",
       color = "Sex") 

graph1 + geom_point() + geom_jitter()


# The axes are not very helpful as is - let's adjust those to make
# the visualization more readable:

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym,
                              colour = affairs$sex)) + 
  labs(title = "Years Married by Age", 
       subtitle = "Political Affairs Dataset",
       x = "Age",
       y = "Years Married",
       color = "Sex") +
  scale_x_continuous(breaks = round(seq(min(affairs$age), max(affairs$age), by = 2),0)) +
  scale_y_continuous(breaks = round(seq(min(affairs$ym), max(affairs$ym), by = 2),0))
graph1 + geom_point() + geom_jitter()

# Let's see if this chart can give us any quick insights into our question - 
# Can we predict when a politician is likely to have an affair?

# Something simple we can do at this stage is change our color legend:

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym,
                              colour = affairs$nbaffairs)) + 
  labs(title = "Years Married by Age", 
       subtitle = "Political Affairs Dataset",
       x = "Age",
       y = "Years Married",
       color = "Number of Affairs") +
  scale_x_continuous(breaks = round(seq(min(affairs$age), max(affairs$age), by = 2),0)) +
  scale_y_continuous(breaks = round(seq(min(affairs$ym), max(affairs$ym), by = 2),0))
graph1 + geom_point() + geom_jitter()

# This doesn't tell us much yet, but seems to suggest a correlation
# between age and years married and the number of affairs. 

# Let's add a line and see if there is a correlation between age and years 
# married:

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym,
                              colour = affairs$nbaffairs)) + 
  labs(title = "Years Married by Age", 
       subtitle = "Political Affairs Dataset",
       x = "Age",
       y = "Years Married",
       color = "Number of Affairs\n") + #notice the new-line character to add a space.
  scale_x_continuous(breaks = round(seq(min(affairs$age), max(affairs$age), by = 2),0)) +
  scale_y_continuous(breaks = round(seq(min(affairs$ym), max(affairs$ym), by = 2),0)) +
  geom_smooth(method='lm')
graph1 + geom_point() + geom_jitter()

cor.test(affairs$age, affairs$ym, method = "pearson", conf.level = 0.95)

# Two interesting data points - our p-value is < 2.23e-16
# or p-value < .00000000000000022
# The p-value shows us the probability of finding the observed
# when the null hypothesis is true - in this case there is 
# a less than 2.23e-16 probability that we would see these data
# if the null hypothesis were true.

# A Pearson's R of .78 shows that there is a strong positive
# relationship between years of age and years of marriage - 
# so when one goes up, so does the other. 

# We can add this to our chart by using annotate()

corr <- toString(round(cor(affairs$age, affairs$ym),2))

graph1 <- ggplot(affairs, aes(affairs$age, affairs$ym,
                              colour = affairs$nbaffairs)) + 
  labs(title = "Years Married by Age", 
       subtitle = "Political Affairs Dataset",
       x = "Age",
       y = "Years Married",
       color = "Number of Affairs\n") +
  scale_x_continuous(breaks = round(seq(min(affairs$age), max(affairs$age), by = 2),0)) +
  scale_y_continuous(breaks = round(seq(min(affairs$ym), max(affairs$ym), by = 2),0)) +
  geom_smooth(method='lm') +
  annotate("text", x=54, y=3, label = paste("r =",corr)) +
  annotate("text", x=54, y=1.5, label = "p-value < 2.23e-16")

graph1 + geom_point()  + geom_jitter()

# =======================================================================================

# Let's play around with some other ggplot types.
# This is how we might do a histogram for age:

names(affairs)

graph2 <- ggplot(affairs, aes(affairs$age))
graph2 + geom_histogram(binwidth = 1.5)

# This isn't a very attractive histogram - let's
# see if we can make this more eye-catching. 

# A good resource for colors in R:
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Let's grab some stats to use later:

sd_age <- toString(round(sd(affairs$age),2))
mean_age <- toString(round(mean(affairs$age),2))

# Let's use a bit of pink to spite Andy:

graph2 <- ggplot(affairs, aes(affairs$age))
graph2 + geom_histogram(binwidth = 4.75, fill="mistyrose1") +     
  scale_x_continuous(breaks = round(seq(min(affairs$age), max(affairs$age), by = 2),0)) +
  geom_density(aes(y=8*..count..), color="mistyrose3", fill = "blue", alpha = .1, adjust=5) +
  theme_classic() +
  theme(axis.text.x=element_text(colour="mistyrose4")) +
  theme(axis.text.y=element_text(colour="mistyrose4")) +
  theme(axis.title.x=element_text(colour="mistyrose4")) +
  theme(axis.title.y=element_text(colour="mistyrose4")) +
  theme(axis.line.x=element_line(colour="mistyrose4")) +
  theme(axis.line.y=element_line(colour="mistyrose4")) +
  theme(plot.title=element_text(colour="mistyrose4")) +
  theme(plot.subtitle=element_text(colour="mistyrose4")) +
  geom_vline(aes(xintercept=mean(affairs$age)),
             color="mistyrose4", linetype="dashed", size=1) +
  labs(title = "Distribution of Age", 
       subtitle = "Political Affairs Dataset",
       x = "Age",
       y = "Number of Politicians",
       colour = "mistyrose4") +
  annotate("text", x=54, y=160, label = paste("Right Skewed Distribution"), color="mistyrose4") +
  annotate("text", x=54, y=150, label = paste("s = ",sd_age), color="mistyrose4") +
  annotate("text", x=54, y=140, label = paste("mean = ",mean_age), color="mistyrose4") 

# After putting all of that effort into building a plot, let's be sure to save it:

ggsave("Age Distribution.png")
ggsave("Age Distribution.pdf")

# Or, you can export it as a PDF directly from the viewer.

# =======================================================================================


# Let's build a box and whisker chart.
# To help in this lets build out some factors:

names(affairs)

affairs$Rating_Factor <- factor(affairs$rate, levels = (1:5),
                                labels = c("Very Unhappy","Unhappy","Neutral","Happy","Very Happy"))

# Let's plot it:

graph3 <- ggplot(affairs, aes(affairs$Rating_Factor, affairs$nbaffairs))
graph3 + geom_boxplot() 

# What's happening here?
# We can use geom_jitter to get a clearer picture:

graph3 <- ggplot(affairs, aes(affairs$Rating_Factor, affairs$nbaffairs))
graph3 + geom_boxplot() + geom_jitter(width = .1, size = 1) 

# jitter adds random noise to each data point to prevent overplotting - 
# here it helps us get an additional visual sense of each boxes distribution.

# It would appear that the majority of the sample are either
# happy or very happy, and that proportionally, happy and
# very happy people have fewer affairs - so much so that affairs
# for these groups appear to be outlier activity.

# Let's look at this data another way

graph4 <- ggplot(affairs, aes(x=affairs$Rating_Factor, y=affairs$nbaffairs))
graph4 + geom_bar(stat="identity")

# This tells us about the number of affairs each group had, but doesn't
# Tell us much about the size of the group.

# Let's try this again with a histogram:

graph4 <- ggplot(affairs, aes(x=affairs$nbaffairs, fill=affairs$Rating_Factor))
graph4 + geom_histogram(binwidth = 1) +
  scale_y_continuous(breaks=seq(0,700,20)) +
  scale_x_continuous(breaks=seq(0,20,1)) 

# This shows us that the vast majority of the sample have not had an affair
# and of those that rated themselves unhappy or very unhappy - a larger
# amount have had multiple affairs.

# =======================================================================================

# Suppose we wanted to see some simple statistics about our dataset and 
# graph them - like the mean number of affairs by gender:

# We could accomplish this by using a bar chart and stat_summary()


graph5 <- ggplot(affairs, aes(sex, nbaffairs))

graph5 + stat_summary(fun.y=mean, geom = "bar", fill = "white", colour = "black") 

# They seem to be pretty close between the sexes - let's fit an error bar
# to see what we're working with:

# If you were following along in your book you will
# have needed to install "Hmisc" already:

#install.packages("Hmisc")

graph5 <- ggplot(affairs, aes(sex, nbaffairs))

graph5 + stat_summary(fun.y=mean, geom = "bar", fill = "white", colour = "black") +
   stat_summary(fun.data=mean_cl_normal, geom = "pointrange")

# Given the error bars we can't really say with any degree of confidence that
# men are more likely to be unfaithful than women - at least not by simply
# looking at the mean alone.

# We could try and break it out by rating factor:

graph5 <- ggplot(affairs, aes(sex, nbaffairs, fill = Rating_Factor))

graph5 + stat_summary(fun.y=mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data=mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.9), width = .5)

# OUr error bars are still pretty wide - not surprising given that unhappy people range from 0-12 affairs.
# Again - our boxplot is probably the best way of getting a visual sense of what is happening in the data.

# =======================================================================================

# ggplot is one of several visualization packages available in R
# Plotly is an R package for creating interactive web-based graphs 
# via the open source JavaScript graphing library plotly.js. As of 
# version 2.0 (November 17, 2015), Plotly graphs are rendered locally 
# through the htmlwidgets framework.

# https://plot.ly/r/ 

# install.packages("plotly")

library("plotly")

# Plotly will automatically create a bar chart:
# You'll also notice that we are no longer in plots pane but in the
# viewer pane:

graph6 <- plot_ly(affairs, x = ~nbaffairs, color = ~sex)
graph6
  
# or a scatter plot depending on the variables sent to it:

graph7 <- plot_ly(affairs, x = ~ym, y = ~age)
graph7

# You can also easliy adjust the size and color of 
# your data points:

graph7 <- plot_ly(affairs, x = ~ym, y = ~age,
                  color = ~ym, size = ~ym)
graph7
  
# That's not as interesting with the affairs dataset because
# of the type of data and number of cases. But if we plot
# against R's dimamonds dataset we start to get a sense
# of the functionality:

help(diamonds)

View(diamonds)

# Lets get a sample of 500 cases:

diamonds <- diamonds[sample(nrow(diamonds), 500), ]

graph8 <- plot_ly(
  diamonds, x = ~carat, y = ~price,
  color = ~carat, size = ~carat)

graph8

# Let's add a line to it:

graph10 <- plot_ly(diamonds, x = ~carat, color = ~carat) %>%
  add_markers(y = ~price, text = rownames(diamonds), showlegend = FALSE) %>%
  add_lines(y = ~fitted(loess(price ~ carat)),
            line = list(color = '#07A4B5'),
            name = "Loess Smoother", showlegend = FALSE) %>%
  layout(xaxis = list(title = 'Carat'),
         yaxis = list(title = 'Price'))

graph10

# In Graph10 we are using what is called Lowess Smoothing:
# Locally Weighted Scatterplot Smoothing)
# This method creates a smooth line through a timeplt or 
# in this case a scatter plot.

# Let's add a straight line to it:

graph11 <- plot_ly(diamonds, x = ~carat, color = ~carat) %>%
  add_markers(y = ~price, text = rownames(diamonds), showlegend = FALSE) %>%
  add_lines(y = ~fitted(line(price ~ carat)),
            line = list(color = '#07A4B5'),
            name = "Loess Smoother", showlegend = FALSE) %>%
  layout(xaxis = list(title = 'Carat'),
         yaxis = list(title = 'Price'))

graph11

# Let's do the same thing now and recreate our plot from
# the affairs dataset:


graph12 <- plot_ly(affairs, x = ~age) %>%
  add_markers(y = ~ym, text = rownames(affairs), showlegend = FALSE, color = ~nbaffairs, size = ~nbaffairs) %>%
  add_lines(y = ~fitted(line(ym ~ age)),
            line = list(color = '#07A4B5'),
            name = "Trend") %>%
  layout(xaxis = list(title = 'age'),
         yaxis = list(title = 'ym'))

graph12

