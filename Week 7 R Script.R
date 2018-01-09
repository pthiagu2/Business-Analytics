# Week 7 - Introduction to R

# ===========================================

# R is a case sensitive, interpreted language.

# You can run commands one at a time at the command prompt, 
# or you can create and run a set of commands from a source file.
# R studio allows you to create projects and is known as an
# IDE - Integrated Development Environment.


# ===========================================

# R statements consist of functions and assignments. R uses
# <- for assignments rather than = though either will work. 

# Assign a value to x (x is the object and '2+1' is the function)

x <- 2+1

# Assign a value to y

y = 2+1 

# Find the sum of x + y

x+y


# ===========================================

# A Sample R Session

# the c() function combines its values into
# a vector (list)

age <- c(1,3,5,2,11,9,3,9,12,3)

weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)

# Let's get the mean for weight

m <- mean(weight)
m

# Let's get the standard deviation for weight.

sd(weight)

# Is the mean a good fit? We can visualize this with a 
# histogram:

# Histogram of Weight

hist(weight)
abline(v = m, col = "blue", lwd = 2)

# Let's get the correlation between age and weight.

cor(age,weight) # .9075655 - they are highly correlated

# Let's create a plot of age and weight:

plot(age,weight)

# Add a line: LOWESS (Locally Weighted Scatterplot Smoothing) 
# is a tool used in regression analysis that creates a smooth 
# line through a timeplot or scatter plot to help you to see 
# relationship between variables and foresee trends.

lines(lowess(age,weight), col="blue") # lowess line (x,y)


# the q() function will end your session - R Studio will
# prompt you to save your work session.

# q()

# ===========================================

# Installing packages

# R is powerful because it allows developers from all
# over the world to add packages which contain functions
# allowing you to perform specific tasks.

# To install a package use the following syntax:

# install.packages("package.name")

install.packages("readr")

# You can also manage packages from the packages pane.
# Many times R will prompt you if a dependant package is required.

# ===========================================

# Setting a working directory

# Setting a working directory is important as it allows you
# to save and access all of the files related to a single 
# project in the same place. 

# the getwd() function will tell you what your current directory
# is set to.

getwd()

# You can change the working directory by using the setwd() function.
# When you run this script at home you will need to change the directories
# so they are specific to your machine.

setwd("C:/Users/jpg2847/Desktop")

getwd()

setwd("C:/Users/jpg2847/Documents/To File 20170817/LIS 590 BAO/Slides/WEEK 7 - R SCRIPT")

getwd()

# If I want to see what files exist in a directory, I can use list.files()

list.files("C:/Users/jpg2847/Documents/To File 20170817/LIS 590 BAO/Slides/WEEK 7 - R SCRIPT")

# ===========================================

# Importing Data

# to load a file from this directory, I could use the following command:

library(readxl)

data <- read_excel("C:/Users/jpg2847/Documents/To File 20170817/LIS 590 BAO/Slides/WEEK 7 - R SCRIPT/Data.xlsx")
View(data)

library(readr)

data1 <- read_csv("C:/Users/jpg2847/Documents/To File 20170817/LIS 590 BAO/Slides/WEEK 7 - R SCRIPT/Data.csv")
View(data1)

library(readr)

data2 <- read_delim("C:/Users/jpg2847/Documents/To File 20170817/LIS 590 BAO/Slides/WEEK 7 - R SCRIPT/Data.txt", 
                   "\t", escape_double = FALSE, trim_ws = TRUE)
View(data2)

# A partial titanic dataset already comes installed with R

View(Titanic)

# We now have three data objects with assigned from different file types
# For our convience they appear in the environment pane as global objects.

# ===========================================

# Getting Help

# If you get stuck on a function you can use the help function
# to understand how it works:

help(help)

# In R Studio - help will appear in the help pane.

help(readr)
help(read_delim)

# ===========================================

# Getting Data into R

# In R we can create objects using the c() function
# Here we are entering strings (words) using "

Enterprise_Captains <- c("Picard","Kirk","Archer")

Enterprise_Captains

# We can also use numbers to order the captains
# by their order of service:

Captain_Order <- c(3,2,1)

# We can use dataframes to combine seperate objects
# into single objects:

Enterprise <- data.frame(Name = Enterprise_Captains, Service_Order = Captain_Order)

# Now when I call the object Enterprise, I get a table in return:

Enterprise

# I can pull out individual variables using the following:

Enterprise$Name

Enterprise$Service_Order

# We can add variables to a data frame as follows; 
# supposing I wanted to Add the captains' first names:

Enterprise$First_Name <- c("Jean-Luc","James","Jonathan")

Enterprise

# If you want to see the variables in a dataframe, you can access them
# by using the names() function:

names(Enterprise)

# ===========================================

# Manipulating Variables

# Suppose I wanted to combine two variables in a dataframe;
# If I wanted to combine Name and First_Name to get Full_name:

Enterprise$Full_name <- paste(Enterprise$First_Name, Enterprise$Name)

Enterprise

# Suppose I add a couple of new variables: Age_At_Command, Age_At_Retirement

Enterprise$Age_At_Command <- c(45,25,35)

Enterprise$Age_At_Retirment <- c(80,65,70)

Enterprise

# Now suppose I want to know their years of service 
# onboard the Enterprise; I can use any logical operator
# that makes sense for my data type (see field page 84).
# In this case, I want to the difference from Age at Retirment
# and Age at Command:

Enterprise$Years_In_Command <- Enterprise$Age_At_Retirment - Enterprise$Age_At_Command

Enterprise

# Suppose now I wanted to add each captains' birthdate;
# I could use the as.Date() function to convert strings to 
# dates:

Enterprise$Birth_Date <- as.Date(c("2305-07-13","2233-03-22","2112-10-9"))

Enterprise

# We can also create binary and non-binary coding variables. 
# Suppose we wanted to create an independent variable for baldness
# We could do this using a binary variable 1 meaning yes,
# and 0 meaning no:

Enterprise$Bald <- c(1,0,0)

Enterprise

# If you had a much larger data frame, you could use
# the rep function instead of hard coding each variable
# as shown below - the result is the same:

Enterprise$Bald2 <- c(rep(1,1), rep(0,2))

Enterprise

# To remove a column from a data frame,
# you can set the column to NULL:

Enterprise$Bald2 <- NULL

Enterprise


# ===========================================

# Factors

# To have R recognize something as a nominal variable
# We need to convert it to a factor.

# Let's create a sample data frame:

Patient_ID <- c(1,2,3,4,5,6,7,8,9,10)
Patient_Group <- c(1,1,1,2,2,3,3,3,4,4)

Patients <-  data.frame(ID = Patient_ID, Group = Patient_Group)

Patients

# Now lets convert Group into a factor:

Patients$Group_Factor <- factor(Patients$Group, levels = (1:4),
                                  labels = c("Red","Yellow","Blue","Green"))

Patients

# Missing Values - Sometimes your data will be 
# incompelte for whatever reason (NULL values in SQL)
# in R we us NA to represent this:

Patients$Age <- c(10,45,NA,60,70,NA,90,50,67,80)

Patients

# Sometimes you will need to tell R to ignore missing 
# values - to compute the mean for example. We can do that
# here as follows:

mean(Patients$Age)

mean(Patients$Age, na.rm = TRUE)

# ===========================================

# Exporting Data

# Sometimes we want to export data to a file. 
# We can do this using the write.table() function:

# as a .TXT file:

write.table(Enterprise,"Enterprise_Data.TXT", sep="\t", row.names = FALSE)

# as a .CSV file:

write.csv(Enterprise,"Enterprise_Data.CSV")

# ===========================================

# Manipulating Data

# Sometimes when you have a lot of data you may only
# want to select a portion of it. 

data

Titanic_Bio <- data[, c("Name","Sex","Age")]

Titanic_Bio

# We can also specify values

Titanic_Women <- data[data$Sex=="female",]

Titanic_Women

# We can also specify both rows and columns

Titanic_Survivers <- data[data$Survived == 1, c("Name","Sex","Age","Survived")]

Titanic_Survivers

# ===========================================

# Matrices

# Some functions in R only work on Matrices.
# We can convert data frames to matrices - Remember
# a matrix only takes numbers

Titanic_Metrics <- data[, c("Survived","Pclass","Age","SibSp","Parch")]

Titanic_Metrics

Titanic_Matrix <- as.matrix(Titanic_Metrics)

Titanic_Matrix

Titanic_Survivers_Matrix <- as.matrix(data[data$Survived == 1, c("Survived","Pclass","Age","SibSp","Parch")])

Titanic_Survivers_Matrix

# ===========================================

# Reshaping or Pivoting Data

# Sometimes we are required to pivot a table
# in order to do certain calucations.

# Start with a tabular data frame:

Donors <- c("Donor_A","Donor_B","Donor_C","Donor_D")
Giving_Year_1 <- c(100,250,10,5)
Giving_Year_2 <- c(10,300,400,7)
Giving_Year_3 <- c(59,254,340,100)

Donations <-  data.frame(Donor = Donors, 
                         Year_1 = Giving_Year_1,
                         Year_2 = Giving_Year_2,
                         Year_3 = Giving_Year_3)

Donations

# Pivot using stack()

Donations1 <- stack(Donations, select = c("Year_1", "Year_2", "Year_3"))

Donations1

# You can unpivot using unstack()

Donations2 <- unstack(Donations1)

Donations2

# Pivot using reshape()

# install.packages("reshape")

library(reshape)

Donations3 <- melt(Donations, 
                   id = c("Donor"),
                   measured = c("Year_1", "Year_2", "Year_3"))

Donations3

# unpivot using cast()

Donations4 <- cast(Donations3, Donor ~ variable, value = "value")

Donations4

# ===========================================

# Connecting R to MySQL

#install.packages("RMySQL")

library(RMySQL)

# Connect to the database 
# ***You will need to be connected to the UIUC VPN for this***

NorthWind <- dbConnect(MySQL(), user="jgough2_student", password="MySQListhebest!", 
               dbname="jgough2_NorthWind", host="cpanel.ischool.illinois.edu")

# If you'd like to see the tables that exist in the database you can do the following:

dbListTables(NorthWind)

# You can query the database useing the dbSendQuery() function and
# a SQL query:

Order_Data <- dbSendQuery(NorthWind, "Select * from Orders_Qry")

# Assign the result set to an R object as follows:

Orders <- fetch(Order_Data, n=-1)

Orders

# convert your object to a dataframe:

Order <- as.data.frame(Orders)

Order

# If I wanted to see the column names in my new dataframe:

colnames(Order)

# ===========================================

# A little Teaser - Plotting MySQL Data

boxplot(split(Order$Freight,Order$Country), main="Freight by Country", las=2, col="cadetblue3", outline=FALSE,
        border = "azure4", xlab = "", ylab = "Order Freight", col.lab="dimgray")
mtext("Country", side=3, col="dimgray")

# fit a line

linearFit <- lm(Freight~Country, data=Orders)
abline(h=c(linearFit$coefficients[1]), col="brown1", lty=2, lwd=5)

# ===========================================






