# Jaana Kuoppala
# 8.11.2022

# IODS2022: Assignment #2
# First steps in creating data and analysing it.

# READING IN THE ORIGINAL DATASET

# Read the data into memory
learning2014 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

# Look at the dimensions and structure of the data
dim(learning2014)
str(learning2014)

# About the dataset
# Dataset is based on an international survey of students' approaches to
# learning in 2013-2015.
# Measures are based on parts A, B and C in ASSIST (Approaches and Study Skills 
# Inventory for Students)and D is based on SATS (Survey of Attitudes Toward 
# Statistics). Some background and other variables have been omitted, but 
# otherwise all 183 responses are included.

# Dataset includes 183 observations and 60 variables.
# All variables except one (gender) are integers. There are no missing values.
  
# CREATING THE ANALYSIS DATASET

# Access the dplyr library
library(dplyr)

# Create an analysis dataset
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

learn2014an <- learning2014 %>% select(gender, Age, Points)
learn2014an$deep <- rowMeans(learning2014[, deep_questions])
learn2014an$surf <- rowMeans(learning2014[, surface_questions])
learn2014an$stra <- rowMeans(learning2014[, strategic_questions]) 

# create the column 'attitude' by scaling the column "Attitude"
learn2014an$attitude <- learning2014$Attitude / 10

# print out the column names of the data
colnames(learn2014an)

# change the names of certain columns
colnames(learn2014an)[2] <- "age"
colnames(learn2014an)[3] <- "points"

# select rows where points is greater than zero
learn2014an <- filter(learn2014an, points > 0)
 
# Look at the dimensions and structure of the analysis data
dim(learn2014an)
str(learn2014an)
  
# SAVING THE ANALYSIS DATASET AND CHECKING THAT READING ANEW WORKS

# Set the working directory
setwd("C:/Users/yona2/Desktop/FT/IODS2022/RStudio/IODS-project/data")

# Access the tidyverse library
library(tidyverse)

# Save the analysis dataset
write.csv(learn2014an,"learn2014an.csv", row.names = FALSE)

# Checking that reading of the saved file works
learn2014an2 <- read.table("learn2014an.csv", sep=",", header=TRUE)
str(learn2014an2)
head(learn2014an2)

# About the analysis dataset
# The original dataset is based on an international survey of students' approaches 
# to learning in 2013-2015. Measures are divided in parts A, B and C as in ASSIST 
# (Approaches and Study Skills Inventory for Students) and D as in SATS (Survey 
# of Attitudes Toward Statistics). Some variables have been omitted, but 
# all responses are included, the dataset containing 183 observations and 60 variables.
# There are no missing values.

# For the analysis dataset variables on gender, age and points were included as such
# but variable attitude was formed by dividing the original variable by 10 and
# variables deep, surf and stra were formed by averaging the respective variables
# of the part B. Value 0 in variable points actually means that the participant did not
# participate in the exam. These participants are excluded from the analysis dataset,
# having 166 observations and 7 variables.

# EXPLORING THE ANALYSIS DATA

# Access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)

# Draw a scatter plot matrix of the variables
pairs(learn2014an[-1], col = "green")

# Create and draw a more advanced plot matrix with ggpairs() - slow!
p <- ggpairs(learn2014an, mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))
p

# Initialize plot with data and aesthetic mapping
p1 <- ggplot(learn2014an, aes(x = attitude, y = points))

# Define the visualization type (points) + add a regression line
# + add a main title and draw the plot
p2 <- p1 + geom_point() + geom_smooth(method = "lm") + ggtitle("Student's attitude versus exam points")
p2


Show a graphical overview of the data and show summaries of the variables in the data. 
Describe and interpret the outputs, commenting on the distributions of the variables 
and the relationships between them.


# BUILDING A MULTIPLE REGRESSION MODEL

# Fit a linear model
my_model1 <- lm(points ~ age, data = learn2014an)
my_model2 <- lm(points ~ gender, data = learn2014an)
my_model3 <- lm(points ~ attitude, data = learn2014an)
my_model4 <- lm(points ~ deep, data = learn2014an)
my_model5 <- lm(points ~ surf, data = learn2014an)
my_model6 <- lm(points ~ stra, data = learn2014an)

my_model31 <- lm(points ~ attitude + age + gender, data = learn2014an)
my_model32 <- lm(points ~ attitude + stra + surf, data = learn2014an)

# Print out a summary of the models
summary(my_model1)
summary(my_model2)
summary(my_model3)
summary(my_model4)
summary(my_model5)
summary(my_model6)

summary(my_model31)
summary(my_model32)

# Lopullisessa mallissa vain attitude selittävänä tekijänä, muut tekijät eivät 
# parantaneet mallia. malli3


# Draw diagnostic plots using the plot() function, choosing the plots 1, 2 and 5
plot(my_model3, which = c(1, 2, 5))

# Plots in the same pane - doesn't really work
# par(mfrow = c(2,2))
# plot(my_model3, which = c(1, 2, 5))


Choose three variables as explanatory variables and fit a regression model 
where exam points is the target (dependent, outcome) variable. 
Show a summary of the fitted model and comment and interpret the results. 
Explain and interpret the statistical test related to the model parameters. 
If an explanatory variable in your model does not have a statistically significant 
relationship with the target variable, remove the variable from the model 
and fit the model again without it. 

Using a summary of your fitted model, explain the relationship between the chosen 
explanatory variables and the target variable (interpret the model parameters). 
Explain and interpret the multiple R-squared of the model. 

Produce the following diagnostic plots: Residuals vs Fitted values, Normal QQ-plot 
and Residuals vs Leverage. Explain the assumptions of the model and 
interpret the validity of those assumptions based on the diagnostic plots.
