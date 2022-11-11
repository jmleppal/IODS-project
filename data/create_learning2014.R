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

# EXPLORING THE ANALYSIS DATA

# Access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)
library(cowplot)

# Create boxplots for other variables by gender
pgen1 <- ggplot(learn2014an, aes(x = gender, y = age)) + geom_boxplot()
pgen2 <- ggplot(learn2014an, aes(x = gender, y = attitude)) + geom_boxplot()
pgen3 <- ggplot(learn2014an, aes(x = gender, y = deep)) + geom_boxplot()
pgen4 <- ggplot(learn2014an, aes(x = gender, y = surf)) + geom_boxplot()
pgen5 <- ggplot(learn2014an, aes(x = gender, y = stra)) + geom_boxplot()
pgen6 <- ggplot(learn2014an, aes(x = gender, y = points)) + geom_boxplot()

# Draw those boxplots in one row (tähän tarvitaan lib cowplot)
ggdraw() +
  draw_plot(pgen1, 0, .5, .16, .5) +
  draw_plot(pgen2, .16, .5, .16, .5) +
  draw_plot(pgen3, .32, .5, .16, .5) +
  draw_plot(pgen4, .5, .5, .16, .5) +
  draw_plot(pgen5, .66, .5, .16, .5) +
  draw_plot(pgen6, .82, .5, .16, .5)

# Get summary values
summarise_at(learn2014an, vars(attitude:points), funs(mean, sd))
learn2014an %>% group_by(gender) %>% summarise_at(vars(age), funs(n(), mean, sd, min, median, max))
learn2014an %>% group_by(gender) %>% summarise_at(vars(attitude), funs(mean, sd, min, median, max))
learn2014an %>% group_by(gender) %>% summarise_at(vars(deep), funs(mean, sd, min, median, max))
learn2014an %>% group_by(gender) %>% summarise_at(vars(surf), funs(mean, sd, min, median, max))
learn2014an %>% group_by(gender) %>% summarise_at(vars(stra), funs(mean, sd, min, median, max))
learn2014an %>% group_by(gender) %>% summarise_at(vars(points), funs(mean, sd, min, median, max))

# Draw a scatter plot matrix of the variables - faster
pairs(learn2014an[-1], col = "green")

# Create and draw a more advanced plot matrix with ggpairs()
p <- ggpairs(learn2014an[, c(1, 2, 7, 4, 5, 6, 3)], mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)), progress = FALSE)
p

# Initialize plot with data and aesthetic mapping
p1 <- ggplot(learn2014an, aes(x = attitude, y = points))

# Define the visualization type (points) + add a regression line
# + add a main title and draw the plot
p2 <- p1 + geom_point() + geom_smooth(method = "lm") + ggtitle("Student's attitude versus exam points")
p2

# BUILDING A MULTIPLE REGRESSION MODEL

# Fit a linear model
my_model1 <- lm(points ~ age, data = learn2014an)
my_model2 <- lm(points ~ gender, data = learn2014an)
my_model3 <- lm(points ~ attitude, data = learn2014an)
my_model4 <- lm(points ~ deep, data = learn2014an)
my_model5 <- lm(points ~ surf, data = learn2014an)
my_model6 <- lm(points ~ stra, data = learn2014an)

my_model31 <- lm(points ~ attitude + stra + surf, data = learn2014an)
my_model32 <- lm(points ~ attitude + age + gender, data = learn2014an)

# Print out a summary of the models
summary(my_model1)
summary(my_model2)
summary(my_model3)
summary(my_model4)
summary(my_model5)
summary(my_model6)

summary(my_model31)
summary(my_model32)

# Lopullisessa mallissa (malli3) vain attitude selittävänä tekijänä, muut tekijät eivät 
# parantaneet mallia.

# Draw diagnostic plots using the plot() function, choosing the plots 1, 2 and 5
# par(mfrow = c(2,2)) # doesn't seem to work in this script but works in R Markdown
plot(my_model3, which = c(1, 2, 5))
