# Jaana Kuoppala
# 16.11.2022

# IODS2022: Assignment #3
# Learning to do logistic regression in R.


## READING THE DATASETS

# access packages readr and dplyr
library(readr); library(dplyr)

# read the math class questionnaire data into memory
math <- read.table("student-mat.csv", sep = ";", header = TRUE)

# read the Portuguese class questionnaire data into memory
por <- read.table("student-por.csv", sep = ";", header = TRUE)

# check structure and dimensions
glimpse(math)
glimpse(por)


##JOINING THE DATASETS

# access the dplyr package
library(dplyr)

# give the columns that vary in the two data sets
free_cols <- c("failures", "paid", "absences", "G1", "G2", "G3")

# the rest of the columns are common identifiers used for joining the data sets
join_cols <- setdiff(colnames(por), free_cols)

# join the two data sets by the selected identifiers
math_por <- inner_join(math, por, by = join_cols, suffix = c(".math", ".por"))

# glimpse at the joined data set
glimpse(math_por)


## 3.3 GETTING RID OF DUPLICATES

# create a new data frame with only the joined columns
alc <- select(math_por, all_of(join_cols))
                          
# for every column name not used for joining...
# select two columns from 'math_por' with the same original name
# select the first column vector of those two columns
#   if that first column vector is numeric...
#     take a rounded average of each row of the two columns and
#     add the resulting vector to the alc data frame
#   else (if the first column vector was not numeric)...
#     add the first column vector to the alc data frame
for(col_name in free_cols) {
  two_cols <- select(math_por, starts_with(col_name))
  first_col <- select(two_cols, 1)[[1]]
  if(is.numeric(first_col)) {
  alc[col_name] <- round(rowMeans(two_cols))
  } else {
         alc[col_name] <- first_col
         }
}
                          
# glimpse at the new combined data
glimpse(alc)

                          
## NEW COLUMNS FOR ALCOHOL USE
                          
# access the tidyverse packages dplyr and ggplot2
library(dplyr); library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# initialize a plot of alcohol use
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))

# define the plot as a bar plot and draw it
g1 + geom_bar()

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

# initialize a plot of 'high_use'
g2 <- ggplot(data = alc, aes(x = high_use))

# draw a bar plot of high_use by sex
g2 + geom_bar() + facet_wrap("sex")

# glimpse at the new combined data
glimpse(alc)


## SAVING THE FILE

# save the alc file
write.csv(alc, file = "alc.csv", quote = FALSE)
