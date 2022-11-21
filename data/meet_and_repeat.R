# Jaana Kuoppala
# 20.11.2022

# IODS2022: Assignment 6
# Analysis of longitudinal data


## READING THE DATASETS

# access packages readr and dplyr
library(readr); library(dplyr)

# Read the BPRS data
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep =" ", header = T)

# Look at the (column) names of BPRS
names(BPRS)

# Glimpse the data
glimpse(BPRS)


# read in the RATS data
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

# Look at the (column) names of BPRS
names(RATS)

# Glimpse the data
glimpse(RATS)


# Set the working directory
setwd("C:/Users/yona2/Desktop/FT/IODS2022/RStudio/IODS-project/data")

write.csv(BPRS, "BPRS.csv", row.names = FALSE)
write.csv(RATS, "RATS.csv", row.names = FALSE)

# BPRS
# Data contains information on 40 subjects that were equally assigned to 2 treatment groups.
# Response to treatment was measured by the Brief psychiatric rating scale (BPRS) at baseline
# (week0) and weekly thereafter up to 8 weeks (week1-week8).
# RATS
# Data contains information on 16 rats that were assigned to 3 treatment groups.
# Weight was measured at baseline (WD1) and about once a week thereafter up to day WD64.


# CONVERSION OF CATEGORICAL VARIABLES TO FACTORS

# Factor variables treatment and subject in BPRS and check
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
glimpse(BPRS)

# Factor variables ID and Group in RATSand check
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)
glimpse(RATS)


# CONVERSION TO LONG FORM AND ADD A VARIABLE

# Convert to long form
BPRSL <-  pivot_longer(BPRS, cols = -c(treatment, subject),
                       names_to = "weeks", values_to = "bprs") %>%
  arrange(weeks) #order by weeks variable

# Extract the week number
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks, 5, 5)))

# Take a glimpse at the BPRSL data
glimpse(BPRSL)


# Convert data to long form
RATSL <- pivot_longer(RATS, cols = -c(ID, Group), 
                      names_to = "WD",
                      values_to = "Weight") %>% 
  mutate(Time = as.integer(substr(WD, 3, 4))) %>%
  arrange(Time)

# Glimpse the data
glimpse(RATSL)


# CHECKING

names(BPRS); names(BPRSL)
str(BPRS); str(BPRSL)
summary(BPRS); summary(BPRSL)

names(RATS); names(RATSL)
str(RATS); str(RATSL)
summary(RATS); summary(RATSL)


# Save long form datasets
write.csv(BPRSL, "BPRSL.csv", row.names = FALSE)
write.csv(RATSL, "RATSL.csv", row.names = FALSE)


# Data wrangling performed successfully and data look as expected.

