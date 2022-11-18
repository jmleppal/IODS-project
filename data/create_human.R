# Jaana Kuoppala
# 18.11.2022

# IODS2022: Assignment 5
# Dimensionality reduction techniques


## READING THE DATASETS

# access packages readr and dplyr
library(readr); library(dplyr)

# read the human development data into memory
hd <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv")

# read the gender inequality data into memory
gii <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv", na = "..")

# check structure and dimensions
glimpse(hd)
glimpse(gii)

# create summaries
summary(hd)
summary(gii)

# get column names
colnames(hd)
colnames(gii)

# rename the columns
hd <- rename(hd,
         HDIrank = "HDI Rank", 
         country = "Country", 
         HDI = "Human Development Index (HDI)",         
         explife = "Life Expectancy at Birth",            
         expyrsed = "Expected Years of Education",          
         myrsed = "Mean Years of Education",          
         GNI  = "Gross National Income (GNI) per Capita",
         GNI_HDI = "GNI per Capita Rank Minus HDI Rank") 
gii <- rename(gii,
        giirank = "GII Rank",                                     
        country = "Country",                                     
        GII = "Gender Inequality Index (GII)",               
        MMR = "Maternal Mortality Ratio",                    
        ABR = "Adolescent Birth Rate",                        
        propparl = "Percent Representation in Parliament",        
        edu2F = "Population with Secondary Education (Female)", 
        edu2M = "Population with Secondary Education (Male)",  
        jobF = "Labour Force Participation Rate (Female)",     
        jobM = "Labour Force Participation Rate (Male)")

gii <- mutate(gii, edu2ratio = edu2F / edu2M, jobratio = jobF / jobM)


##JOINING THE DATASETS

# join the two data sets by the selected identifiers
human <- inner_join(hd, gii, by = "country")

# glimpse at the joined data set
glimpse(human)


# SAVING THE COMBINED DATASET

# Set the working directory
setwd("C:/Users/yona2/Desktop/FT/IODS2022/RStudio/IODS-project/data")

write.csv(human, file = "human.csv", row.names = FALSE)

  