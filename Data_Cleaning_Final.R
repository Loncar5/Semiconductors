library(tidyverse)
library(readr)
library(plyr)
library(stringr)
library(gsubfn)
library(proto)

getwd()

Predictors <- read_delim("C:/Users/Luka Loncar/Desktop/HTW/Data Mining/Semiconducotrs/Semiconducters Project/Predictors.csv", 
                         " ", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

View(Predictors)
class(Predictors)

Labels <- read_delim("C:/Users/Luka Loncar/Desktop/HTW/Data Mining/Semiconducotrs/Semiconducters Project/Predictors.csv", 
                     " ", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)

#Renaming variables/features in Predictors 
names(Predictors) <- sprintf("feature% d", 1:590)

#Renaming variables/features in Predictors 
  #I used R base functions beacause of conflict between dplyr and plyr packages
  #for some reason even chanelling the direct enviroment didn't manage to work with ::
names(Labels)[names(Labels) == "X1"] <- "date"
names(Labels)[names(Labels) == "X2"] <- "time"

#Merging together two data sets
analysis <- cbind(Labels, Predictors)

#Changing from data.frame to tibble format because of packages that will come
as_tibble(analysis)

#Creating a new variable, becase we need 1 and -1 for classification modeling
new_variable <- str_split_fixed(analysis$date, " ", 2)

#Cbining newly created data set with main analysis data set
analysis <- cbind(new_variable, analysis)

#Removing duplicate column
analysis <- analysis[-3]

#Column renaming (1 means that production of semicounductors failed at some point)
names(analysis)[names(analysis) == "1"] <- "production"
names(analysis)[names(analysis) == "2"] <- "date"

#Removing all the unnecessary quotations marks
analysis$time <- lapply(analysis$time, gsub, pattern='"', replacement='')
analysis$date <- lapply(analysis$date, gsub, pattern='"', replacement='')
analysis$production <- lapply(analysis$production, gsub, pattern='"', replacement='')

#all together number of na's 
sum(is.na(analysis))

#NA's per column 
count_the_na_values <-sapply(analysis, function(y) sum(length(which(is.na(y)))))
count_the_na_values <- data.frame(count_the_na_values)
View(count_the_na_values)

#If there is more than 55% of NA's, then write TRUE
null_a <- colMeans(is.na(analysis)) > .55
null_a <- data.frame(null_a)

#Now we have a dataframe which has a column of true/false values. (t stands for transform)
null <- t(null_a)

#Make true/false values numeric
null <- as.numeric(null)
null <- data.frame(null)

#Now we are transposing the column to a row to add it at the end of analysis table as the last row.
null <- t(null)

#Now null will be the last row of analysis_3
analysis[1568,] <- null

#in new table, we are removing the columns which have value 1 on the last row of it.
analysis_with_NAcontrol <- analysis[ ,!(analysis[1568,]==1)]

#we can count the NA's again, idk Luka you wanted to count them again duuude
#na_count <-sapply(analysis_4, function(y) sum(length(which(is.na(y)))))
#na_count <- data.frame(na_count)
#View(na_count)
###########################

# So, now we are going to replace remaining NA values with the mean of the column.

for(i in 1:ncol(analysis_with_NAcontrol)){
  analysis_with_NAcontrol[is.na(analysis_with_NAcontrol[,i]), i] <- mean(analysis_with_NAcontrol[,i], na.rm = TRUE)
}

#So the number of NA's were 41951 and now it is zero.
sum(is.na(analysis_with_NAcontrol))
sum(is.na(analysis))

summary(analysis_with_NAcontrol)

#We remove the columns with 0 variance.
data_variance_filter <- analysis_with_NAcontrol[-c(1:3)]
data_variance_filter <- Filter(var, data_variance_filter)
summary(data_variance_filter)

# Bind the target variable and time stamps with the cleaned dataset
filtered_data <- cbind(analysis_with_NAcontrol[1:3],data_variance_filter)

#At the end, here are the datasets that we found after different level of data cleaning

# 1st dataset we got after converting the dataset on the website to .csv file that we can use offline
summary(Predictors)
ncol(Predictors)

# The dataset we have after removing the columns with 55% of NA values
Predictors_without_NA <- analysis_with_NAcontrol[-c(1:3)]
ncol(Predictors_without_NA)

# The dataset we found after removing the variables with variance = 0
Predictors_without_Duplicates <- data_variance_filter
ncol(Predictors_without_Duplicates)

#Compare the number of NA's

ncol(Predictors)
ncol(Predictors_without_NA)
ncol(Predictors_without_Duplicates)

sum(!is.na(Predictors))
sum(is.na(Predictors_without_NA))
sum(!is.na(Predictors_without_Duplicates))
