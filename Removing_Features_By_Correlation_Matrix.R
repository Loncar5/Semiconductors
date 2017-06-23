install.packages("mlbench")
library(mlbench)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)

# calculate correlation matrix
correlationMatrix <- cor(clean_predictors[,])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
# print indexes of highly correlated attributes
print(highlyCorrelated)

as.data.frame(highlyCorrelated)
