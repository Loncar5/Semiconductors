library(tidyverse)
library(readr)
library(plyr)
library(stringr)
library(gsubfn)
library(proto)
library(rpart) # #runs a decision tree

View(clean_predictors) # I created a new variable, which is the same as Predictors_without_Duplicates

#to preform PCA, the data set has to be numeric
str(clean_predictors)

#deviding the new data set

pca.train <- clean_predictors[1:nrow(clean_predictors),]
pca.test <- clean_predictors[-(1:nrow(clean_predictors)),]

prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)

#outputs the mean of variables
prin_comp$center
#outputs the standard deviation of variables
prin_comp$scale

prin_comp$rotation

prin_comp$rotation[1:5,1:4]

dim(prin_comp$x)

biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev <- prin_comp$sdev

#compute variance
pr_var <- std_dev^2

pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
