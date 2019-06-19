#############################
#  This R code uses ordinarlt least squares linear regression to predict real estate selling prices for n>p case. 
#  R script author - B. Waidyawansa (2/20/2019)
#############################


# Load required packages
library(caret) # Package to traint the model
library(DMwR)
library(boot) # Package to do bootstrap error estimates
library(rstudioapi)
library(knitr)
library(tidyr)
library(ggplot2)
library(e1071)          # Explore the Skewness of various features of the dataset.

# Clean up the directory
rm(list=ls())

## @knitr ols_preprocess
# Seperate the target variable (y) and the preictors (x)
dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
dataIn.x <- dataIn[-nrow(dataIn),-1]


## @knitr ols_cor_plot
###################################################################
# Check for correlations between predictors
###################################################################
corrplot::corrplot(cor(as.matrix(dataIn.x)),method = "number",type = "upper",number.cex=1.5)
#chart.Correlation(dataIn, histogram=TRUE, pch=19)

###################################################################
# Full data set to use for model testing
###################################################################
# Remove the last line from the data set before using for testing because in this program
# the last line is contains the data that needs to be predicted using the final model
dataIn.full <- cbind(dataIn.y,dataIn.x)

###################################################################
# Get the test data set.
# This program assumes the test data are in the last row of the input
# dataset
###################################################################
dataIn.test <- dataIn[nrow(dataIn),-1]
x.test <-as.matrix(dataIn.test)

## @knitr make_ols_model
###################################################################
# Create a model
###################################################################
y.name=names(dataIn.y)[1]
x.names=names(dataIn.x)
lm.formula=paste(y.name,paste0(x.names,collapse=' + '),sep=' ~ ')

#dataIn <- dataIn[!is.na(dataIn[,1]),]
ols.lmodel=lm(lm.formula,data=dataIn.full)

###################################################################
# Use caret package to train the ols model to get the 
# best values for the intercept is selected by tenfold CV. 
# The chosen intercept is the one giving the smallest CV error.
###################################################################

# Use leave-one-out-cross-validation method for cross-validation.
# Using RMSE
train.control = trainControl(method = "LOOCV")

# perfrom cross-validated forecasting of SellingPrice using all features
set.seed(42)
train.ols = train(
  dataIn.full[, -1], dataIn.full[, 1],
    method = "lm",
    metric = "RMSE",
    trControl = train.control
)

###################################################################
# Variable importance
###################################################################
## @ knitr var_importance
plot(varImp(train.ols))


###################################################################
# Make predictions using the final model selected by caret
###################################################################
## @ knitr best_model
# Get model prediction error(RMSE) from the best model
best = which(rownames(train.ols$results) == rownames(train.ols$bestTune))
best.result = train.ols$results[best, ]
rownames(best.result) = NULL

prediction.error <- best.result$RMSE
prediction.rsquared <-best.result$Rsquared

# Get the best model (model with best intercept that gives minimum RMSE)
# By default, the train function chooses the model with the largest performance value
# (or smallest, for mean squared error in regression models).
final.ols.model <- train.ols$finalModel


# Get the model coefficients from the final model
beta.hat <- tidy(train.ols$finalModel)
y.hat.ols <- predict(final.ols.model,as.data.frame(x.test))
y.hat <- y.hat.ols



