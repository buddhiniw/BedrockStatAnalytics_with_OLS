#############################
#  This R code uses Gaussian linear regression with Elastic Net regularization to predict
#  real estate selling prices. Apart from the analysis, most display functionality has been stripped down 
#  from the original format to enable embedding in RMarkdown. The original code can be found:
#  https://github.com/buddhiniw/BEDROCK-ELNET
#
#  Reference - "Regularization and variable selection via the elastic net"
#  Hui Zou and Trevor Hastie
#  J. R. Statist. Soc. B (2005) 67 , Part 2 , pp. 301–320
# 
#  
#  R script author - B. Waidyawansa (2/18/2019)
#############################


# Load required packages
library(caret) # Package to traint the model
library(glmnet) # Package to fit ridge/lasso/elastic net models
library(DMwR)
library(boot) # Package to do bootstrap error estimates
library(elasticnet)
library(rstudioapi)
library(knitr)
# Clean up the directory
rm(list=ls())

#dataIn <- read.csv(file='/home/buddhini/MyWork/Upwork/R_bedrock_gui/PDF_Report/Bedrock-Add-OLS/src/ason1.csv')


## @knitr standardize_xy
###################################################################
# Standardize the predictor(x) variables and response(y) variable
###################################################################
# Standarizing/scaling the predictor variables prior to fitting the model will ensure
# the lasso penalization will treat different scale explanatory variables on a more 
# equal footing. 
# BUT only continuos predictors needs to be standardized.
# DONOT scale factor variables (using 1/0) used for yes/no situations.
#
# NOTE - We cannot use the preProcess option in the caret package to do the scaling 
# because real estate datasets contain factor variables.
#
# Get x variables (including the data-to-be-predicted row)
# 
# Scale x varibles
dataIn.x <- dataIn[,-1]

# Identify columns with categorical data (0/1)
cat.vars <- apply(dataIn.x,2,function(x) { all(x %in% 0:1) })
dataIn.continuous <- dataIn.x[!cat.vars]
dataIn.catrgorical <- dataIn.x[cat.vars]

# Now scale the continuous variables 
dataIn.continuous.scaled <-scale(dataIn.continuous,center=TRUE, scale=TRUE)

# Create the scaled x 
x.scaled <- cbind(dataIn.continuous.scaled,dataIn.catrgorical)

###############################
# Scale y (responce) variable
###############################
dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
y.scaled <- as.data.frame(dataIn.y.scaled)

# Create the X and Y regression matrices
x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
y <- as.matrix(y.scaled)


###################################################################
# Get the scaled test data set.
# This program assumes the test data are in the last row of the input
# dataset
###################################################################
dataIn.scaled.test <- x.scaled[nrow(x.scaled),]
x.test <-as.matrix(dataIn.scaled.test)


## @knitr corr_plot
###################################################################
# Check for correlations between predictors
###################################################################
corrplot::corrplot(cor(as.matrix(dataIn.x)),method = "number",type = "upper",number.cex=1.5)


## @knitr loocv
###################################################################
# Use caret package to train the elasticnet model to get the 
# best parameter values for lambda(weight decay) and s(fraction).
# From reference -
# Pick a (relatively small) grid of values for λ2 , say .0, 0:01, 0:1, 1, 10, 100/. 
# Then, for each λ2 , algorithm LARS-EN produces 
# the entire solution path of the elastic net. 
# The other tuning parameter (λ1 , s or k) is selected by tenfold CV. 
# The chosen λ2 is the one giving the smallest CV error.
###################################################################

# Set s and lambda grid 
#lambda.grid <- 10^seq(5,-5,length=100)
lambda.grid <- c(0,0.01,0.1,1,10,100)

#lambda.grid <- seq(0,10,by = 1)
s.grid <- seq(0,1,by=0.05)

# Use leave-one-out-cross-validation method for cross-validation.
# Using RMSE
train.control = trainControl(method = "LOOCV")

# Setup serach grid for s and lambda
#search.grid <- expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)
search.grid <- expand.grid(.fraction = s.grid, .lambda = lambda.grid)

# Full dataset after scaling
dataIn.scaled <- cbind(y,x)
# Remove NA entries
dataIn.scaled <- na.omit(dataIn.scaled)

# perfrom cross-validated forecasting of SellingPrice using all features
set.seed(42)
train.enet = train(
    x.scaled[1:(nrow(x.scaled)-1),], y.scaled[[1]],
    method = "enet",
    metric = "RMSE",
    tuneGrid = search.grid,
    normalize = FALSE,
    intercept = FALSE,
    trControl = train.control
)


# Plot CV performance
par(mar = c(5,5,6,5))
plot(train.enet,plotType = "line", xlab="Fraction (s)",scales=list(x=list(cex=0.75), y=list(cex=0.75)))
trellis.par.set(caretTheme())

best.fraction <- train.enet$bestTune$fraction
best.lambda <- train.enet$bestTune$lambda

###################################################################
# Variable importance
###################################################################
## @knitr var_importance
plot(varImp(train.enet))

## @knitr best_model
# Get model prediction error(RMSE) from the best tune
best = which(rownames(train.enet$results) == rownames(train.enet$bestTune))
best.result = train.enet$results[best, ]
rownames(best.result) = NULL
prediction.error <- best.result$RMSE
prediction.rsquared <-best.result$Rsquared

# Get the best model (model with best alpha that gives minimum RMSE)
# By default, the train function chooses the model with the largest performance value
# (or smallest, for mean squared error in regression models).
final.enet.model <- train.enet$finalModel

# Get the model coefficients at optimal lambda from the final model
beta.hat <- predict.enet(final.enet.model,
                                     s=best.fraction,
                                     type="coefficient",
                                     mode="fraction")

###################################################################
# Make predictions using the final model selected by caret
###################################################################
y.hat.enet.scaled <- predict.enet(final.enet.model,
                                  as.data.frame(x.test),
                                  s=best.fraction,
                                  type="fit",
                                  mode="fraction")
# Unscale to get the actual magnitude
y.hat.enet.unscaled <- unscale(y.hat.enet.scaled$fit,dataIn.y.scaled)
y.hat <- y.hat.enet.unscaled # to make it easy to pass into help functions.



