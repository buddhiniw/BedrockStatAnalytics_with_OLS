#############################
# Helper functions to create Tables For MS Word .docx
#############################
#  Author by B. Waidyawansa (10/10/2018)


library(flextable)
library(officer)
library(tidyverse)
library(prettyR)
library(knitr)
library(broom)

## @knitr descriptive_stat
###################################################################
# Descriptive statistics from unscaled data
###################################################################
# Get the original data frame minus the test data set
df<-dataIn[1:(nrow(dataIn)-1),]
data <- as.data.frame(describe(df ,num.desc=c("valid.n","mean","sd","min","max"))$Numeric)
data.t <- as.matrix(t(data))
data.t <- formatC(data.t, digits = 2, format = "d", flag = "0")
colnames(data.t) <- c("N", "Mean", "Std. Dev.", "Min","Max")
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
print(xtable(data.t, 
             align= "|l|c|r|r|r|r|",
             caption = "Summary statistics. Note that Mean and Std. Dev. are meaningless for categorical variables."), 
      comment=FALSE, 
      sanitize.colnames.function=bold,
      sanitize.rownames.function=bold, 
      booktabs=F,
      floating = TRUE, latex.environments = "center")


###################################################################
# Make predictions using the final model selected by caret
###################################################################
# Predict coefficients
## @knitr coef_tble
data.c <- as.matrix(beta.hat$coefficients)

data.c <- formatC(data.c, digits = 3, format = "f", flag = "0")
colnames(data.c) <- c("Estimate")
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
print(xtable(data.c,
             align = "|l|r|"),
      comment=FALSE,
      sanitize.colnames.function=bold,
      sanitize.rownames.function=bold,
      booktabs=F,
      floating = TRUE, latex.environments = "center")


## @knitr ols_coef_tble
data.c <- as.matrix(beta.hat)

data.c <- formatC(data.c, digits = 3, format = "f", flag = "0")
#colnames(data.c) <- c("Estimate")
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
print(xtable(data.c,
             align = "|l|r|r|r|r|r|"),
      comment=FALSE,
      sanitize.colnames.function=bold,
      sanitize.rownames.function=bold,
      booktabs=F,
      floating = TRUE, latex.environments = "center")


# Predicted Price
## @knitr predict_price
data.p <- as.matrix(cbind(y.hat,prediction.error, prediction.rsquared))
data.p <- formatC(data.p, digits = 3, format = "f", flag = "0")
colnames(data.p) <- c("Predicted Value", "Prediction Error", "R2")
rownames(data.p) <- c("")
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
print(xtable(data.p,
             align = "|l|r|r|r|"),
      inculde.rownames = FALSE,
      comment=FALSE,
      sanitize.colnames.function=bold,
      sanitize.rownames.function=bold,
      booktabs=F,
      floating = TRUE, latex.environments = "center")






