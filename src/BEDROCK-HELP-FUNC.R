#############################
# Helper functions to create Tables For MS Word .docx
#############################
#  Author by B. Waidyawansa (10/10/2018)


library(flextable)
library(officer)
library(tidyverse)
library(prettyR)

# Descriptive Statistics
## @knitr descriptive_sat

# Get the original data frame minus the test data set
df<-dataIn[1:(nrow(dataIn)-1),]
data <- as.data.frame(describe(df ,num.desc=c("valid.n","mean","sd","min","max"))$Numeric)
data.t <- as.matrix(t(data))
data.t <- formatC(data.t, digits = 2, format = "d", flag = "0")
colnames(data.t) <- c("N", "Mean", "Std. Dev.", "Min","Max")

# Correlations between predictors
## @knitr corr_plot
corrplot::corrplot(cov(as.matrix(x.scaled)),method = "number")
#dev.off()





# create flextable of coefficients from enet without errors
flextable.enet.coef <- function(beta.hat.scaled) {
  
  data <- as.matrix(beta.hat.scaled$coefficients)
  # format the data values
  ftable <- flextable(rownames_to_column(as.data.frame(data)))
  ftable <- set_header_labels(ftable,rowname = "Variable", V1="Estimate")
  
  ftable <- fontsize(ftable, size = 12, part = "all")
  ftable <- autofit(ftable)
  ftable <- theme_zebra(ftable)
  
}


# create a FlexTable of predicted value and prediction error
flextable.predict <- function(value,error, r2){
  data <- as.matrix(cbind(value,error,r2))
  ftable <- flextable(as.data.frame(data))
  ftable <- set_header_labels(ftable,V1 = "Predicted Value", error="Error", r2 = "R2")
  
  ftable <- fontsize(ftable, size = 12, part = "all")
  ftable <- autofit(ftable)
  ftable <- theme_zebra(ftable)
}

