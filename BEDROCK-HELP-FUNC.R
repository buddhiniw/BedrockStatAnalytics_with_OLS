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


options(knitr.table.format = "html") 



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


## @knitr descriptive_stat_pdf_table
print(xtable(data.t,
             align= "|l|c|r|r|r|r|",
             caption = "Summary statistics. Note that Mean and Std. Dev. are meaningless for categorical variables."),
      comment=FALSE,
      sanitize.colnames.function=bold,
      sanitize.rownames.function=bold,
      booktabs=F,
      floating = TRUE, latex.environments = "center", type="latex")

## @knitr descriptive_stat_html_table
kable(data.t)%>%
  kable_styling(bootstrap_options = c("striped", "hover"))

###################################################################
# Make predictions using the final model selected by caret
###################################################################

## @knitr coefficients_table
data.c <- as.matrix(beta.hat$coefficients)
data.c <- formatC(data.c, digits = 3, format = "f", flag = "0")
colnames(data.c) <- c("Estimate")
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}

## @knitr coefficients_html_table
kable(data.c)%>%
  kable_styling(bootstrap_options = c("striped", "hover"))


## @knitr ols_coef_tble
data.c <- as.matrix(beta.hat)
data.c <- formatC(data.c, digits = 3, format = "f", flag = "0")
#colnames(data.c) <- c("Estimate")
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
## @knitr ols_coef_tble
kable(data.c,format = "markdown")


# Predicted Price
## @knitr predict_price
data.p <- as.matrix(cbind(y.hat,prediction.error, prediction.rsquared))
data.p <- formatC(data.p, digits = 3, format = "f", flag = "0")
colnames(data.p) <- c("Predicted Value", "Prediction Error", "R2")
rownames(data.p) <- c("")
bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
#data.p <-as.data.frame(data.p)
## @knitr predict_price_pdf
print(xtable(data.p,
             align = "|l|r|r|r|"),
      inculde.rownames = FALSE,
      comment=FALSE,
      sanitize.colnames.function=bold,
      sanitize.rownames.function=bold,
      booktabs=F,
      floating = TRUE, latex.environments = "center")

## @knitr predict_price_html
kable(data.p)%>%
  kable_styling(bootstrap_options = c("striped", "hover"))


## @knitr data_summary
#landscape(kable(dataIn, format = "html",longtable = TRUE))



