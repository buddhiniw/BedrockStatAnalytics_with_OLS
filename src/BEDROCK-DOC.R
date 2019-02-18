#############################
# Load required packages
#############################
#  Author by B. Waidyawansa (10/10/2018)

library(officer)
library(flextable)
# Package `magrittr` makes officer usage easier.
library(magrittr)


###################################################################
# Descriptive statistics from unscaled data
###################################################################
print('Get Descriptive Stats')
# Get the original data frame minus the test data set
df=dataIn[1:(nrow(dataIn)-1),]
# Prepare Descriptive Satistics FlexTable
dataIn.desc.ft=flextable.descriptive(df)


###################################################################
# Check for correlations between predictors
###################################################################
print('Check for Correlations in Predictors')
corrplot::corrplot(cov(as.matrix(x.scaled)),method = "number")
src1  <- paste0(dirname(sys.frame(1)$ofile),"/out/cplot.png")
png(filename = src1, width = 6, height = 6, units = 'in', res = 300)
corrplot::corrplot(cov(as.matrix(x.scaled)),method = "number")
dev.off()

###################################################################
# Selection of Lambda and S/alpha
###################################################################
print('Select Best Lambda and s Using LOOCV')
src2  <- paste0(dirname(sys.frame(1)$ofile),"/out/loocvplot.png")
png(filename = src2, width = 6, height = 6, units = 'in', res = 300)
par(mar = c(5,5,6,5))
print(plot(train.enet,plotType = "line", xlab="Fraction (s)",scales=list(x=list(cex=0.75), y=list(cex=0.75))))
trellis.par.set(caretTheme())
dev.off()


###################################################################
# Plot Variable Importance
###################################################################
print('Plot variable importance')
src3  <- paste0(dirname(sys.frame(1)$ofile),"/out/varimpplot.png")
png(filename = src3, width = 6, height = 6, units = 'in', res = 300)
print(plot(varImp(train.enet)))
dev.off()

################ Model coefficients ################
print('Extract Model Coefficients')
coef.ft<-flextable.enet.coef(beta.hat.enet.scaled)

################ MODEL PREDICTION ################
print('Make Predictions')
predict.ft <- flextable.predict(y.hat.enet.unscaled,prediction.error, prediction.rsquared)



###################################################################
#  Prepare to generate MS Word Document
###################################################################
fileName=gsub(".csv$",".docx",inFileName)
fileName=gsub("data","out",fileName)
outfileName = fileName

# Create a new word document
print('Open new doccument to save output')
doc <- read_docx() %>%
  ### heading
  body_add_fpar(fpar( ftext("Real Estate Price Prediction with Elastic-net Regression", 
                            prop = fp_text(font.size = 30)), 
                            fp_p = fp_par(text.align = "center") )) %>%
  body_add_break() %>%
  body_add_fpar(fpar( ftext(paste("Data set:",args[1],collapse=" "), 
                            prop = fp_text(font.size = 20)), 
                      fp_p = fp_par(text.align = "center") )) %>%
  ####
  body_end_section_continuous() %>%
  body_add_par("Basic summary statistics",style = "heading 1") %>%
  body_add_flextable(dataIn.desc.ft) %>%
  body_add_par(" NOTE - No summary statistics are provided for categorical variables.",
               style = "Normal") %>%
  ####
  body_add_par("Correlations Between Predictors",style = "heading 2" ) %>%
  body_add_img(src = src1, width = 5, height = 5, style = NULL, pos = "after") %>%
  ####
  body_add_par("Tuning Parameter Selection Using LOOCV",style = "heading 2" ) %>%
  body_add_img(src = src2, width = 5, height = 5, style = NULL, pos = "after") %>%
  body_add_par(sprintf("\nFrom above plot, lambda =%.2f and s =%.2f gives the minimum RMSE model."
                        ,best.lambda, best.fraction),style = "Normal") %>%
  ####
  body_add_par("Plot Variable Importance",style = "heading 2" ) %>%
  body_add_img(src = src3, width = 5, height = 4, style = NULL, pos = "after") %>%
  ####
  body_add_par("Standardized Model Coefficients",style = "heading 2" ) %>%
  body_add_flextable(coef.ft) %>%
  body_add_par("NOTE std. errors are calculated using bootstrapping which is the only way to determine coef. errors for a penalized regression. But the errors should be only used for reference. It is yet unclear how meaningful the std. errors are in penalized regression."
                , style = "Normal") %>%
  ####
  body_add_par("Model Prediction",style = "heading 2" ) %>%
  body_add_flextable(predict.ft) %>%
    

print(doc, target = outfileName )

print('Done!')
