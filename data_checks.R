###################################################################
# Check input data to make sure they follow input data guidelines
###################################################################
valid_input_data<- function(dat_file){

  # Set some data check flags
  #flagPriceColNotFirst = FALSE
  flagLastRowNAData = FALSE
  flagNotEnoughData = FALSE
  flagMissingData = FALSE
  flagDataQualityGood = TRUE
  
  # Remove all rows with all NAs
  dataIn <- read.csv(dat_file,header = T)
  dataIn <- dataIn[!(rowSums(is.na(dataIn))==ncol(dataIn)),]
  
  # Check to see if there is enough data
  if(NROW(dataIn)<2){ # 1 row of data cannot be used for prediction
    flagNotEnoughData = TRUE
  }
  
  # Check data file format. First column should be the Dependant variable (DV), the rest Independant variables (IV)
  '%ni%' <- Negate('%in%') # Define NotIn 
  # if("SellingPrice" %ni% colnames(dataIn)){
  #   # 1) First check if SellingPrice is there. If not quit.
  #   cat("Unable to find 'SellingPrice'. Check the data file.\n");
  #   flagMissingPrice = TRUE
  # } else 
  # if ('SellingPrice' %ni% colnames(dataIn)[1]){ 
  #   # 2) Next check if SellingPrice is the first column, if not quit.
  #   cat("'SellingPrice' should be the first column in dataframe. Check the data file.\n");
  #   flagPriceColNotFirst = TRUE
  # } else 
    
  if(rowSums(is.na(dataIn))>1){
    # 3) Check to see if the last row contains data for prediction. SellingPrice should be the only NA there
    cat("There is more than 1 missing data value in the last row containing the data used for rediction.\n");
    flagLastRowNAData = TRUE
  }
  
  # Last row contains the data to be used for prediction. So it will have NA for the price. Therefore skip the last 
  # row when checking for NA's in indicidual IVs
  null.row <- rowSums(is.na(dataIn))
  
  if(all(colSums(is.na(dataIn[1:nrow(dataIn)-1,]))!=0)){
    cat("There are missing data.\n");
    flagMissingData = TRUE
  }
  
  if(flagNotEnoughData || flagLastRowNAData || flagMissingData){
    cat("Failed input data quality check. See output summary for details.")
    flagDataQualityGood = FALSE
    #quit()
  }
  
  return (flagDataQualityGood)
}