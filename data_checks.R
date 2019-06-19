###################################################################
# Check input data to make sure they follow input data guidelines
###################################################################
valid_input_data<- function(dat_file){

  # Default flags
  flagNotEnoughData = FALSE
  flagMissingTestData = FALSE
  flagMissingPredData = FALSE
  flagMissingPredPrice = FALSE
  flagDataQualityGood = TRUE
  
  # Read data file
  dataIn <- read.csv(dat_file,header = T)
  
  # Remove rows with all NAs
  ind <- apply(dataIn, 1, function(x) all(is.na(x)))
  dataIn <- dataIn[!ind,]
  
  #dataIn <- dataIn[!(rowSums(is.na(dataIn))==ncol(dataIn)),]
  
  # Check to see if there is enough data
  # There should be at least 3 rows of test data to run this app
  if(NROW(dataIn)<3){ 
    flagNotEnoughData = TRUE
  }
  
  # Define NotIn function
  '%ni%' <- Negate('%in%') # Define NotIn 

  #########################################
  # Check data file format. 
  # First column should be the Dependant variable (DV). The rest Independant variables (IV)
  # Last row should have N/A for price/rent to be predicted. 
  # The rest of the rows should have test data to train the model
  ##########################################
  # Are there N/A in the test data (all rows except last one) ?
  if(sum(colSums(is.na(dataIn[1:nrow(dataIn)-1,]))!=0)){
    cat("There are missing data in the test data set.\n");
    flagMissingTestData = TRUE
  }
  
  # Are there N/A in the data used for prediction (except in price/rent)?
  if(sum(rowSums(is.na(dataIn[,2:ncol(dataIn)]))!=0)){
    cat("There are missing data in the predict data set\n.");
    flagMissingPredData = TRUE
  }
  
  # Is there a N/A for price/rent in the last row (We need N/A there to predic price/rent)?
  if(is.na(dataIn[nrow(dataIn),1]) != TRUE){
    cat("There is something wrong with the predic data set (last row)\n.");
    flagMissingPredPrice = TRUE
  }
  

  # Calculate the quality status
  if(flagNotEnoughData || flagMissingTestData || flagMissingPredData || flagMissingPredPrice ){
    cat("Failed input data quality check. See output summary for details.")
    flagDataQualityGood = FALSE
  }
  
  return (flagDataQualityGood)
}