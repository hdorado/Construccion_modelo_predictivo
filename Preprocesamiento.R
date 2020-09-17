
# Procesamiento y analisis de datos Guanajuato

library(caret)
library(ggplot2)
library(lubridate)


dataSetProcess <- function(dataset,rm.vars = NULL ){
  
  require(caret)
  
  # Transformation and filtering
  
  removeVars <- NULL
  

  nz <- nearZeroVar(dataset)

  dataset <- dataset[,-which(names(dataset) %in% rm.vars)]


  if(sum(nz)!=0){ 
    removeVars <- paste('zero Variance: ', paste(names(dataset)[nz],
                                                 collapse = ',')) 
    
    dataset <- dataset[,-nz] 
    
  }

  numDataset  <- dataset[sapply(dataset,is.numeric)]

  cualDataset <- dataset[sapply(dataset,function(w){!is.numeric(w)})]

  # correlacion mayor a 7

  HighCorr <- findCorrelation(cor(numDataset),cutoff = 0.7)

  if(sum(HighCorr)!=0){
    
    removeVars <- c(removeVars,paste('High correl: ',
                                     paste(names(numDataset)[HighCorr],
                                           collapse = ',')))
    numDataset <- numDataset[,-HighCorr] 
 
  }


  

  dataset <- data.frame(cualDataset,numDataset)

  dataset_transf <- model.matrix(Rendimiento ~ ., data = dataset)

  dataset_transf <- data.frame(dataset_transf[,-1],Yield = dataset$Rendimiento)

  list(data=dataset_transf,vars.rm = removeVars)

}
