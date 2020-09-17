
library(lubridate)

rm(list=ls())

source('Preprocesamiento.R')

datasetIni <- read.csv('DATA/dataset.csv',row.names=1)

datasetIni <- datasetIni[complete.cases(datasetIni),]

datasetIni$fecha_de_siembra <- as.Date(datasetIni$fecha_de_siembra,"%m/%d/%Y")

ref.vars <- c('fecha_de_siembra','latitud_n','longitud_w')

dSetProcess <- dataSetProcess(datasetIni,ref.vars) # Este paso se puede saltar si hay un dataset mixto (categoricas + cualitativas)

dSetProcess$vars.rm

dSetProcess <- data.frame(fecha_de_siembra=datasetIni[,'fecha_de_siembra'],dSetProcess$data)

dSetProcess$Year <- year(dSetProcess$fecha_de_siembra)

table(dSetProcess$Year)

dSetProcess <- dSetProcess[dSetProcess$Year!=2019,] # Remuevo el ultimo año por pocos registros

# Comenzar en el año cuarto y hacerlo año tras año (podemos hacerlo mes a mes)

years <- unique(dSetProcess$Year) 

timeDis <- createTimeSlices(years,4,1, fixedWindow = F)

trainYears <- lapply(timeDis$train,function(x){years[x]})

valYears <- lapply(timeDis$test,function(x){years[x]})

# extract positions

trainYearsPos <- lapply(trainYears ,function(x){which(dSetProcess$Year %in% x)})
  
valYearsPos <- lapply(valYears ,function(x){which(dSetProcess$Year %in% x)})

# Splite dataset 

dSetProcess <- dSetProcess[,-which(names(dSetProcess) %in% c("Year","fecha_de_siembra"))]

trainDSList <- lapply(trainYearsPos,function(x){dSetProcess[x,]})

testDSList  <- lapply(valYearsPos,function(x){dSetProcess[x,]})

set.seed(123)

trainModel <- function(datasetTrain){
  
  dimMat <- ncol(datasetTrain)
      
  model_train <- train(x = datasetTrain[,-dimMat], y = datasetTrain[,dimMat], 
                            trControl = trainControl(method = 'repeatedcv',
                                              number = 3,repeats = 2),
                       model='rf')
  model_train
  
}

ModelsTrained <- lapply(trainDSList, trainModel)


performance <- lapply(seq(length(ModelsTrained)),
                      
       function(w){
         cat(w)
          prVa <- predict(ModelsTrained[[w]],testDSList[[w]])
          postResample(prVa,testDSList[[w]]$Yield)
      }
)

do.call(rbind,performance)

