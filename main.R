library(rpart)
library(caret)
library(doMC)

registerDoMC(4)

seeds <- c( 442619, 664579, 235813 , 502841, 351551 )

#clase ternaria
abril_dataset = db.getDataset()

#trainingList = c()

#for( s in 1:length(seeds) )
#{
#  set.seed( seed[s] )
#  trainingList = c(trainingList, createDataPartition( abril_dataset$clase, p = .70))
#}
?"%dopar%"



#aca poner bien los parámetros
paramsGrid = expand.grid( cp = c(0, 0.0001, 0.001),
                          #minsplit = c(20,50,200),
                          #minbucket = c(10,25,100,4,10,40),
                          maxdepth = c(5,10,15,20)
                          );

ctrl <- trainControl(method = "LGOCV",
                     number = 1,
                     p = 0.7,
                     #seeds = seeds,
                     #summaryFunction = ganancia.ternaria,
                     #classProbs = TRUE,
                     verboseIter = TRUE
                     #index = trainingList,
                     #savePredictions = TRUE
                     )

fit.ternaria = train( x = abril_dataset[,-ncol(abril_dataset)], 
                       y = abril_dataset[,ncol(abril_dataset)], 
                      method = "rpart2", 
                      cp = 0 )
                      #trControl = ctrl, 
                      #verbose = TRUE, 
                      #tuneGrid = paramsGrid)

fit.ternaria
=