library(rpart)
library(caret)
library(doMC)

registerDoMC(4)

seeds <- c( 442619, 664579, 235813 , 502841, 351551 )

#clase ternaria
abril_dataset = db.getDataset()


#model params
vcp <- 1 ;
vminsplit <- 50 ;
vminbucket <- 8 ;
vmaxdepth <- 8

vcpValues = c(0, 0.0001, 0.001, 0.005)
vminsplitValues = c(20,50,200)
vminbucketValues = c(2,5) #padre / cada número
vmaxdepthValues = c(5,10,15,20)

vcpValues = c(0)
vminsplitValues = c(18,19,20,21,22)
vminbucketValues = c(1,2,3) #padre / cada número
vmaxdepthValues = c(9,10,11,12,13)

table(abril_dataset$clase)


# models = foreach( vcp = vcpValues ) %dopar%
for ( vcp in vcpValues ) {
  for ( vminsplit in vminsplitValues ) {
    for ( vminbucket in vminbucketValues ) {
      for ( vmaxdepth in vmaxdepthValues ) {
        tiempos = c()
        ganancias = c()
        
        
        
        # for( s in  1:length(seeds) )
        for( s in  1:4 ) {
        # models = foreach( s = 1:2 ) %dopar% {
          set.seed( seeds[s] )
          abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
          abril_dataset_training <- abril_dataset[ abril_inTraining,]
          abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
          
          t0 =  Sys.time()  
          
          abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset_training,   cp=vcp, minsplit=vminsplit, minbucket=vminsplit/vminbucket, maxdepth=vmaxdepth, x = F, y = F )
          
          t1 =  Sys.time()
          tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
          
          abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")
          ganancias[s] <- ganancia.ternaria( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
          # ganancia.ternaria( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
        }
        
        
        #tiempos = as.numeric(  t1 - t0, units = "secs" )
        
        log.add("ternaria_noparallel", vcp, vminsplit, vminbucket, vmaxdepth, ganancias, tiempos )
      }
    }
  }
}












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

fit.ternaria = ?train( x = abril_dataset[,-ncol(abril_dataset)], 
                       y = abril_dataset[,ncol(abril_dataset)], 
                      method = "rpart2", 
                      cp = 0 )
                      #trControl = ctrl, 
                      #verbose = TRUE, 
                      #tuneGrid = paramsGrid)

fit.ternaria
=