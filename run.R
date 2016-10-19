library(doMC)

registerDoMC(3)

run = function(dataset, dataset_desc = "", funcGanancia = ganancia.ternaria, vcpValues, vminsplitValues, vminbucketValues, vmaxdepthValues) {
  vcp <- 0
  vminsplit <- 0
  vminbucket <- 0
  vmaxdepth <- 0
  
  totalTime0 =  Sys.time()  
  
  # models = foreach( vcp = vcpValues ) %dopar%
  #for ( vminbucket in vminbucketValues ) {
  
  # Guarda que acá conviene usar length(vminbucketValues) >= 3 para que use los 3 procesadores
  models = foreach( vminbucket = vminbucketValues ) %dopar% {
    for ( vcp in vcpValues ) {
      for ( vminsplit in vminsplitValues ) {
        for ( vmaxdepth in vmaxdepthValues ) {
          tiempos = c()
          ganancias = c()
          
          
          
          # for( s in  1:length(seeds) )
          for( s in  1:4 ) {
            # models = foreach( s = 1:2 ) %dopar% {
            set.seed( seeds[s] )
            abril_inTraining <- createDataPartition( dataset$clase, p = .70, list = FALSE)
            abril_dataset_training <- dataset[ abril_inTraining,]
            abril_dataset_testing  <- dataset[-abril_inTraining,]
            
            t0 =  Sys.time()  
            
            abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset_training,   cp=vcp, minsplit=vminsplit, minbucket=vminsplit/vminbucket, maxdepth=vmaxdepth, x = F, y = F )
            
            t1 =  Sys.time()
            tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
            
            abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")
            ganancias[s] <- funcGanancia( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
            # ganancia.ternaria( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
          }
          
          
          #tiempos = as.numeric(  t1 - t0, units = "secs" )
          
          log.add(dataset_desc, vcp, vminsplit, vminbucket, vmaxdepth, ganancias, tiempos )
        }
      }
    }
  }
  
  totalTime1 =  Sys.time()
  
  cat("Run tardó", as.numeric(  totalTime1 - totalTime0, units = "mins" ) )
}


run.noParallel = function(dataset, dataset_desc = "", funcGanancia = ganancia.ternaria, vcpValues, vminsplitValues, vminbucketValues, vmaxdepthValues) {
  vcp <- 0
  vminsplit <- 0
  vminbucket <- 0
  vmaxdepth <- 0
  # 
  # vcp = c(0.001)
  # vminsplit = c(20)
  # vminbucket = c(2) #padre / cada número
  # vmaxdepth = c(11)
  
  
  totalTime0 =  Sys.time()  
  
  for ( vminbucket in vminbucketValues ) {
    for ( vcp in vcpValues ) {
      for ( vminsplit in vminsplitValues ) {
        for ( vmaxdepth in vmaxdepthValues ) {
          tiempos = c()
          ganancias = c()
          
          # for( s in  1:length(seeds) )
          for( s in  1:4 ) {
            cat("Semilla: ",s, fill = T)
            set.seed( seeds[s] )
            abril_inTraining <- createDataPartition( dataset$clase, p = .70, list = FALSE)
            abril_dataset_training <- dataset[ abril_inTraining,]
            abril_dataset_testing  <- dataset[-abril_inTraining,]
            
            t0 =  Sys.time()  
            
            abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset_training,   cp=vcp, minsplit=vminsplit, minbucket=vminsplit/vminbucket, maxdepth=vmaxdepth, xval = 0, x = F, y = F )
            
            t1 =  Sys.time()
            tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
            cat("Tardó:",tiempos[s], fill = T)
            
            abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")
            ganancias[s] <- funcGanancia( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
            # ganancia.ternaria( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
          }
          
          
          #tiempos = as.numeric(  t1 - t0, units = "secs" )
          
          log.add(dataset_desc, vcp, vminsplit, vminbucket, vmaxdepth, ganancias, tiempos )
        }
      }
    }
  }
  
  totalTime1 =  Sys.time()
  
  cat("Run tardó", as.numeric(  totalTime1 - totalTime0, units = "mins" ), "minutos" , fill = T)
  # abril_modelo
}



#devuelve el modelo
run.one.noSplit = function(dataset, vcp, vminsplit, vminbucket, vmaxdepth) {
  t0 =  Sys.time()  
  abril_modelo  <- rpart( clase ~ .   ,   data = dataset, cp=vcp, minsplit=vminsplit, minbucket=vminsplit/vminbucket, maxdepth=vmaxdepth, xval = F )
  t1 =  Sys.time()
  cat("Tiempo:", as.numeric(  t1 - t0, units = "secs" ))
  abril_modelo   
}