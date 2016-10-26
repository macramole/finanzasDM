#########
# ENSAMBLE
#########

library(C50)
library(rtree)

abril_dataset = db.getDataset(db.TERNARIA, F)
claseIndex = which( colnames(abril_dataset) == "clase" )

pesosEnsamble = rbind(
  # c(0.5,0.5), 
  # c(0.45,0.55)
  c(0.4, 0.6),
  c(0.3, 0.7)
)

sum(pesosEnsamble)

##C5
cf = 0.001
minCases = 400
##RTree
vcp = 0.005	
vminsplit = 400	
vminbucket = 1
vmaxdepth = 6

ganancias = c()
tiempos = c()

for ( p in 1:nrow(pesosEnsamble) ) {
  paste(pesosEnsamble[p,1], pesosEnsamble[p,2])
  
  for( s in 1:5 ) {
    t0 =  Sys.time()  
    
    #armo datasets
    set.seed( seeds[s] )
    abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
    abril_dataset_training    <- abril_dataset[ abril_inTraining,]
    abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
    
    #Asigno pesos <7750, 250>  es equivalente a  <31, 1>
    vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )
    
    model.1 <- C5.0(  x = abril_dataset_training[ , -claseIndex],
                      y = abril_dataset_training[ , claseIndex],
                      weights = vweights,
                      rules = F,
                      trials = 1,
                      control = C5.0Control(CF = cf, minCases = minCases) 
    )
    
    abril_dataset_training_noNulls = db.nonulls(abril_dataset_training)
    # db.cantnulls(abril_dataset_training_noNulls)
    model.2 <- rpart( clase ~ .,
                      data = abril_dataset_training_noNulls,
                      method="class", 
                      xval=0, 
                      maxsurrogate=1, 
                      surrogatestyle=1, 
                      x = F,
                      y = F,
                      weights = vweights,
                      cp=vcp, 
                      minsplit=vminsplit, 
                      minbucket=vminbucket, 
                      maxdepth=vmaxdepth ) 
    
    #calculo la ganancia en TESTING
    model.1.prediction.testing = predict(  model.1, abril_dataset_testing , type = "prob")
    abril_dataset_testing_noNulls = db.nonulls(abril_dataset_testing)
    model.2.prediction.testing = predict(  model.2, abril_dataset_testing_noNulls , type = "prob")
    
    # ganancia.ternaria(model.1.prediction.testing, abril_dataset_testing$clase, 0.5) / 0.3
    # ganancia.ternaria(model.2.prediction.testing, abril_dataset_testing$clase, 0.5) / 0.3
    
    # pesosEnsamble = c(0.55,0.45)
    # sum(pesosEnsamble)
    ensamble.prediction.testing = ( model.1.prediction.testing * pesosEnsamble[p,1] + model.2.prediction.testing * pesosEnsamble[p,2] )
    ganancias[s] = ganancia.ternaria(ensamble.prediction.testing, abril_dataset_testing$clase, 0.5) / 0.3 #c(0.55,0.45) 1491667
    
    t1 =  Sys.time()
    tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
    
    cat(ganancias[s]," | ",tiempos[s],"\n")
  }
  
  log.add.ensamble("abril_visamaster", "c50_rpart", paste(pesosEnsamble[p,1], pesosEnsamble[p,2]), ganancias, tiempos)
}


