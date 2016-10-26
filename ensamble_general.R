#########
# ENSAMBLE
#########

library(C50)
library(rtree)

abril_dataset = db.getDataset(db.TERNARIA, F)
claseIndex = which( colnames(abril_dataset) == "clase" )

trainModels = list (
  c50 = function(trainIds, dfTest) {
    df = abril_dataset[ trainIds,]
    
    vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )
    
    cf = 0.001
    minCases = 400
    
    model = C5.0(  x = df[ , -claseIndex],
                   y = df[ , claseIndex],
                   weights = vweights,
                   rules = F,
                   trials = 1,
                   control = C5.0Control(CF = cf, minCases = minCases) )
    
    predict(  model, dfTest , type = "prob")
  },
  rpart = function(trainIds, dfTest) {
    df = db.nonulls( abril_dataset[ trainIds,] )
    
    vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )
    
    vcp = 0.005	
    vminsplit = 400	
    vminbucket = 1
    vmaxdepth = 6
    
    model = rpart( clase ~ .,
                   data = df,
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
    
    dfTest_noNulls = db.nonulls(dfTest)
    predict(  model, dfTest_noNulls , type = "prob")
  }
)

pesosEnsamble = rbind(
  c(0.5,0.5), 
  c(0.45,0.55)
)

sum(pesosEnsamble)

ganancias = c()
tiempos = c()

for ( p in 1:nrow(pesosEnsamble) ) {
  cat(pesosEnsamble[p,1], pesosEnsamble[p,2], "\n")
  
  for( s in 1:5 ) {
    t0 =  Sys.time()  
    
    #armo datasets
    set.seed( seeds[s] )
    abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
    
    predictions = list()
    
    for( t in 1:length(trainModels) ) {
      predictions[[t]] = trainModels[[t]](abril_dataset_training, vweights) * pesosEnsamble[p,t] #prediciones ya ponderadas
    }
    
    ensamblePrediction = Reduce('+', predictions)
    
    ganancias[s] = ganancia.ternaria(ensamblePrediction, abril_dataset_testing$clase, 0.5) / 0.3 #c(0.55,0.45) 1491667
    
    t1 =  Sys.time()
    tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
    
    cat(ganancias[s]," | ",tiempos[s],"\n")
  }
  
  log.add.ensamble("abril_visamaster", paste(names(trainModels), collapse = " "), paste(pesosEnsamble[p,], collapse = " "), ganancias, tiempos)
}


