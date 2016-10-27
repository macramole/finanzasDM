#########
# ENSAMBLE
#########

library(C50)
library(rpart)
library(ranger)

abril_dataset = db.getDataset(db.TERNARIA, F)
claseIndex = which( colnames(abril_dataset) == "clase" )

trainModels = list (
  c50 = function(trainIds) {
    df = abril_dataset[ trainIds,]
    dfTest = abril_dataset[ -trainIds,] 
    
    vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )

    cf = 0.001
    minCases = 400
    vtrials = 2

    model = C5.0(  x = df[ , -claseIndex],
                   y = df[ , claseIndex],
                   weights = vweights,
                   rules = F,
                   trials = vtrials,
                   control = C5.0Control(CF = cf, minCases = minCases) )

    predict(  model, dfTest , type = "prob")
  },
  ranger = function(trainIds) {
    df = db.nonulls( abril_dataset[ trainIds,] )
    dfTest = db.nonulls( abril_dataset[ -trainIds,] )
    
    canttrees = 500
    vmin.node.size = 1000
    vimportance = "impurity"
    
    vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )
    
    t0 =  Sys.time()  
    model = ranger( 
      dependent.variable.name = "clase",
      data = df, 
      num.trees = canttrees,
      importance = vimportance,
      case.weights = vweights,
      num.threads = 2,
      min.node.size = vmin.node.size,
      probability = T
      # save.memory = T
    )
    t1 =  Sys.time()
    tiempos[s] = as.numeric(  t1 - t0, units = "secs" )
    
    result = predict(  model, dfTest , type = "response")
    result$predictions
  },
  rpart = function(trainIds) {
    df = db.nonulls( abril_dataset[ trainIds,] )
    dfTest = db.nonulls(abril_dataset[ -trainIds,])
    
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
    
    predict(  model, dfTest, type = "prob")
  }
)

pesosEnsamble = rbind(
  c(0.3,0.3,0.4),
  c(0.3,0.4,0.3),
  c(0.4,0.3,0.3)
)

sum(pesosEnsamble)

ganancias = c()
tiempos = c()

for ( p in 1:nrow(pesosEnsamble) ) {
  # p = 1
  # s = 1
  
  paste(pesosEnsamble[p,], collapse = " ")
  
  for( s in 1:5 ) {
    t0 =  Sys.time()  
    
    #armo datasets
    set.seed( seeds[s] )
    abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
    
    predictions = list()
    
    for( t in 1:length(trainModels) ) {
      predictions[[t]] = trainModels[[t]](abril_inTraining) * pesosEnsamble[p,t] #prediciones ya ponderadas
    }
    
    ensamblePrediction = Reduce('+', predictions)
    
    ganancias[s] = ganancia.ternaria(ensamblePrediction, abril_dataset[-abril_inTraining, ]$clase, 0.5) / 0.3 #c(0.55,0.45) 1491667
    
    t1 =  Sys.time()
    tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
    
    cat(ganancias[s]," | ",tiempos[s],"\n")
  }
  
  log.add.ensamble("abril_visamaster", paste(names(trainModels), collapse = " "), paste(pesosEnsamble[p,], collapse = " "), ganancias, tiempos)
}


