#########
# ENSAMBLE
#########

library(C50)
library(rpart)
library(ranger)

# abril_dataset = db.getDataset(db.TERNARIA, F)
abril_dataset = db.getBigDataset()
claseIndex = which( colnames(abril_dataset) == "clase" )

trainModels = list (
  # c50 = function(trainIds) {
  #   df = abril_dataset[ trainIds,]
  #   dfTest = abril_dataset[ -trainIds,]
  # 
  #   vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )
  # 
  #   cf = 0.001
  #   minCases = 400
  #   vtrials = 2
  #   
  #   model = C5.0(  x = df[ , -claseIndex2],
  #                  y = df[ , claseIndex2],
  #                  weights = vweights,
  #                  rules = F,
  #                  trials = vtrials,
  #                  control = C5.0Control(CF = cf, minCases = minCases) )
  # 
  #   predict(  model, dfTest , type = "prob")
  # },
  rpart = function(trainIds) {
    # df = db.nonulls( abril_dataset[ trainIds,] )
    # dfTest = db.nonulls(abril_dataset[ -nIds,] )
    df = abril_dataset[ trainIds,]
    dfTest = abril_dataset[ -trainIds,]
    
    cat("Rpart... ")
    
    vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )
    
    vcp = 0.005	
    vminsplit = 400	
    vminbucket = 1
    vmaxdepth = 6
    
    model = rpart( clase ~ . ,
                   weights = vweights,
                   data = df,
                   method="class", 
                   xval=0, 
                   maxsurrogate=1, 
                   surrogatestyle=1, 
                   x = F,
                   y = F,
                   cp=vcp, 
                   minsplit=vminsplit, 
                   minbucket=vminbucket, 
                   maxdepth=vmaxdepth ) 
    
    result = predict(  model, dfTest, type = "prob")
    
    rm(df, dfTest,vweights,model)
    gc()
    
    cat("done","\n")
    
    result
  },
  ranger = function(trainIds) {
    df = db.nonulls( abril_dataset[ trainIds,] )
    dfTest = db.nonulls( abril_dataset[ -trainIds,] )
    
    canttrees = 200
    vmin.node.size = 2500
    vimportance = "impurity"
    
    vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )
  
    model = ranger( 
      dependent.variable.name = "clase",
      data = df, 
      num.trees = canttrees,
      importance = vimportance,
      case.weights = vweights,
      num.threads = 1,
      min.node.size = vmin.node.size,
      probability = T
      # save.memory = T
    )
    
    result = predict(  model, dfTest , type = "response")
    
    rm(df, dfTest,vweights,model)
    gc()
    
    result$predictions
  }
)

pesosEnsamble = rbind(
  c(0.50,0.50),
  c(0.45,0.55),
  c(0.40,0.60),
  c(0.35,0.65),
  c(0.55,0.45),
  c(0.60,0.40)
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
  
  log.add.ensamble("joined_new", paste(names(trainModels), collapse = " "), paste(pesosEnsamble[p,], collapse = " "), ganancias, tiempos)
}


