#########
# ENSAMBLE
#########

# library(C50)
# library(rpart)
library(ranger)
library(xgboost)

# abril_dataset = db.getDataset(db.TERNARIA, F)
# abril_dataset = db.getBigDataset()
df.all = db.getDatasetImportantes(discret = F)
# df.training = NULL
claseIndex = which( colnames(df.all) == "clase" )

umbralesModels = c(0.05875, 250/8000)
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
  # rpart = function(trainIds) {
  #   # df = db.nonulls( abril_dataset[ trainIds,] )
  #   # dfTest = db.nonulls(abril_dataset[ -nIds,] )
  #   df = abril_dataset[ trainIds,]
  #   dfTest = abril_dataset[ -trainIds,]
  #   
  #   cat("Rpart... ")
  #   
  #   vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )
  #   
  #   vcp = 0.005	
  #   vminsplit = 400	
  #   vminbucket = 1
  #   vmaxdepth = 6
  #   
  #   model = rpart( clase ~ . ,
  #                  weights = vweights,
  #                  data = df,
  #                  method="class", 
  #                  xval=0, 
  #                  maxsurrogate=1, 
  #                  surrogatestyle=1, 
  #                  x = F,
  #                  y = F,
  #                  cp=vcp, 
  #                  minsplit=vminsplit, 
  #                  minbucket=vminbucket, 
  #                  maxdepth=vmaxdepth ) 
  #   
  #   result = predict(  model, dfTest, type = "prob")
  #   
  #   rm(df, dfTest,vweights,model)
  #   gc()
  #   
  #   cat("done","\n")
  #   
  #   result
  # },
  ranger = function(trainIds) {
    discretized = db.discretize.soft(df.all)
    discretized = db.nonulls(discretized)
    
    df = discretized[ trainIds,]
    dfTest = discretized[ -trainIds,] 
    
    canttrees = 300
    vmin.node.size = 100
    vimportance = "impurity"
    
    # vweights <- ifelse( df$clase =='BAJA+2', 31, 1 )
  
    model = ranger( 
      dependent.variable.name = "clase",
      data = df, 
      num.trees = canttrees,
      importance = vimportance,
      # case.weights = vweights,
      num.threads = 4,
      min.node.size = vmin.node.size,
      probability = T
      # save.memory = T
    )
    
    result = predict(  model, dfTest , type = "response")
    
    rm(df, dfTest,model, discretized)
    gc()
    
    predictions = result$predictions
    predictions[,2]
  },
  xgboost = function(trainIds) {
    dfNoNulls = db.nonulls( df.all, nullValue = 0 )
    
    #lo paso a binaria
    dfNoNulls$clasebinaria1 = as.factor ( ifelse( dfNoNulls$clase == "BAJA+2", "POS", "NEG" ) )
    dfNoNulls = dfNoNulls[, -claseIndex]
    newClaseIndex = which( colnames(dfNoNulls) == "clasebinaria1" )
    colnames(dfNoNulls)[newClaseIndex] = "clase"
    
    df = dfNoNulls[trainIds, ]
    dfTest = dfNoNulls[-trainIds, ]
    
    vnround = 400
    vmax_depth = 15
    vmin_child_weight = 5
    
    model = xgboost(  data = as.matrix( df[, -claseIndex] ),
                      label = as.numeric( df[, claseIndex] ) - 1,
                      # missing = db.NULL_VALUE,
                      nrounds = vnround,
                      params = list(
                        objective = "multi:softprob",
                        # objective = "binary:logistic",
                        eval_metric = "merror",
                        num_class = 2,
                        eta = 0.01,
                        
                        max_depth = vmax_depth,
                        min_child_weight = vmin_child_weight,
                        max_delta_step = 0,
                        subsample = 0.7,
                        colsample_bytree = 0.4,
                        colsample_bylevel = 1,
                        
                        alpha = 0,
                        lambda = 0.1,
                        gamma = 0.01,
                        
                        # scale_pos_weight = sum( df[training.indexes, claseIndex] == "NEG" ) / sum( df[training.indexes, claseIndex] == "POS" ),
                        nthread = 4
                      )
    )
    
    result = predict(  model, as.matrix(dfTest[, -claseIndex]) )
    result = matrix( data = result, ncol = 2, nrow = nrow( dfTest ), byrow = T )
    result = result[,2]
    
    rm(df, dfTest,model, dfNoNulls, newClaseIndex)
    gc()
    
    result
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

# for ( p in 1:nrow(pesosEnsamble) ) {
  p = 1
  s = 2
  
  # paste(pesosEnsamble[p,], collapse = " ")
  
  for( s in 1:5 ) {
    t0 =  Sys.time()  
    
    #armo datasets
    set.seed( seeds[s] )
    # df.trainingAndValidation.indexes <- createDataPartition( df.all$clase, p = .70, list = FALSE)
    # df.trainingAndValidation  <- df.all[ df.training.indexes,]
    # df.testing  <- df.all[-df.training.indexes,]
    # 
    # df.training.indexes <- createDataPartition( df.trainingAndValidation$clase, p = .70, list = FALSE)
    # df.training <- df.trainingAndValidation[ df.training.indexes, ]
    # df.validation <- df.trainingAndValidation[ -df.training.indexes, ]
    df.indexes = createDataPartition( df.all$clase, p = .70, list = FALSE)
    
    
    # rm(df.training.indexes, df.trainingAndValidation, df.trainingAndValidation.indexes)
    # gc()
    
    predictions = list()
    
    for( t in 1:length(trainModels) ) {
      # predictions[[t]] = trainModels[[t]](df.indexes) * pesosEnsamble[p,t] #prediciones ya ponderadas
      predictions[[t]] = trainModels[[t]](df.indexes)
      
    }
    
    votosPredictions = list()
    
    for( t in 1:length(trainModels) ) {
      # predictions[[t]] = trainModels[[t]](df.indexes) * pesosEnsamble[p,t] #prediciones ya ponderadas
      votosPredictions[[t]] = ifelse( predictions[[t]] > umbralesModels[t], 1, 0 )
      
    }
    
    ensamblePrediction = Reduce('+', votosPredictions) #esto me queda 0 1 2 , 2 es si los dos modelos estan de acuerdo
    
    ganancias[s] = ganancia.ensamble(ensamblePrediction, df.all[-df.indexes, ]$clase, length(trainModels) - 0.1) / 0.3 #c(0.55,0.45) 1491667
    
    t1 =  Sys.time()
    tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
    
    cat(ganancias[s]," | ",tiempos[s],"\n")
    
    rm(predictions, votosPredictions, ensamblePrediction, df.indexes)
    gc()
  }
  
  log.add.ensamble("abril_importantes", paste(names(trainModels), collapse = " "), paste(pesosEnsamble[p,], collapse = " "), ganancias, tiempos)


