library(xgboost)
library(ROCR)

# df = db.getBigDataset( cual = db.BINARIA1, discret = F)
df = db.getDatasetImportantes( cual = db.BINARIA1, discret = F )
db.NULL_VALUE = 0
df = db.nonulls(df)

claseIndex = which( colnames(df) == "clase" )

# vweights <- ifelse( df.training$clase =='BAJA+2', 31, 1 )

20*4*3*3/60

for(  vmax_depth  in  c( 5, 6 ) )
{
  for(  vmin_child_weight  in  c( 18, 19, 21, 22 ) )
  {
    
    # vmax_depth = 5
    # vmin_child_weight = 15
    # s = 1
    

    tiempos = c()
    ganancias = c()
    umbrales = c()
    aucTraining = c()
    aucTesting = c()
    
    vnround <- 700
    
    for( s in  1:5) {
      # 
      # set.seed( seeds[s] )
      # training.indexes <- createDataPartition( df$clase, p = .70, list = FALSE)
      # df.training <- df[ training.indexes,]
      # df.testing  <- df[-training.indexes,]
      
      set.seed( seeds[s] )
      df.trainingAndValidation.indexes = createDataPartition( df$clase, p = .70, list = FALSE) 
      df.trainingAndValidation = df[ df.trainingAndValidation.indexes, ]
      df.training.indexes = createDataPartition( df.trainingAndValidation$clase, p = .70, list = FALSE) 
      # df.training = df.trainingAndValidation
      df.training = df.trainingAndValidation[ df.training.indexes, ]
      df.validation = df.trainingAndValidation[ -df.training.indexes, ]
      df.testing = df[ -df.trainingAndValidation.indexes, ]
      rm(df.trainingAndValidation, df.trainingAndValidation.indexes, df.training.indexes)
      
      
      t0 =  Sys.time()  
      model = xgboost(  data = as.matrix( df.training[, -claseIndex] ),
                        label = as.numeric( df.training[, claseIndex] ) - 1,
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
      
      for ( i in 1:35 ) {
        training.predict = predict(model, as.matrix( df.training[, -claseIndex] ), ntreelimit= i*20 )
        training.predict = matrix( data = training.predict, ncol = 2, nrow = nrow( df.training ), byrow = T )
        training.predict = training.predict[,2]
        
        training.prediccionROC = prediction(training.predict, df.training$clase)
        # training.performance = performance(training.prediccionROC, measure = "tpr", x.measure = "fpr") 
        #plot(training.performance)
        training.auc = performance(training.prediccionROC, measure = "auc")
        training.auc = training.auc@y.values[[1]]
        aucTraining[i*5 + s] = training.auc
        
        validation.predict = predict(model, as.matrix( df.validation[, -claseIndex] ), ntreelimit= i*20 )
        validation.predict = matrix( data = validation.predict, ncol = 2, nrow = nrow( df.validation ), byrow = T )
        validation.predict = validation.predict[,2]
        
        umbrales[i*5 + s] = umbral_ganancia_optimo( validation.predict, df.validation$clase, seq(0.02,0.1,0.01) )
        
        testing.predict = predict(model, as.matrix( df.testing[, -claseIndex] ), ntreelimit= i*20 )
        testing.predict = matrix( data = testing.predict, ncol = 2, nrow = nrow( df.testing ), byrow = T )
        testing.predict = testing.predict[,2]
        
        testing.prediccionROC = prediction(testing.predict, df.testing$clase)
        # testing.performance = performance(testing.prediccionROC, measure = "tpr", x.measure = "fpr") 
        #plot(testing.performance)
        testing.auc = performance(testing.prediccionROC, measure = "auc")
        testing.auc = testing.auc@y.values[[1]]
        aucTesting[i*5 + s] = testing.auc
        
        ganancias[i*5 + s] = ganancia.binaria1( testing.predict,  df.testing[, claseIndex], umbrales[i*5 + s] ) / 0.3
        # cat (vmax_depth, vmin_child_weight, ganancias[s], "\n")
      }
      
      t1 =  Sys.time()
      
      tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
      paste(tiempos[s] / 60,"minutos")
      
      # ganancias = ganancia.binaria1.umbral ( testing.predict,  df.testing[, claseIndex] )
      
      rm(df.testing, df.training, training.indexes)
      gc()
    }
    
    for( i in 1:35 )
    {
      cat( format(Sys.time(), "%Y%m%d %H%M%S"), 
           "abril_importantes", 
           "0",  
           vmax_depth, 
           vmin_child_weight, 
           i*20, 
           mean( c(ganancias[i*5+1], ganancias[i*5+2], ganancias[i*5+3], ganancias[i*5+4], ganancias[i*5+5] )), 
           mean(tiempos), 
           ganancias[i*5+1], ganancias[i*5+2], ganancias[i*5+3], ganancias[i*5+4], ganancias[i*5+5], 
           umbrales[i*5+1], umbrales[i*5+2], umbrales[i*5+3], umbrales[i*5+4], umbrales[i*5+5],
           mean( c(aucTraining[i*5+1], aucTraining[i*5+2], aucTraining[i*5+3], aucTraining[i*5+4], aucTraining[i*5+5] )),
           mean( c(aucTesting[i*5+1], aucTesting[i*5+2], aucTesting[i*5+3], aucTesting[i*5+4], aucTesting[i*5+5] )),
           "\n", sep="\t", file=log.file.xgboost, fill=FALSE, append=TRUE )
    }
    
    # log.add.xgboost("joined_new", vnround, vmax_depth, vmin_child_weight, tiempos, ganancias)
  }
}

# save(model, file = "xgboost.model")
# load(file = "xgboost.model")
# 
# varImportance = xgb.importance( feature_names = colnames(df), 
#                                 model = model, 
#                                 data = as.matrix( df[training.indexes, -claseIndex] ), 
#                                 label = as.numeric( df[training.indexes, claseIndex] ) - 1 )




# testing.predict = matrix( data = testing.predict, ncol = 3, nrow = nrow( df[-training.indexes,] ), byrow = T )
# testing.predict = matrix( data = testing.predict, ncol = 2, nrow = nrow( df[-training.indexes,] ), byrow = T )
# colnames( testing.predict ) = c("BAJA+1", "BAJA+2", "CONTINUA")
# colnames( testing.predict ) = c("NEG", "POS")
# ganancias[s] = ganancia.ternaria( testing.predict,  df[-training.indexes, claseIndex], 0.1 ) / 0.30
# plot( testing.predict ~ df[-training.indexes, claseIndex] )

# ganancias[s] = 
ganancias
# ganancias[s] = ganancia.ternaria( testing.predict,  testClass ) / 0.30

cat(tiempos[s], " | ", ganancias[s], " | ", umbrales[s], "\n")


# sink("xgboost.txt")
# str(df, list.len = ncol(df))
# sink()



#xgboost( data = dtrain, eta = 0.01, subsample = 1.0, colsample_bytree = 0.6, min_child_weight = 5, max_depth = 11, alpha = 0, lambda = 0.1, gamma = 0.01, nround= 650, num_class = 2, objective="multi:softprob", eval_metric= "merror", nthread = 8 )   SOLO CON LAS VARIABLES ORIGINALES
