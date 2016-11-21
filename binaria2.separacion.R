library(ROCR)
library(ranger)
library(caret)
source("db.R")
source("ganancias.R")
source("ternaria.funciones.R")

file.out = "output_binaria2_separacion.tsv"

df = db.getDatasetImportantes( cual = db.BINARIA2 )
df = df[ df$clasebinaria2 == "POS", ]
df$clase = factor( ifelse( df$clase == "BAJA+2", "POS", "NEG" ) )
df$clasebinaria2 = NULL

df = db.nonulls(df)
db.cantnulls(df)

for ( canttrees in c(500, 800) ) {
  for ( vmin.node.size in c(100,200,300) ) {
    # for ( vmtry in c(10,30,50) ) {
    canttrees = 300
    vmin.node.size = 100
    # vmtry = 20
    s = 1
    
    aucTraining = c()
    aucTesting = c()
    
    for( s in  1:5 ) {
      set.seed( seeds[s] )
      training.indexes <- createDataPartition( df$clase, p = .70, list = FALSE)
      df.training <- df[ training.indexes,]
      df.testing  <- df[-training.indexes,]
      
      t0 =  Sys.time()  
      model = ranger( 
        dependent.variable.name = "clase",
        data = df.training, 
        num.trees = canttrees,
        importance = "impurity",
        # case.weights = vweights,
        num.threads = 4,
        min.node.size = vmin.node.size,
        # mtry = vmtry,
        probability = T
        # save.memory = T
      )
      t1 =  Sys.time()
      tiempos[s] = as.numeric(  t1 - t0, units = "secs" )
      
      df.training.prediccion  = predict(  model, df.training , type = "response")
      df.training.prediccionROC = prediction(df.training.prediccion$predictions[, 2], df.training$clase)
      # df.training.performance = performance(df.training.prediccionROC, measure = "tpr", x.measure = "fpr") 
      #plot(df.training.performance)
      df.training.auc = performance(df.training.prediccionROC, measure = "auc")
      df.training.auc = df.training.auc@y.values[[1]]
      aucTraining[s] = df.training.auc
      
      df.testing.prediccion  = predict(  model, df.testing , type = "response")
      df.testing.prediccionROC = prediction(df.testing.prediccion$predictions[, 2], df.testing$clase)
      # df.testing.performance = performance(df.testing.prediccionROC, measure = "tpr", x.measure = "fpr") 
      #plot(df.testing.performance)
      df.testing.auc = performance(df.testing.prediccionROC, measure = "auc")
      df.testing.auc = df.testing.auc@y.values[[1]]
      aucTesting[s] = df.training.auc
      
      cat(canttrees, " | ", vmin.node.size, " | ", aucTraining[s], " | ", aucTesting[s], " | ", umbrales[s], "\n")
    }
    # }  
    
    log.add.ranger("abril_importantes", canttrees, vmin.node.size, ganancias, tiempos, umbrales)
    gc()
  }
}
