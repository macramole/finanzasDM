library(ranger)

#ranger num.trees= 500 min.node.size=20 mtry=20 probabilidad_corte=0.0568		

# abril_dataset = db.getDataset(historicas = F)

# abril_dataset = db.getBigDataset()
abril_dataset = db.getDatasetImportantes()
# abril_dataset = db.getBigDataset(db = db.DICIEMBRE, discret = F)
# abril_dataset = db.discretize.soft(abril_dataset)
# abril_dataset = db.discretize.tend(abril_dataset)
# abril_dataset = db.clean(abril_dataset)

abril_dataset = db.nonulls(abril_dataset)
db.cantnulls(abril_dataset)

# head(abril_dataset)


for ( canttrees in c(300, 200, 400) ) {
  for ( vmin.node.size in c(100, 150, 200, 250) ) {
    # for ( vmtry in c(10,30,50) ) {
      # canttrees = 300
      # vmin.node.size = 2200
      # vmtry = 20
      # s = 2
      
      ganancias = c()
      tiempos = c()
      umbrales = c()
      
      
      for( s in  1:4 ) {
        set.seed( seeds[s] )
        abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
        abril_dataset_train    <- abril_dataset[ abril_inTraining,]
        abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
        
        abril_inValidation <- createDataPartition( abril_dataset_train$clase, p = .70, list = FALSE)
        abril_dataset_training    <- abril_dataset_train[  abril_inValidation, ]
        abril_dataset_validation  <- abril_dataset_train[ -abril_inValidation, ]
        
        # vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )
        
        t0 =  Sys.time()  
        model = ranger( 
          dependent.variable.name = "clase",
          data = abril_dataset_training, 
          num.trees = canttrees,
          importance = "impurity",
          # case.weights = vweights,
          num.threads = 4,
          min.node.size = vmin.node.size,
          probability = T
          # mtry = vmtry
          # save.memory = T
        )
        t1 =  Sys.time()
        tiempos[s] = as.numeric(  t1 - t0, units = "secs" )
        
        
        # write.table(model$variable.importance, file = "ranger_variable_importance_2.txt")
        
        abril_validation_prediccion  = predict(  model,  abril_dataset_validation )
        umbrales[s]  <-  umbral_ganancia_optimo( abril_validation_prediccion$predictions,  abril_dataset_validation$clase )
        
        abril_testing_prediccion  = predict(  model, abril_dataset_testing , type = "response")
        # head(abril_testing_prediccion$predictions, n = 100)
        
        ganancias[s] = ganancia.ternaria( abril_testing_prediccion$predictions,  abril_dataset_testing$clase, umbrales[s] ) / 0.30
        
        cat(canttrees, " | ", vmin.node.size, " | ", tiempos[s], " | ", ganancias[s], " | ", umbrales[s], "\n")
        
        rm(abril_inTraining, abril_dataset_train, abril_dataset_training, abril_dataset_testing, abril_inValidation, abril_dataset_validation, model, abril_testing_prediccion, vweights, abril_validation_prediccion )
        gc()
      }
    # }  

	  log.add.ranger("abril_importantes", canttrees, vmin.node.size, ganancias, tiempos, umbrales)
	  gc()
	}
}
