library(ranger)

#ranger num.trees= 500 min.node.size=20 mtry=20 probabilidad_corte=0.0568		

# abril_dataset = db.getDataset(historicas = F)

# abril_dataset = db.getBigDataset()
abril_dataset = db.getDatasetImportantes()
# abril_dataset = db.getDataset(historicas = F)
# dfImportantes = db.getDatasetImportantes()
# abril_dataset = cbind(abril_dataset, dfImportantes[, setdiff( colnames(dfImportantes), colnames(abril_dataset) )])

# abril_dataset = db.getBigDataset(db = db.DICIEMBRE, discret = F)
# abril_dataset = db.discretize.soft(abril_dataset)
# abril_dataset = db.discretize.tend(abril_dataset)
# abril_dataset = db.clean(abril_dataset)

abril_dataset = db.nonulls(abril_dataset)
db.cantnulls(abril_dataset)

# head(abril_dataset)

#checkpoint2 : 500  min.node.size=20 mtry=20  probabilidad_corte=0.0568

400*5*3*4/60/60

for ( canttrees in c(500, 800) ) {
  for ( vmin.node.size in c(100,200,300) ) {
    # for ( vmtry in c(10,30,50) ) {
    canttrees = 300
    vmin.node.size = 100
    # vmtry = 20
    s = 1
    
    ganancias = c()
    tiempos = c()
    umbrales = c()
    
    
    for( s in  1:5 ) {
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
        # mtry = vmtry,
        probability = T
        # save.memory = T
      )
      
      abril_training_prediccion = predict(  model,  abril_dataset_training, type = "response" )
      abril_dataset_training$stacking = abril_training_prediccion$predictions[, 2]
      
      model.stacking = ranger( 
        dependent.variable.name = "clase",
        data = abril_dataset_training, 
        num.trees = canttrees,
        importance = "impurity",
        # case.weights = vweights,
        num.threads = 4,
        min.node.size = vmin.node.size,
        # mtry = vmtry,
        probability = T
        # save.memory = T
      )
      
      # write.table(model$variable.importance, file = "ranger_variable_importance_2.txt")
      
      abril_validation_prediccion  = predict(  model,  abril_dataset_validation, type = "response" )
      abril_dataset_validation$stacking = abril_validation_prediccion$predictions[, 2]
      abril_validation_prediccion = predict(  model.stacking,  abril_dataset_validation, type = "response" )
      
      umbrales[s]  <-  umbral_ganancia_optimo( abril_validation_prediccion$predictions[, 2],  abril_dataset_validation$clase )
      
      abril_testing_prediccion  = predict(  model, abril_dataset_testing , type = "response")
      abril_dataset_testing$stacking = abril_testing_prediccion$predictions[, 2]
      abril_testing_prediccion  = predict(  model.stacking, abril_dataset_testing , type = "response")
      # head(abril_testing_prediccion$predictions, n = 100)
      
      ganancias[s] = ganancia( abril_testing_prediccion$predictions[, 2],  abril_dataset_testing$clase, umbrales[s] ) / 0.30 #1639167
      
      t1 =  Sys.time()
      tiempos[s] = as.numeric(  t1 - t0, units = "secs" )
      
      cat(canttrees, " | ", vmin.node.size, " | ", tiempos[s], " | ", ganancias[s], " | ", umbrales[s], "\n")
      
      rm(abril_inTraining, abril_dataset_train, abril_dataset_training, abril_dataset_testing, abril_inValidation, abril_dataset_validation, model, abril_testing_prediccion, vweights, abril_validation_prediccion )
      gc()
    }
    # }  
    
    log.add.ranger("abril_importantes_stacking", canttrees, vmin.node.size, ganancias, tiempos, umbrales)
    gc()
  }
}
