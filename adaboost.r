library(adabag)

vcp = 0.005
vminsplit = 400
vminbucket = 1
vmaxdepth = 6

df = db.getBigDataset()

s = 1

tiempos = c()
ganancias = c()
umbrales = c()

for ( s in 1:4 ) {
  #armo datasets
  set.seed( seeds[s] )
  df.training <- createDataPartition( df$clase, p = .70, list = FALSE)
  df.validation <- createDataPartition( df[df.training, "clase"], p = .70, list = FALSE)
  
  #Asigno pesos <7750, 250>  es equivalente a  <31, 1>  , le pongo 15 porque algunos son neg
  # vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )
  
  t0 =  Sys.time() 
  model = boosting(clase ~ ., 
                   data = df[ -df.validation, ],  
                   boos = F,
                   mfinal = 3,
                   coeflearn = "Breiman",
                   control = rpart.control(
                     # weights = vweights,
                     cp=vcp, 
                     minsplit=vminsplit, 
                     xval=0, 
                     maxsurrogate=1, 
                     surrogatestyle=1, 
                     minbucket=vminsplit/vminbucket, 
                     maxdepth=vmaxdepth
                   )
  )
  t1 =  Sys.time()
  tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
  
  #determino las hojas con ganancia positiva en VALIDATION
  validation.pred  = predict(  model, df[ df.validation, ] , type = "prob")
  validation.prob = validation.pred$prob
  colnames(validation.prob) = c("BAJA+1", "BAJA+2", "CONTINUA")
  # vhojas_positivas = hojas_positivas( abril_validation_prediccion,  abril_dataset_validation$clase )
  umbrales[s]  <-  umbral_ganancia_optimo( validation.prob,  df[ df.validation, "clase"] )
  
  table(validation.prob[,2])
  
  #calculo la ganancia en TESTING
  testing.pred = predict(  model, df[ -df.training, ], type = "prob")
  testing.prob = validation.pred$prob
  colnames(testing.prob) = c("BAJA+1", "BAJA+2", "CONTINUA")
  
  ganancias[s] <- ganancia.ternaria( testing.prob,  df[ -df.training, "clase"], 0 ) / 0.30
  
  cat(tiempos[s], " | ", ganancias[s], "\n")
  
  rm(abril_inTraining, abril_dataset_training, abril_dataset_testing, abril_dataset_train, abril_inTraining, abril_inValidation, abril_dataset_validation, model, abril_testing_prediccion, vweights, abril_validation_prediccion )
  gc()
}

log.add("ternaria_VisaMaster_NEW_DISCRET_ORDERED_weights_corteProb", vcp, vminsplit, vminbucket, vmaxdepth, ganancias, tiempos)
