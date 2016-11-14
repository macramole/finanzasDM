library(C50)
source("ternaria.funciones.R")
df = db.getBigDataset(db.TERNARIA)

cfValues = c(0.001)
minCasesValues = c(500, 600, 800, 1000)
trialsValues = 2:4

tiempoTotal0 =  Sys.time()  

for ( minCases in minCasesValues ) {
  for ( cf in cfValues ) {
    for ( trials in trialsValues ) {
      # cf = 0.001
      # minCases = 600
      # trials = 2
      # s = 1
      
      tiempos = c()
      ganancias = c()
      
      for ( s in 1:4 ) {
        #armo datasets
        set.seed( seeds[s] )
        abril_inTraining <- createDataPartition( df$clase, p = .70, list = FALSE)
        abril_dataset_train    <- df[ abril_inTraining,]
        abril_dataset_testing  <- df[-abril_inTraining,]
        
        abril_inValidation <- createDataPartition( abril_dataset_train$clase, p = .70, list = FALSE)
        abril_dataset_training    <- abril_dataset_train[  abril_inValidation, ]
        abril_dataset_validation  <- abril_dataset_train[ -abril_inValidation, ]
        
        #Asigno pesos <7750, 250>  es equivalente a  <31, 1>  , le pongo 15 porque algunos son neg
        vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )
        
        claseIndex = which( colnames(abril_dataset_training) == "clase" )
        
        t0 =  Sys.time()  
        model <- C5.0(  x = abril_dataset_training[ , -claseIndex],
                        y = abril_dataset_training[ , claseIndex],
                        weights = vweights,
                        rules = F,
                        trials = trials,
                        control = C5.0Control(CF = cf, minCases = minCases) 
        )
        t1 =  Sys.time()
        tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
        
        #determino las hojas con ganancia positiva en VALIDATION
        abril_validation_prediccion  = predict(  model, abril_dataset_validation , type = "prob")
        vhojas_positivas = hojas_positivas( abril_validation_prediccion,  abril_dataset_validation$clase )
        
        #calculo la ganancia en TESTING
        abril_testing_prediccion  = predict(  model, abril_dataset_testing , type = "prob")
        ganancias[s] <- ganancia_lista( abril_testing_prediccion,  abril_dataset_testing$clase,  vhojas_positivas  ) / 0.30
        
        cat(s, ":", cf, minCases, trials, "\n")
        cat("tardo: ", tiempos[s], "\n")
        cat("ganancia: ", ganancias[s], "\n")
      }
      
      log.add.c50("joined_new", cf, minCases, trials, ganancias, tiempos )
    }
  }
}

tiempoTotal1 =  Sys.time()  
cat("Tardó: ", as.numeric( tiempoTotal1 - tiempoTotal0 ), "\n")
