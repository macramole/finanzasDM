source("ternaria.funciones.R")

abril_dataset = db.getDataset(db.TERNARIA, F)
abril_dataset = db.nonulls(abril_dataset)
db.cantnulls(abril_dataset)

# head(abril_dataset)

#me armo un dataset solo con la clase
# abril_dataset.clase = abril_dataset$clase
# names(abril_dataset.clase) = rownames(abril_dataset)
# head(abril_dataset.clase)

8*4*2*4*4*7/60/60

vcpValues = c(0.001, 0.005, 0.0001)
vminsplitValues = c(100,200,400,800)
vminbucketValues = c(1,2,3,4) #padre / cada número
vmaxdepthValues = c(4,5,6)

tiempoTotal0 =  Sys.time()  

for ( vminbucket in vminbucketValues ) {
  for ( vcp in vcpValues ) {
    for ( vminsplit in vminsplitValues ) {
      for ( vmaxdepth in vmaxdepthValues ) {

        # vcp = 0.001
        # vminsplit = 100
        # vminbucket = 1
        # vmaxdepth = 5
        
        # s = 1
        
        tiempos = c()
        ganancias = c()
        
        for ( s in 1:4 ) {
          #armo datasets
          set.seed( seeds[s] )
          abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
          abril_dataset_train    <- abril_dataset[ abril_inTraining,]
          abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
          
          abril_inValidation <- createDataPartition( abril_dataset_train$clase, p = .70, list = FALSE)
          abril_dataset_training    <- abril_dataset_train[  abril_inValidation, ]
          abril_dataset_validation  <- abril_dataset_train[ -abril_inValidation, ]
          
          #Asigno pesos <7750, 250>  es equivalente a  <31, 1>  , le pongo 15 porque algunos son neg
          vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )
          
          t0 =  Sys.time()  
          model <- rpart( clase ~ .,
                          data = abril_dataset_training,  
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
          t1 =  Sys.time()
          tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
          
          #determino las hojas con ganancia positiva en VALIDATION
          abril_validation_prediccion  = predict(  model, abril_dataset_validation , type = "prob")
          vhojas_positivas = hojas_positivas( abril_validation_prediccion,  abril_dataset_validation$clase )
          
          #calculo la ganancia en TESTING
          abril_testing_prediccion  = predict(  model, abril_dataset_testing , type = "prob")
          ganancias[s] <- ganancia_lista( abril_testing_prediccion,  abril_dataset_testing$clase,  vhojas_positivas  ) / 0.30
        }
        
        log.add("ternaria_VisaMaster_weights_corteProb", vcp, vminsplit, vminbucket, vmaxdepth, ganancias, tiempos)
      }
    }
  }
}

tiempoTotal1 =  Sys.time()  
cat("Tardó: ", as.numeric( tiempoTotal1 - tiempoTotal0 ), fill = T)




#########
# C 5.0
########

library(C50)
cfValues = c(0.0001,0.001,0.01,0.1,0.2)
minCasesValues = c(10,50,100,200,400)

tiempoTotal0 =  Sys.time()  

for ( minCases in minCasesValues ) {
  for ( cf in cfValues ) {
    # cf = 0.001
    # minCases = 400
    # s = 1
    
    tiempos = c()
    ganancias = c()
    
    for ( s in 1:4 ) {
      #armo datasets
      set.seed( seeds[s] )
      abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
      abril_dataset_train    <- abril_dataset[ abril_inTraining,]
      abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
      
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
                      trials = 1,
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
      
      cat(cf, minCases, "\n")
      cat("tardo: ", tiempos[s], "\n")
      cat("ganancia: ", ganancias[s], "\n")
    }
    
    log.add.c50("ternaria_VisaMaster_weights_corteProb", cf, minCases, ganancias, tiempos )
  }
}

tiempoTotal1 =  Sys.time()  
cat("Tardó: ", as.numeric( tiempoTotal1 - tiempoTotal0 ), "\n")

#########
# ENSAMBLE
#########

abril_dataset = db.getDataset(db.TERNARIA, F)

##C5
s = 1

#armo datasets
set.seed( seeds[s] )
abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
abril_dataset_train    <- abril_dataset[ abril_inTraining,]
abril_dataset_testing  <- abril_dataset[-abril_inTraining,]

abril_inValidation <- createDataPartition( abril_dataset_train$clase, p = .70, list = FALSE)
abril_dataset_training    <- abril_dataset_train[  abril_inValidation, ]
abril_dataset_validation  <- abril_dataset_train[ -abril_inValidation, ]

#Asigno pesos <7750, 250>  es equivalente a  <31, 1>  , le pongo 15 porque algunos son neg
vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )

claseIndex = which( colnames(abril_dataset_training) == "clase" )

library(C50)
cf = 0.001
minCases = 400
model.1 <- C5.0(  x = abril_dataset_training[ , -claseIndex],
                y = abril_dataset_training[ , claseIndex],
                weights = vweights,
                rules = F,
                trials = 1,
                control = C5.0Control(CF = cf, minCases = minCases) 
)

summary(model.1)

library(rtree)
vcp = 0.005	
vminsplit = 400	
vminbucket = 1
vmaxdepth = 6
abril_dataset_training_noNulls = db.nonulls(abril_dataset_training)
db.cantnulls(abril_dataset_training_noNulls)
model.2 <- rpart( clase ~ .,
                data = abril_dataset_training_noNulls,  
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

summary(model.2)



#determino las hojas con ganancia positiva en VALIDATION
model.1.prediction.validation  = predict(  model.1, abril_dataset_validation , type = "prob")
abril_dataset_validation_noNulls =  db.nonulls(abril_dataset_validation)
model.2.prediction.validation  = predict(  model.2, abril_dataset_validation_noNulls , type = "prob")

pesosEnsamble = c(0,1)
sum(pesosEnsamble)

ensamble.prediction.validation = ( model.1.prediction.validation * pesosEnsamble[1] + model.2.prediction.validation * pesosEnsamble[2] )

# model.1.prediction.validation[5,]
# model.2.prediction.validation[5,]
# ensamble.prediction.validation[5,]

model.1.hojas_positivas = hojas_positivas( model.1.prediction.validation,  abril_dataset_validation$clase )
model.2.hojas_positivas = hojas_positivas( model.2.prediction.validation,  abril_dataset_validation$clase )
ensamble.hojas_positivas = hojas_positivas( ensamble.prediction.validation,  abril_dataset_validation$clase )

#calculo la ganancia en TESTING
model.1.prediction.testing = predict(  model.1, abril_dataset_testing , type = "prob")
abril_dataset_testing_noNulls = db.nonulls(abril_dataset_testing)
model.2.prediction.testing = predict(  model.2, abril_dataset_testing_noNulls , type = "prob")

ensamble.prediction.testing = ( model.1.prediction.testing * pesosEnsamble[1] + model.2.prediction.testing * pesosEnsamble[2] )

# model.1.prediction.testing[15,]
# model.2.prediction.testing[15,]
# ensamble.prediction.testing[15,]

model.1.ganancia = ganancia_lista( model.1.prediction.testing,  abril_dataset_testing$clase,  model.1.hojas_positivas  ) / 0.30
model.2.ganancia = ganancia_lista( model.2.prediction.testing,  abril_dataset_testing$clase,  model.2.hojas_positivas  ) / 0.30
ensamble.ganancia <- ganancia_lista( ensamble.prediction.testing,  abril_dataset_testing$clase,  ensamble.hojas_positivas  ) / 0.30


