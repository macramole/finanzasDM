## Funciones de ganancia
#definicion  funcion ganancia_puntual para nuestro problema
ganancia_puntual = function( probs, clases, prob_puntual )
{
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    if( probs[ i, "POS"]  == prob_puntual   ){ suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
    } ;
  }
  
  return( suma )
}



#definicion  funcion ganancia_lista para una lista de probabilidades
ganancia_lista = function( probs, clases, prob_lista )
{
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    if( probs[ i, "POS"] %in% prob_lista   ){ suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
    } ;
  }
  
  return( suma )
}


#funcion que devuelve las probabilidades de clase_binaria2  para las cuales la ganancia es positiva
hojas_positivas = function( probs, clases )	
{
  
  vunicos =  unique( probs[ , "POS"], incomparables=FALSE ) ;
  
  hojas_lista <- c() ;
  
  largo = length( vunicos ) ;
  
  
  for( i in 1:largo )
  {
    vgan =  ganancia_puntual( probs, clases, vunicos[i] )  
    if( vgan>0 )  hojas_lista = append( hojas_lista,  vunicos[i] ) ;
  }
  
  
  return(   hojas_lista  )
}

#######################################



abril_dataset = db.getDataset(db.BINARIA1, F)
abril_dataset = db.nonulls(abril_dataset)
db.cantnulls(abril_dataset)

head(abril_dataset)

#me armo un dataset solo con la clase
# abril_dataset.clase = abril_dataset$clase
# names(abril_dataset.clase) = rownames(abril_dataset)
# head(abril_dataset.clase)

8*4*2*4*4*7/60/60

vcpValues = c(0, 0.001)
vminsplitValues = c(10,20,50,100)
vminbucketValues = c(1,2,3,4) #padre / cada número
vmaxdepthValues = c(4,5,6,7,8,9,10)

tiempoTotal0 =  Sys.time()  

for ( vminbucket in vminbucketValues ) {
  for ( vcp in vcpValues ) {
    for ( vminsplit in vminsplitValues ) {
      for ( vmaxdepth in vmaxdepthValues ) {

        # vminbucket = 1
        # vcp = 0
        # vminsplit = 18
        # vmaxdepth = 9
        # s = 1
        
        tiempos = c()
        ganancias = c()
        
        for ( s in 1:4 ) {
          #armo datasets
          set.seed( seeds[s] )
          abril_inTraining <- createDataPartition( abril_dataset$clasebinaria1, p = .70, list = FALSE)
          abril_dataset_train    <- abril_dataset[ abril_inTraining,]
          abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
          
          abril_inValidation <- createDataPartition( abril_dataset_train$clasebinaria1, p = .70, list = FALSE)
          abril_dataset_training    <- abril_dataset_train[  abril_inValidation, ]
          abril_dataset_validation  <- abril_dataset_train[ -abril_inValidation, ]
          
          #le saco la clase
          claseIndex = which( colnames(abril_dataset) == "clase" )
          abril_dataset_training = abril_dataset_training[, -claseIndex]
          
          #Asigno pesos <7750, 250>  es equivalente a  <31, 1>  
          vweights <- ifelse( abril_dataset_training$clasebinaria1 =='POS', 31, 1 )
          
          t0 =  Sys.time()  
          model <- rpart( clasebinaria1 ~ .,
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
        
        log.add("binaria1_VisaMaster_weights_corteProb", vcp, vminsplit, vminbucket, vmaxdepth, ganancias, tiempos )
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

cfValues = c(0.0001,0.001,0.01)
minCasesValues = c(100,200,400,500,600)


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
      abril_inTraining <- createDataPartition( abril_dataset$clasebinaria1, p = .70, list = FALSE)
      abril_dataset_train    <- abril_dataset[ abril_inTraining,]
      abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
      
      abril_inValidation <- createDataPartition( abril_dataset_train$clasebinaria1, p = .70, list = FALSE)
      abril_dataset_training    <- abril_dataset_train[  abril_inValidation, ]
      abril_dataset_validation  <- abril_dataset_train[ -abril_inValidation, ]
      
      #Asigno pesos <7750, 250>  es equivalente a  <31, 1>  , le pongo 15 porque algunos son neg
      vweights <- ifelse( abril_dataset_training$clasebinaria1 =='POS', 31, 1 )
      
      claseIndex = which( colnames(abril_dataset_training) == "clase" )
      abril_dataset_training = abril_dataset_training[ , -claseIndex]
      claseBinariaIndex = which( colnames(abril_dataset_training) == "clasebinaria1" )
      
      t0 =  Sys.time()  
      model <- C5.0(  x = abril_dataset_training[ , -claseIndex],
                      y = as.factor(abril_dataset_training[ , claseIndex]),
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
    
    log.add.c50("binaria_VisaMaster_weights_corteProb", cf, minCases, ganancias, tiempos )
  }
}

tiempoTotal1 =  Sys.time()  
cat("Tardó: ", as.numeric( tiempoTotal1 - tiempoTotal0 ), "\n")


