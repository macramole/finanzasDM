library(CHAID)
library(arules)
library(entropy)

abril_dataset = db.getDataset(cual = db.TERNARIA, historicas = F)
db.nonulls()

head(abril_dataset)
sum(is.na(abril_dataset$Master_mpagominimo))
cuts = discretize(abril_dataset$Master_mpagominimo, onlycuts = T)
discTest = discretize(abril_dataset$Master_mpagominimo, categories = 3)
table(discTest)
sum(is.na(discTest))

disc = discretize(abril_dataset$Master_mpagominimo, 3)

abril_tiny = abril_dataset[1:10000,80:82]
abril_dataset_disc = apply( abril_tiny, 2, discretize )
str(abril_tiny)
head(abril_dataset_disc)
  
# for ( canttrees in c(1000,3000) ) {
  ganancias = c()
  tiempos = c()
  s = 1
  
  valpha2 = 0.05
  valpha3 = -1
  valpha4 = 0.05
  vminsplit = 20 #default es 20
  vminbucket = 50
  vminprob = 0.01
  vmaxheight = 8 #default es -1
  
  for( s in  1:3 ) {
    set.seed( seeds[s] )
    abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
    abril_dataset_training <- abril_dataset[ abril_inTraining,]
    abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
    
    
    t0 =  Sys.time()
    model = chaid(
      clase ~ .,
      abril_dataset_training,
      control = chaid_control(
        alpha2 = valpha2,
        alpha3 = valpha3,
        alpha4 = valpha4,
        minsplit = vminsplit, #default es 20
        minbucket = vminbucket,
        minprob = vminprob,
        maxheight = vmaxheight #default es -1
      )
    )
    t1 =  Sys.time()
    tiempos[s] = as.numeric(  t1 - t0, units = "secs" )
    
    abril_testing_prediccion  = predict(  model, abril_dataset_testing)
    # head(abril_testing_prediccion$predictions, n = 100)
    
    ganancias[s] = ganancia.ternaria( abril_testing_prediccion$predictions,  abril_dataset_testing$clase ) / 0.30
  }
  
  log.add.chaid("abril_historicas_visamaster", valpha2, valpha3, valpha4, vminsplit, vminbucket, vminprob, vmaxheight, ganancias, tiempos)
#}

