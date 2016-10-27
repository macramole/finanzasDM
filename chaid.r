library(CHAID)
library(arules)
library(entropy)

abril_dataset = db.getDataset(cual = db.TERNARIA, historicas = F)
claseIndex = which( colnames(abril_dataset) == "clase" )

abril_dataset_discret = db.discretize(abril_dataset[,-claseIndex])
abril_dataset_discret = cbind(abril_dataset_discret, abril_dataset$clase)
colnames(abril_dataset_discret)[ncol(abril_dataset_discret)] = "clase"

# head(abril_dataset_discret)
# sink("asd.txt")
# str(abril_dataset_discret)
# sink()

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
  vmaxheight = -1 #default es -1
  
  for( s in  1:3 ) {
    set.seed( seeds[s] )
    abril_inTraining <- createDataPartition( abril_dataset_discret$clase, p = .70, list = FALSE)
    abril_dataset_training <- abril_dataset_discret[ abril_inTraining,]
    abril_dataset_testing  <- abril_dataset_discret[-abril_inTraining,]
    
    vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )
    
    t0 =  Sys.time()
    model = chaid(
      clase ~ .,
      abril_dataset_training,
      weights = vweights,
      na.action = na.pass,
      control = chaid_control(
        alpha2 = valpha2,
        alpha3 = valpha3,
        alpha4 = valpha4,
        minsplit = vminsplit,
        minbucket = vminbucket,
        minprob = vminprob,
        maxheight = vmaxheight
      )
    )
    t1 =  Sys.time()
    tiempos[s] = as.numeric(  t1 - t0, units = "secs" )
    
    abril_testing_prediccion  = predict(  model, abril_dataset_testing ,  type = "prob")
    head(abril_testing_prediccion, n = 100)
    
    ganancias[s] = ganancia.ternaria( abril_testing_prediccion,  abril_dataset_testing$clase, 0.5 ) / 0.30
  }
  
  log.add.chaid("abril_historicas_visamaster", valpha2, valpha3, valpha4, vminsplit, vminbucket, vminprob, vmaxheight, ganancias, tiempos)
#}

