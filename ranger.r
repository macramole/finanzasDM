library(ranger)

#abril_dataset = db.getDataset(historicas = F)
abril_dataset = db.getBigDataset()
abril_dataset = db.nonulls(abril_dataset)
head(abril_dataset)

for ( canttrees in c(200,300,500) ) {
  for ( vmin.node.size in c(500, 1000, 1500, 2000, 2500, 3000) ) {
	  # canttrees = 300
	  # vmin.node.size = 1000
    # s = 1
    
	  ganancias = c()
	  tiempos = c()
	  
	  
	  vimportance = "impurity"
	  for( s in  1:5 ) {
  		set.seed( seeds[s] )
  		abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
  		abril_dataset_training <- abril_dataset[ abril_inTraining,]
  		abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
  		
  		vweights <- ifelse( abril_dataset_training$clase =='BAJA+2', 31, 1 )
  		#mirar save.memory = TRUE
  
  		t0 =  Sys.time()  
  		model = ranger( 
  			dependent.variable.name = "clase",
  			data = abril_dataset_training, 
  			num.trees = canttrees,
  			importance = vimportance,
  			case.weights = vweights,
  			num.threads = 1,
  			min.node.size = vmin.node.size,
  			probability = T
  			# save.memory = T
  		)
  		t1 =  Sys.time()
  		tiempos[s] = as.numeric(  t1 - t0, units = "secs" )
    		
  		
  		abril_testing_prediccion  = predict(  model, abril_dataset_testing , type = "response")
  		# head(abril_testing_prediccion$predictions, n = 100)
  		
  		ganancias[s] = ganancia.ternaria( abril_testing_prediccion$predictions,  abril_dataset_testing$clase, 0.5 ) / 0.30
  		
  		cat(tiempos[s], " | ", ganancias[s], "\n")
	  }
	  
	  log.add.ranger("abril_visamaster_historicas_tendencia", canttrees, vmin.node.size, ganancias, tiempos)
	}
}


