library(ranger)

db.nonulls()
head(abril_dataset)

sum( c(1000,3000) )*4/60/60



for ( vmin.node.size in c(10, 100, 200) ) {
	for ( canttrees in c(500, 1000,3000) ) {
	  # canttrees = 1000

	  ganancias = c()
	  tiempos = c()
	  
	  s = 1
	  vimportance = "impurity"
	  for( s in  1:3 ) {
		set.seed( seeds[s] )
		abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
		abril_dataset_training <- abril_dataset[ abril_inTraining,]
		abril_dataset_testing  <- abril_dataset[-abril_inTraining,]
		
		#mirar save.memory = TRUE

		t0 =  Sys.time()  
		model = ranger( 
			dependent.variable.name = clase,
			data = abril_dataset_training, 
			num.trees = canttrees, 
			case.weights = "TODO",
			num.threads = 3,
			min.node.size = vmin.node.size,
			probability = T
		)
		t1 =  Sys.time()
		tiempos[s] = as.numeric(  t1 - t0, units = "secs" )
		
		
		
		abril_testing_prediccion  = predict(  model, abril_dataset_testing , type = "response")
		# head(abril_testing_prediccion$predictions, n = 100)
		
		ganancias[s] = ganancia.ternaria( abril_testing_prediccion$predictions,  abril_dataset_testing$clase ) / 0.30
	  }
	  
	  log.add.ranger("abril_historicas_visamaster", canttrees, vmin.node.size, ganancias, tiempos)
	}
}


