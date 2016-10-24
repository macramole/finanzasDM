log.file = "./output.tsv"
log.file.ranger = "./output_ranger.tsv"
log.file.chaid = "./output_chaid.tsv"
log.file.c50 = "./output_c50.tsv"

log.writeHeader = function() {
  cat( "fecha", "dataset", "algoritmo", "cp" , "minsplit", "minbucket", "maxdepth", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file=log.file, fill=FALSE, append=FALSE )
}

log.writeHeader.ranger = function() {
  cat( "fecha", "dataset", "cantTrees", "importance", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file=log.file.ranger, fill=FALSE, append=FALSE )
}

log.writeHeader.chaid = function() {
  cat( "fecha", "dataset", "alpha2", "alpha3", "alpha4", "minsplit", "minbucket", "minprob", "maxheight", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file=log.file.chaid, fill=FALSE, append=FALSE )
}

log.writeHeader.c50 = function() {
  cat( "fecha", "dataset","cf", "minCases", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file=log.file.c50, fill=FALSE, append=FALSE )
}

log.add = function(dataset, vcp, vminsplit, vminbucket, vmaxdepth, ganancias, tiempos, algo = "rpart" ) {
  cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, algo, vcp , vminsplit, vminsplit/vminbucket, vmaxdepth, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file=log.file, fill=FALSE, append=TRUE ) 
  # cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, algo, vcp , vminsplit, vminsplit/vminbucket, vmaxdepth, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t" ) 
}

log.add.ranger = function(dataset, cantTrees, importance, ganancias, tiempos) {
  cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, cantTrees, importance, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file=log.file.ranger, fill=FALSE, append=TRUE ) 
  # cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, cantTrees, importance, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", fill=FALSE, append=TRUE ) 
}

log.add.chaid = function(dataset, alpha2, alpha3, alpha4, minsplit, minbucket, minprob, maxheight, ganancias, tiempos) {
  cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, alpha2, alpha3, alpha4, minsplit, minbucket, minprob, maxheight, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file=log.file.chaid, fill=FALSE, append=TRUE ) 
  # cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, alpha2, alpha3, alpha4, minsplit, minbucket, minprob, maxheight, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", fill=FALSE, append=TRUE ) 
}

log.add.c50 = function(dataset, cf, minCases, ganancias, tiempos) {
  cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, cf, minCases, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file=log.file.c50, fill=FALSE, append=TRUE ) 
  # cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, alpha2, alpha3, alpha4, minsplit, minbucket, minprob, maxheight, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", fill=FALSE, append=TRUE ) 
}