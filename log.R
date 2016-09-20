log.file = "/home/macramole/Desktop/dm/2do_cuatri/finanzas/outputs.tsv"

log.writeHeader = function() {
  cat( "fecha", "dataset", "algoritmo", "cp" , "minsplit", "minbucket", "maxdepth", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file=log.file, fill=FALSE, append=FALSE )
}

log.add = function(dataset, algo = "rpart", vcp, vminsplit, vminbucket, vmaxdepth, ganancias, tiempos ) {
  cat( format(Sys.time(), "%Y%m%d %H%M%S"), dataset, algo, vcp , vminsplit, vminbucket, vmaxdepth, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file=log.file, fill=FALSE, append=TRUE ) 
}