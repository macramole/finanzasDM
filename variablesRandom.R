df = db.getDatasetImportantes()
claseIndex = which( colnames(df) == "clase" )
df = df[, -claseIndex]
variablesNuevas = list()

# creo nuevas variables de la forma (v1 + v2) / v3
for ( i in 1:300 ) {
  tresVariables = floor( runif(3, 1, ncol(df)) )
  nuevaColumna = apply(df[1:100, ], 1, function(x) {
    cat(x)
    ( x[ tresVariables[1] ] + x[ tresVariables[2] ] ) / x[ tresVariables[3] ] 
  })
}