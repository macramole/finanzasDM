library(DBI)
# library(RcppEigen)
library(doMC)
registerDoMC(3)

con = dbConnect(RSQLite::SQLite(), "db/producto_premium_201511_201604.sqlite")

sql = "SELECT * FROM data_visamaster WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM data_visamaster WHERE foto_mes = 201604 ) AND foto_mes <> 201512 ORDER BY numero_de_cliente ASC, foto_mes ASC"
res = dbSendQuery(con, sql)
df = dbFetch(res, n = -1 )
df = df[,colnames(df) != "participa" ]

ignoreCols = c("numero_de_cliente","foto_mes", "participa", "cliente_edad", "cliente_antiguedad", "clase")
columnas = colnames( df )
#saco columnas innecesarias
for ( col in ignoreCols ) {
  columnas = columnas[ columnas != col ]
}

# cat("numero_de_cliente", columnas, "\n", file = "tendencias.tsv", fill = F, sep = "\t", append = F)

# for ( currentNumCliente in unique(df$numero_de_cliente) ) {

t0 =  Sys.time()  
t <- foreach( currentNumCliente = unique(df$numero_de_cliente), .combine = "rbind" ) %dopar% {
  currentCliente =  df[ df$numero_de_cliente == currentNumCliente, ]
  
  # head(currentCliente)
  
  #saco columnas innecesarias
  for ( col in ignoreCols ) {
    currentCliente = currentCliente[,colnames(currentCliente) != col ]
  }
  
  tendency = apply( currentCliente, 2, function(y) {
    if ( all(is.na( y )) || all( is.null(y) )) {
      return(NA)
    }
    
    y[ is.na(y) ] = 0
    
    if ( sum(y) == y[1]*length(y) ) {
      return(0)
    }
    
    x = 0:(length(y)-1)
    y = scale(y, center = y[1])
    
    data = as.data.frame( cbind(x,y) )
    model = lm( y ~ x, data, qr = F )
    # model = fastLm(y ~ x, data)
    # model = fastLm(X = x, y = y)

    model$coefficients[2]
    # sum(y)
  })
  c(currentNumCliente, as.vector(tendency))
  #cat(c(currentNumCliente, as.vector(tendency)), "\n", file = "tendencias.tsv", fill = F, sep = "\t", append = T)
}
t1 =  Sys.time()  

cat("Tardó ", as.numeric(t1-t0, units="hours"), "horas", "\n") #5.6 horas 180 variables
colnames(t) = c("numero_de_cliente",columnas)
write.table(t, file = "tendencias.tsv", sep = "\t", row.names = F)

# write.csv(tendencies, file = "tendencies.csv", sep = "\t", row.names = F)

# numCliente = unique(df$numero_de_cliente)[4]
# currentCliente =  df[ df$numero_de_cliente == numCliente, ]
# currentCliente.tendency = tendencies[ tendencies$numero_de_cliente == numCliente, ]
# currentCliente$VisaMaster_mconsumototal
# currentCliente.tendency$VisaMaster_mconsumototal
# qplot( as.factor(currentCliente$foto_mes), currentCliente$VisaMaster_mconsumototal )
# 
# #currentCliente.tendency
# currentCliente$Master_marca_atraso
# 
# if ( all(is.na(currentCliente$Master_marca_atraso)) ) {
#   NA
# }
# 
# y = currentCliente$Master_marca_atraso
# x = 0:(length(y)-1)
# 
# qplot(x,y)
# 
# data = as.data.frame( cbind(x,y) )
# 
# model = lm( y ~ x, data )
# model$coefficients[2]
# 
# 
# 
# library(ggplot2)
# qplot(x,y)
# line()
