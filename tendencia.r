library(DBI)
# library(RcppEigen)
library(doMC)
registerDoMC(4)

# con = dbConnect(RSQLite::SQLite(), "db/producto_premium_201511_201604.sqlite")
# con = dbConnect(RSQLite::SQLite(), "db/checkpoint/checkpoint.2.sqlite")
con = dbConnect(RSQLite::SQLite(), "db/data_all.sqlite")

# sql = "SELECT * FROM diciembre_historicas WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM diciembre_historicas WHERE foto_mes = 201512 ) ORDER BY numero_de_cliente ASC, foto_mes ASC"
sql = "SELECT * FROM abril_mdos_visamaster WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM abril_mdos_visamaster WHERE foto_mes = 201604 ) ORDER BY numero_de_cliente ASC, foto_mes ASC"
# sql = "SELECT numero_de_cliente, VisaMaster_finiciomora FROM data_visamaster_new WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM data_visamaster_new WHERE foto_mes = 201604 ) AND foto_mes >= 201511 AND foto_mes <> 201512 ORDER BY numero_de_cliente ASC, foto_mes ASC"
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

# todo esto es para una sola variable sin paralelo
# t = data.frame()
# # currentNumCliente = unique(df$numero_de_cliente)[3]
# cantClientes = length(unique(df$numero_de_cliente))
# currentClienteNum = 0
# 
# for ( currentNumCliente in unique(df$numero_de_cliente) ) {
#   y =  df[ df$numero_de_cliente == currentNumCliente, "VisaMaster_finiciomora" ]
#   
#   if ( all(is.na( y )) || all( is.null(y) )) {
#     t = rbind( t, c(currentNumCliente, NA) )
#   } else {
#     y[ is.na(y) ] = 0
#     
#     if ( sum(y) == y[1]*length(y) ) {
#       t = rbind( t, c(currentNumCliente, 0) )
#     } else {
#       x = 0:(length(y)-1)
#       y = scale(y, center = y[1])
#       
#       data = as.data.frame( cbind(x,y) )
#       model = lm( y ~ x, data, qr = F )
#       
#       t = rbind( t, c(currentNumCliente, model$coefficients[2]) )
#     }
#   }
#   
#   currentClienteNum = currentClienteNum + 1
#   if ( currentClienteNum %% 1000 == 0 ) {
#     cat(  currentClienteNum, "/", cantClientes, "\n", sep = " " )
#   }
#   
# }
##################################################

#esto es para todo paralelo
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
# write.table(t, file = "db/checkpoint/checkpoint.2.tend.tsv", sep = "\t", row.names = F)
write.table(t, file = "db/abril.tend2.tsv", sep = "\t", row.names = F)
# 
# t.df = as.data.frame(t)
# head(t)
# 
# malosCampos = c()
# for ( i in colnames(t.df) ) {
#   if ( all( is.na( t.df[, i] ) ) || length(unique( t.df[, i] )) == 1 ) {
#     malosCampos = c(malosCampos, i)
#   }
# }
# malosCamposIndex = which ( colnames(t.df) %in% malosCampos )
# 
# t.df = t.df[, -malosCamposIndex]
# head(t.df)
# 
# colnames(t.df) = c("numero_de_cliente",paste( colnames(t.df)[-1], "_tend", sep = "" ))

# 
# df[df$numero_de_cliente == 5520041,]

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
