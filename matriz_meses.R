library(DBI)
library(rpart)
library(caret)


con = dbConnect(RSQLite::SQLite(), "db/producto_premium_201511_201604.sqlite")
sql = "SELECT * FROM data_visamaster_new"
res = dbSendQuery(con, sql)
df = dbFetch(res, n = -1 )

# df = db.discretize.soft(df)
# df = db.clean(df)

meses = c(201511,201512,201601,201602,201603,201604)
df.m = list()
for (i in 1:6) {
  df.m[[i]] = df[ df$foto_mes == meses[i], ]
}

rm(df)
gc()

matriz_meses = matrix(0, nrow = 6, ncol = 6)
colnames(matriz_meses) = c("Noviembre", "Diciembre", "Enero", "Febrero", "Marzo", "Abril")
rownames(matriz_meses) = colnames(matriz_meses)

vcp = 0.005
vminsplit = 400
vminbucket = 1
vmaxdepth = 6

# i = 1
# j = 2

for ( i in 1:6 ) {
  for ( j in 1:6 ) {
    if ( i != j) {
      vweights <- ifelse( df.m[[i]]$clase =='BAJA+2', 31, 1 )
      model <- rpart( clase ~ .,
                      data = df.m[[i]],  
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
      
      pred  = predict(  model,  df.m[[j]] , type = "prob")
      matriz_meses[i,j] = ganancia.ternaria( pred,  df.m[[j]]$clase, 0.5 )
    } else {
      partitionIndex <- createDataPartition( df.m[[i]]$clase, p = .70, list = FALSE)
      
      vweights <- ifelse( df.m[[i]][partitionIndex, "clase"] =='BAJA+2', 31, 1 )
      model <- rpart( clase ~ .,
                      data = df.m[[i]][partitionIndex,],  
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
      
      pred  = predict(  model,  df.m[[i]][-partitionIndex,] , type = "prob")
      matriz_meses[i,j] = ganancia.ternaria( pred,  df.m[[i]][-partitionIndex, "clase"], 0.5 ) / 0.3
    }
  }
}

write.table(matriz_meses, file = "matriz_meses.tsv", sep = "\t", append = F)
