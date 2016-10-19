library(DBI)

con = dbConnect(RSQLite::SQLite(), "db/producto_premium_201604.sqlite")

dbListTables(con)
dbListFields(con,"data")

db.TERNARIA = 1
db.BINARIA1 = 2
db.BINARIA2 = 3

db.connect = function() {
  con <<- dbConnect(RSQLite::SQLite(), "db/producto_premium_201604.sqlite")
}

db.getDataset = function(cual = db.TERNARIA, historicas = T) {
  # res = dbSendQuery(con, "SELECT * FROM data")
  
  sql = "SELECT * FROM data, historicas, visamaster WHERE data.numero_de_cliente = historicas.numero_de_cliente and data.numero_de_cliente = visamaster.numero_de_cliente"
  
  if ( historicas == F ) {
    sql = "SELECT * FROM data, visamaster WHERE data.numero_de_cliente = visamaster.numero_de_cliente"
  }
  
  res = dbSendQuery(con, sql)
  
  abril_dataset = dbFetch(res, n = -1 )
  
  abril_dataset = abril_dataset[,colnames(abril_dataset) != "numero_de_cliente" ]
  abril_dataset = abril_dataset[,colnames(abril_dataset) != "foto_mes" ]
  abril_dataset = abril_dataset[,colnames(abril_dataset) != "participa" ]
  
  if ( cual == db.TERNARIA) {
    abril_dataset$clasebinaria1 = NULL
    abril_dataset$clasebinaria2 = NULL
    abril_dataset$clase = as.factor(abril_dataset$clase)
    levels(abril_dataset$clase)[1] = "BAJAMASUNO"
    levels(abril_dataset$clase)[2] = "BAJAMASDOS"
  } else if ( cual == db.BINARIA1 ) {
    abril_dataset$clase = NULL
    abril_dataset$clasebinaria2 = NULL
    names(abril_dataset)[171] = "clase"
  } else if ( cual == db.BINARIA2 ) {
    abril_dataset$clase = NULL
    abril_dataset$clasebinaria1 = NULL
    names(abril_dataset)[171] = "clase"
  }
  
  
  abril_dataset
}