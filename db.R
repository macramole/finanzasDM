library(DBI)

con = dbConnect(RSQLite::SQLite(), "db/producto_premium_201604.sqlite")

# dbListTables(con)
# dbListFields(con,"data")

db.TERNARIA = 1
db.BINARIA1 = 2
db.BINARIA2 = 3

db.connect = function() {
  con <<- dbConnect(RSQLite::SQLite(), "db/producto_premium_201604.sqlite")
}

# db.save = function( filename = "dataset_save.csv" ) {
#   write.csv()
# }

db.cantnulls = function(df) {
  sum(is.na(df)) #cant na
}

db.nonulls = function(df) {
  db.NOT_NULL <- -99999999
  df[ is.na(df) ] <- db.NOT_NULL
  df
}

db.getDataset = function(cual = db.TERNARIA, historicas = T) {
  # res = dbSendQuery(con, "SELECT * FROM data")
  
  #sql = "SELECT * FROM data d INNER JOIN historicas h ON d.numero_de_cliente = h.numero_de_cliente INNER JOIN visamaster vm ON d.numero_de_cliente = h.numero_de_cliente"
    # SELECT 
    #   * 
    # FROM 
    #   data d
    # INNER JOIN historicas h ON
    #   data.numero_de_cliente = h.numero_de_cliente
    # INNER JOIN visamaster vm ON
    #   data.numero_de_cliente = h.numero_de_cliente
  
  sql = "SELECT * FROM data, visamaster WHERE data.numero_de_cliente = visamaster.numero_de_cliente ORDER BY numero_de_cliente"
  
  
  res = dbSendQuery(con, sql)
  
  abril_dataset = dbFetch(res, n = -1 )
  
  if ( historicas == T ) {
    dfHistoricas = read.table("db/tendencias.tsv", sep = "\t", header = T)
    abril_dataset = merge(x = abril_dataset, y = dfHistoricas, by = 1, all.x = TRUE)
  }
  
  rownames(abril_dataset) = abril_dataset$numero_de_cliente
  abril_dataset = abril_dataset[,colnames(abril_dataset) != "numero_de_cliente" ]
  abril_dataset = abril_dataset[,colnames(abril_dataset) != "foto_mes" ]
  abril_dataset = abril_dataset[,colnames(abril_dataset) != "participa" ]
  
  abril_dataset$clase = as.factor(abril_dataset$clase)
  
  if ( cual == db.TERNARIA) {
    abril_dataset$clasebinaria1 = NULL
    abril_dataset$clasebinaria2 = NULL
    # levels(abril_dataset$clase)[1] = "BAJAMASUNO"
    # levels(abril_dataset$clase)[2] = "BAJAMASDOS"
  } else if ( cual == db.BINARIA1 ) {
    #abril_dataset$clase = NULL
    abril_dataset$clasebinaria2 = NULL
    #names(abril_dataset)[171] = "clase"
  } else if ( cual == db.BINARIA2 ) {
    # abril_dataset$clase = NULL
    abril_dataset$clasebinaria1 = NULL
    # names(abril_dataset)[171] = "clase"
  }
  
  abril_dataset
}