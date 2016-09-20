library(DBI)

con = dbConnect(RSQLite::SQLite(), "/home/macramole/Desktop/dm/2do_cuatri/finanzas/producto_premium_201604.sqlite")

dbListTables(con)
dbListFields(con,"data")

db.getDataset = function(cual) {
  res = dbSendQuery(con, "SELECT * FROM data")
  abril_dataset = dbFetch(res, n = -1 )
  
  if ( cual == "binaria1" ) {
    abril_dataset$clase = NULL
    abril_dataset$clasebinaria2 = NULL
    names(abril_dataset)[171] = "clase"
  } else if ( cual == "binaria2" ) {
    abril_dataset$clase = NULL
    abril_dataset$clasebinaria1 = NULL
    names(abril_dataset)[171] = "clase"
  }
  
  abril_dataset
}