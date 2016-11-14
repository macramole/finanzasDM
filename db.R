library(DBI)

con = dbConnect(RSQLite::SQLite(), "db/producto_premium_201604.sqlite")

# dbListTables(con)
# dbListFields(con,"data")

db.TERNARIA = 1
db.BINARIA1 = 2
db.BINARIA2 = 3

db.ABRIL = 1
db.DICIEMBRE = 2

db.NULL_VALUE = -99999999

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
  db.NOT_NULL <- db.NULL_VALUE
  df[ is.na(df) ] <- db.NOT_NULL
  df
}

db.discretize = function(df, maxbins = 10) {
  library(arules)
  
  # discretizeIfYouCan = function(x) {
  #   # head(x)
  #   if ( length(unique(x)) <= bins ) {
  #     return( factor(x, exclude = NULL, ordered = T ) )
  #   } else {
  #     return( discretize(x, categories = bins, method = "interval") )
  #   }
  # }
  # 
  # df_discr = as.data.frame(apply( df, 2, discretizeIfYouCan ))
  # for ( i in 1:ncol(df_discr) ) {
  #   df_discr[,i] = addNA( df_discr[,i], ifany = T )
  # }
  # df_discr
  
  
  for ( col in colnames(df) ) {
    x = df[,col]
    if ( !is.factor(x) ) {
      if ( length(unique(x)) <= maxbins ) {
        df[,col] = factor(x, exclude = NULL, ordered = T)
      } else {
        df[,col] = discretize(x, categories = maxbins, method = "interval", ordered = T)
        df[,col] = addNA( df[,col], ifany = T )
      }
    } 
  }
  df
}

db.discretize.soft = function(df, maxbins = 10) {
  for ( col in colnames(df) ) {
    x = df[,col]
    if ( !is.factor(x) && length(unique(x)) <= maxbins ) {
      df[,col] = factor(x, exclude = NULL)
    }
  }
  df
}

db.discretize.tend = function(df) {
  library(arules)
  primerCampo = which(colnames(df) == "marketing_activo_ultimos90dias_tend" )
  colnames(df)[primerCampo:ncol(df)]
  maxbins = 5
  
  for ( col in colnames(df)[primerCampo:ncol(df)] ) {
    x = df[,col]
    if ( !is.factor(x) ) {
      if ( length(unique(x)) <= maxbins ) {
        df[,col] = factor(x, exclude = NULL, ordered = T)
      } else {
        df[,col] = discretize(x, categories = c(-Inf,-1,-0.1,0.1,1,Inf), method = "fixed", ordered = T)
        df[,col] = addNA( df[,col], ifany = T )
      }
    } 
  }
  df
}

db.doDump = function() {
  # con = dbConnect(RSQLite::SQLite(), "db/producto_premium_201511_201604.sqlite")
  con = dbConnect(RSQLite::SQLite(), "db/data_all.sqlite")
  
  # sql = "SELECT * FROM data_visamaster_new WHERE foto_mes = 201604 ORDER BY numero_de_cliente"
  # sql = "SELECT * FROM visamaster WHERE foto_mes = 201604 ORDER BY numero_de_cliente"
  sql = "SELECT * FROM abril_mdos_visamaster WHERE foto_mes = 201604 ORDER BY numero_de_cliente"
  res = dbSendQuery(con, sql)
  data = dbFetch(res, n = -1 )
  
  # sql = "SELECT * FROM historicas WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM visamaster WHERE foto_mes = 201604 ) ORDER BY numero_de_cliente"
  # sql = "SELECT * FROM diciembre_historicas_posta WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM diciembre_historicas WHERE foto_mes = 201512 ) ORDER BY numero_de_cliente"
  sql = "SELECT * FROM abril_mdos_historicas WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM abril_mdos_visamaster WHERE foto_mes = 201604 ) ORDER BY numero_de_cliente"
  res = dbSendQuery(con, sql)
  historicas = dbFetch(res, n = -1 )
  
  # sql = "SELECT * FROM tendencias WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM data ) ORDER BY numero_de_cliente"
  # res = dbSendQuery(con, sql)
  # tendencias = dbFetch(res, n = -1 )
  # colnames(tendencias) = paste(colnames(tendencias),"_tend", sep = "")
  # colnames(tendencias)[1] = "numero_de_cliente"
  
  # tendencias = read.table("tendencias_new.tsv", header = T)
  tendencias = as.data.frame(t)
  colnames(tendencias) = paste(colnames(tendencias),"_tend", sep = "")
  colnames(tendencias)[1] = "numero_de_cliente"
  
  # trunc(tendencias[10, "mrentabilidad_tend"]) 
  
  # sql = "SELECT * FROM visamaster WHERE numero_de_cliente IN ( SELECT numero_de_cliente FROM data ) ORDER BY numero_de_cliente"
  # res = dbSendQuery(con, sql)
  # visamaster = dbFetch(res, n = -1 )
  
  head(data$numero_de_cliente, n = 50)
  head(historicas$numero_de_cliente, n = 50)
  head(tendencias$numero_de_cliente, n = 50)
  # head(visamaster$numero_de_cliente, n = 50)
  
  any((data$numero_de_cliente == historicas$numero_de_cliente) == FALSE)
  any((data$numero_de_cliente == tendencias$numero_de_cliente) == FALSE)
  # any((data$numero_de_cliente == visamaster$numero_de_cliente) == FALSE)
  
  joined = cbind(data, historicas, tendencias)
  sum( ncol(data), ncol(historicas), ncol(tendencias) )
  
  which(colnames(joined) == "numero_de_cliente")
  # 1 192 667
  
  joined = joined[,-c(192,667)]
  colnames(joined)
  
  # joined = read.table("db/diciembre_joined.tsv", header = T)
  
  rownames(joined) = joined$numero_de_cliente
  joined = joined [ , -1]
  head(joined)
  
  # head(joined$VisaMaster_finiciomora_tend, n =50)
  # head(inicioMora$VisaMaster_finiciomora, n =50)
  
  # inicioMora = read.table("tendencias_new_VisaMaster_iniciomora.tsv", header = T)
  # joined$VisaMaster_finiciomora_tend = inicioMora$VisaMaster_finiciomora
  
  # write.table(joined, "db/checkpoint/checkpoint.2.joined.tsv", sep = "\t", row.names = T, append = F)
  write.table(joined, "db/abril_dos_joined.tsv", sep = "\t", row.names = T, append = F)
rm}

db.getBigDataset = function(db = db.ABRIL, cual = db.TERNARIA, discret = T) {
  filename = "db/abril_dos_joined.tsv"
  
  if( db == db.DICIEMBRE ) {
    filename = "db/diciembre_joined.tsv"
  }
  
  df = read.table(filename, row.names = 1)
  
  df = df[,colnames(df) != "numero_de_cliente" ]
  df = df[,colnames(df) != "foto_mes" ]
  df = df[,colnames(df) != "participa" ]
  
  df$clase = as.factor(df$clase)
  
  if ( discret ) {
    df = db.discretize.soft(df)
    df = db.discretize.tend(df)
    df = db.clean(df)
  }
  
  if ( cual == db.TERNARIA) {
    # df$clasebinaria1 = NULL
    # df$clasebinaria2 = NULL
  } else if ( cual == db.BINARIA1 ) {
    df$clasebinaria1 = as.factor ( ifelse( df$clase == "BAJA+2", "POS", "NEG" ) )
    claseIndex = which( colnames(df) == "clase" )
    df = df[, -claseIndex]
    claseIndex = which( colnames(df) == "clasebinaria1" )
    colnames(df)[claseIndex] = "clase"
  } else if ( cual == db.BINARIA2 ) {
    df$clasebinaria1 = NULL
  }
  
  df
}

db.clean = function(df) {
  colsToRemove = c()
  
  for ( col in 1:ncol(df) )  {
    if ( length(unique(df[,col])) == 1 ) {
      colsToRemove = c(colsToRemove, col)
    }
  }
  
  df[,-colsToRemove]
}

db.getDataset = function(cual = db.TERNARIA, historicas = F) {
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