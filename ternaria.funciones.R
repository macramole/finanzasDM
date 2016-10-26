## Funciones de ganancia
#definicion  funcion ganancia_puntual para nuestro problema
ganancia_puntual = function( probs, clases, prob_puntual )
{
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    if( probs[ i, "BAJA+2"]  == prob_puntual   ) { 
      suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
    } 
  }
  
  return( suma )
}

getVotaciones = function(modelos = list(), prob_listas = list()) {
  votacion = list()
  
  for ( i in 1:length(modelos) ) {
    votacion[[i]] = apply(modelos[[i]], 1, function(x) {
      if ( x["BAJA+2"] %in% prob_listas[[i]] ) {
        1
      } else {
        0
      }
    })
  }
  
  as.data.frame(votacion, col.names = paste("Modelo", 1:length(modelos), sep = ".") )
}

# getGananciaVotacion = function( modelos = list(), prob_listas = list(), pesos = c() ) {
#   votacion = list()
#   
#   for ( i in 1:length(modelos) ) {
#     votacion[[i]] = apply(modelos[[i]], 1, function(x) {
#       if ( x["BAJA+2"] %in% prob_listas[[i]] ) {
#         1
#       } else {
#         0
#       }
#     })
#   }
#   
#   votacion = as.data.frame(votacion, col.names = paste("Modelo", 1:length(modelos), sep = ".") )
#   
#   
# }

#definicion  funcion ganancia_lista para una lista de probabilidades
ganancia_lista = function( probs, clases, prob_lista )
{
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    if( probs[ i, "BAJA+2"] %in% prob_lista   ){ suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
    } ;
  }
  
  return( suma )
}


hojas_positivas = function( probs, clases )	
{
  
  vunicos =  unique( probs[ , "BAJA+2"], incomparables=FALSE ) ;
  
  hojas_lista <- c() ;
  
  largo = length( vunicos ) ;
  
  
  for( i in 1:largo )
  {
    vgan =  ganancia_puntual( probs, clases, vunicos[i] )  
    if( vgan>0 )  hojas_lista = append( hojas_lista,  vunicos[i] ) ;
  }
  
  
  return(   hojas_lista  )
}