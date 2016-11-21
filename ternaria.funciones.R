## Funciones de ganancia

ganancia = function( probs, clases, prob = 250/8000 )
{
  suma = 0 ;
  largo = length( clases ) ;

  for( i in 1:largo )
  {
    if( probs[ i ]  > prob   ){ suma <- suma + if( clases[i]=="BAJA+2" || clases[i]=="POS" ) { 7750 } else { -250 }
    } ;
  }

  return( suma )
  
  # g = apply( cbind(probs,as.numeric(clases)), MARGIN = 1, FUN = function(x) { 
  #     if ( x[1] > prob ) {
  #       if (x[2]=="BAJA+2" || x[2]=="POS") {
  #         return(7750)
  #       } else {
  #         return(-250)
  #       }
  #     }
  #     0
  #   } )
  # 
  # a = c(1,2,3)
  # Reduce(function(x) { if ( x == 1)  }, a)
}


umbral_ganancia_optimo = function( probs, clases, rango = seq(0.02,0.1,0.001))
{
  
  vgan_maxima = -9999999.0 ;
  vumbral =  0 ;
  
  
  #itero de 0.02 a 0.10  en incrementos de 0.01

  for( i in rango ) 
  {
    vgan = ganancia(  probs, clases, i )
    
    if( vgan > vgan_maxima )
    {
      vgan_maxima =  vgan ;
      vumbral =  i ;
    }
    
  }
  # for( i in seq(vumbral - 0.1,vumbral + 0.1,0.01) ) 
  # {
  #   vgan = ganancia(  probs, clases, i )
  #   
  #   if( vgan > vgan_maxima )
  #   {
  #     vgan_maxima =  vgan ;
  #     vumbral =  i ;
  #   }
  #   
  # }
  # for( i in seq(vumbral - 0.01,vumbral + 0.01,0.001) ) 
  # {
  #   vgan = ganancia(  probs, clases, i )
  #   
  #   if( vgan > vgan_maxima )
  #   {
  #     vgan_maxima =  vgan ;
  #     vumbral =  i ;
  #   }
  #   
  # }
  
  return( vumbral )
}



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

getPositivosDeLista = function( probs, prob_lista )
{
  listaPos = c()
  
  for( i in 1:nrow(probs) )
  {
    if( probs[ i, "BAJA+2"] %in% prob_lista   )
    { 
      listaPos = c(listaPos, rownames(probs)[i]  )
    } ;
  }
  
  return( listaPos )
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