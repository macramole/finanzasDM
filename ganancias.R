ganancia.ternaria = function( probs, clases, threshold = 250/8000  ){
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    # if( probs[ i, "BAJAMASDOS"]  > ( 250/8000)   ){ suma <- suma + if( clases[i]=="BAJAMASDOS" ) { 7750 } else { -250 }
    if( probs[ i, "BAJA+2"]  > ( threshold ) ) { suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
    } ;
  }
  
  return( suma )
}

ganancia.binaria1 = function( probs, clases, threshold = 250/8000 ){
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    if( probs[ i]  > threshold ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
    } ;
  }
  
  suma
}

ganancia.binaria1.umbral = function( probs, clases, rango = seq(0,1,0.01) ){
  
  largo = length( clases ) ;
  
  dfThresholds = data.frame()

  #for ( threshold in seq(0,0.01,0.0001) ) {  
  for ( threshold in rango ) {
    suma = 0 ;
    
    for( i in 1:largo )
    {
      if( probs[ i]  > threshold ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
      } ;
    }
    
    dfThresholds = rbind(dfThresholds, c(threshold, suma))
  }
  
  colnames(dfThresholds) = c("Threshold", "suma")
  dfThresholds
}

ganancia.binaria2 = NULL

ganancia.oversampled = function( probs, clases ){
  suma = 0
  largo = length( clases )
  
  for( i in 1:largo )
  {
    if( probs[ i, "POS"]  > ( 75000/82750 )   ){ 
      suma <- suma + if( clases[i]=="POS" ) { 7750 / 313 } else { -250 }  
    }
  }
  
  suma
}