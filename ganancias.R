ganancia.ternaria = function( data, lev = NULL, model = NULL ){
  suma = 0 ;
  
  
  
  #todo !
  for( i in 1:length(data) )
  {
    if( probs[ i, "BAJA+2"]  > ( 250/8000)   ){ suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
    } ;
  }
  
  suma
}

ganancia.binaria1 = function( probs, clases ){
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    if( probs[ i, "POS"]  > ( 250/8000)   ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
    } ;
  }
  
  suma
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