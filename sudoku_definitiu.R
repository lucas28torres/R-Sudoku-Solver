#funció que comprova si un número n és possible



#x serà la fila, y la columna
possible=function(x,y,n,M){
  for(i in 1:9){
    if(M[x,i]==n){
      return(FALSE)
    }
  }
  for (i in 1:9) {
    if(M[i,y]==n){
      return(FALSE)
    }
  }
  #MIRAR CADA SUBMATRIU DE 3 ELEMENTS, algo no funciona...
  for(i in 1:3){
    for(j in 1:3){
      if(M[i:(i+2),j:(j+2)]==n){
        return(FALSE)
      }
    }
  }
  return(TRUE)
}
  

Sudoku=function(M){
  
  for(x in 1:nrow(M)){
    for(y in 1:ncol(M)){
      if(M[x,y]==0){
        for(n in 1:9){
          if(possible(x,y,n,M)){
            M[x,y]==n
            print(M)
          }
        }
      }
    }
  }
  res=as.sudoku(M)
  return(res)
  
}








