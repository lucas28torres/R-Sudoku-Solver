#funció que comprova si un número n és possible
#Per transformar la matriu en sudoku, cal usar as.sudoku(M), del paquet sudokuAlt

#M=matrix(c(0,0,6,0,9,0,0,5,0,8,3,4,0,0,5,0,0,0,5,9,1,2,8,0,0,6,0,0,0,0,4,3,0,0,0,2,0,0,9,7,0,1,5,4,8,0,0,8,5,0,0,6,7,0,1,0,0,9,7,0,4,0,5,0,0,3,0,0,0,0,1,6,6,4,0,0,5,0,2,8,0),ncol=9)

#x serà la fila, y la columna

possible <- function(x, y, n, M) {
  ## ¿Está n en fila x o en column y?
  if (n %in% M[x, ] || n %in% M[, y]) {
    return(FALSE)
  }
  ## ¿Se encuentra n en la submatriz definido por fila x y columna y?
  i <- ceiling(x / 3)
  j <- ceiling(y / 3)
  if (n %in% M[(i * 3 - 2):(i * 3), (j * 3 - 2):(j * 3)]) {
    return(FALSE)
  }
  return(TRUE)
}


Sudoku=function(M){

  for(x in 1:nrow(M)){
    for(y in 1:ncol(M)){
      if(M[x,y] == 0){
        for(n in 1:9){
          if(possible(x,y,n,M)){
            M[x,y] = n
            print(M)
          }
        }
      }
    }
  }
  res=as.sudoku(M)
  return(res)

}
