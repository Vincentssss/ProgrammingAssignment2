## Carl Vincent R. Asis

library(MASS)
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}                    
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x            
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}



cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("Catching the cached data!")     
    return(inv)                        
  }
  mat <- x$get()
  inv <-solve(mat, ...)                     
  x$setinv(inv)
  inv
}