## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than computing it repeatedly
## This function assumes that the matrix supplied is always invertible

## Here is some code you can use to test:
## > f <- makeCacheMatrix()
## > MyMatrix <- matrix(c(1,5,9,6,5,3,2,4,3), 3, 3) 
## > f$set(MyMatrix)
## > cacheSolve(f)

## Functons below:

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("Retrieving from cache")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
