## functions that cache the inverse of a matrix

## Created a matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
## initialize inverse property
  m<-NULL
  ## set the matrix
  set<-function(y){
  x<<-y
  m<<-NULL
}
## get the matrix
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
cacheSolve <- function(x=matrix(), ...) {
## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    ## Just return the inverse if its already set
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
     ## Get the matrix from our object
    matrix<-x$get()
     ## Calculate the inverse using matrix multiplication
    m<-solve(matrix, ...)
    ## set the matrix to the object
    x$setmatrix(m)
     ## Return the matrix
    m
}
