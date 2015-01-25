##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setMatrixInversion<-function(solve) m<<- solve
  getMatrixInversion<-function() m
  list(set=set, get=get,
       setMatrixInversion=setMatrixInversion,
       getMatrixInversion=getMatrixInversion)
}

## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getMatrixInversion()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setMatrixInversion(m)
  m
}

##Testing of functions
a<-makeCacheMatrix()
a$set(matrix(0:3,2,2))
cacheSolve(a)