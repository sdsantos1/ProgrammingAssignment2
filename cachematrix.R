## This program will create a cache of the matrix, if it not yet cached, 
## using the makeCacheMatrix function. Then, will get the inverse of a matrix by calling the 
## cacheSolve function

## This wil create a special "object" that will cache the given matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setMatrix <- function(z) i <<- z
  getMatrix <- function() i
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}



## This function will return the inverse of the cached matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a<-x$getMatrix()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  data<-x$get()
  a<-solve(data)
  x$setMatrix(a)
  data
}