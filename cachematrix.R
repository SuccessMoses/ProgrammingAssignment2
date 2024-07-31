## The following two functions significantly reduce the cost of calculating
## the inverse of a matrix by caching rather than computing repeatedly

## The following function creates a special 'matrix', which is really
## a list containing some functions

makeCacheMatrix <- function(m=matrix()){
  
  inv <- NULL
  set <- function(y){
    
      m <<- y
      inv <<- NULL
  }
  
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get=get,
       setinverse=setinverse, getinverse=getinverse)
}


## The following calculates the inverse of a special 'matrix'

cacheSolve <- function(x,... ) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
      message('getting cached data')
      return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  
  inv
}