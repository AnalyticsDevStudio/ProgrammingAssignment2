## Caching the Inverse of a Matrix

## The makeCacheMatrix function will create a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() {
    
    x
    
  }
  setinv = function(inverse) {
    
    inv <<- inverse
  
  }
  
  getinv = function() {
    
    inv
    
  }
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## The cacheSolve function will compute the inverse of the special "matrix" 
## returned by makeCacheMatrix function.
## If the inverse has already been calculated and it has not changed, then 
## the cacheSolve function should load the inverse from the cache.

cacheSolve <- function(x, ...) {

  inv = x$getinv()
  
  if (!is.null(inv)){
    message("Getting Cached Data...")
    inv
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  inv

}
