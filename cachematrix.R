## set function with 'x' & 'inv' variables accepting global values.'y' is 
# the argument of set function.
## get function - returns variable 'x'
# setinverse function - has argument 'solv' and variable 'inv'. 
# The global value of 'solv' is assigned to 'inv'.
# getinverse function just returns 'inv' value.
# 


makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solv) inv <<- solv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the function cacheSolve 1st surchases for availability of inversed matrix 
## in cache memory, if it is there then it simply returns the value 'inv' - which
# is the value of inverse matrix, otherwise it calculates the inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}
