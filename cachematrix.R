## function makeCacheMatrix and cachesolve are a functions to create a special object that stores a matrix and caches its inverse

## creates special matrix that can create an inverse of its matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL                 ## sets any stored inv to NULL
  set <- function(y) {        ## assign matrix value
    x <<- y
    inv <<- NULL
  }
  get <- function() x                                 ## get value of matrix
  setinverse <- function(inverse) inv <<- inverse     ## set inverse of matrix
  getinverse <- function() inv                        ## get inverse of matrix
  list(set = set, get = get,                          ## return list of matrix
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the mean of the special Matrix created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the Inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
CacheSolve <- function(x, ...) {
  inv <- x$getinverse()                              ## retrieve inverse
  
  if(!is.null(inv)) {                                ## if inverse exists, function checks if there is cached value
    message("pleae wait, fetching cached data")      ## retrieve inverse if exists
    return(inv)
  }
  data <- x$get()                                    ## if not get inverse of matrix
  inv <- solve(data, ...)                            ## solve for inverse of matrix
  x$setinverse(inv)                                  ## cache inverse of matrix
  return(inv)                                        ## return inv      
}
