## makeCacheMatrix creates a matrix whose inverse is to be stored in the cache. 
## cacheSolve takes the matrix and calculates its inverse, if needed. If not, it returns it from the cache. 

## Creates matrix to be inverted and stored in the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## Takes matrix and inverts it or just retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 

    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("Retrieving your cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
  }
