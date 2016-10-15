## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      # holds the cached value or NULL if nothing is cached
      # initially nothing is cached so set it to NULL
      inv <- NULL
      # store a matrix
      set <- function(y) {
            x <<- y
            # since the matrix is assigned a new value, flash the cache
            inv <<- NULL
      }
      
      # returns the stored matrix
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      # return a list. Each named element of the list is a function
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        
      # get the cached value
inv <- x$getInverse()
# if a cached value exists return it
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
# otherwise get the matrix, caclulate the inverse and store it in
# the cache
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      # return the inverse
      inv
}

