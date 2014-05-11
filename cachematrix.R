## These functions take advantage of caching to "save" computationally 
## expensive outputs for later use without having to recompute. Specifically,
## we are doign this for the computation of a matrix inverse

## First function: creates a special "matrix" object that can cache its inverse.
## Steps:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      set.inverse <- function(solve) m <<- solve
      get.inverse <- function() m
      list(set = set, get = get, 
           set.inverse = set.inverse,
           get.inverse = get.inverse)
}

## Second function: computes the inverse of the special "matrix" defined above;
## if this has been already been calculated, it retrieves it from the cache

cacheSolve <- function(x, ...) {
      m <- x$get.inverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$set.inverse(m)
      m
}