# creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <-  NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversion <- function(inverse) i <<- inverse
  getinversion <- function() i
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}

#  computes the inverse of the special "matrix" returned by makeCacheMatrix.
#  if already computed, it will retrieve it from the cash

cacheSolve <- function(x, ...) {
  i <- x$getinversion()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversion(i)
  i
}


  



Write the following functions:
  
  1.  `makeCacheMatrix`: This function creates a special "matrix" object
that can cache its inverse.
2.  `cacheSolve`: This function computes the inverse of the special
"matrix" returned by `makeCacheMatrix` above. If the inverse has
already been calculated (and the matrix has not changed), then
`cacheSolve` should retrieve the inverse from the cache.

Computing the inverse of a square matrix can be done with the `solve`
function in R. For example, if `X` is a square invertible matrix, then
`solve(X)` returns its inverse.
