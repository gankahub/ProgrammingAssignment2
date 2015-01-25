## This code will invert a matrix

## OVERALL SUMMARY
##On the first pass it will invert the matrix and cache the results
## On subsequent passes it will use the cached matrix to reduce processing time

##lists functions which will be read into a list vector
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##sets value of the vector 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##gets value of the vector
  get <- function() x
  ##sets value of the matrix
  setmatrix <- function(matrix) m <<- matrix
  ##gets valud of the matrix
  getmatrix <- function() m
  ##generates a list
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## inverts the matrix
cacheSolve <- function(x, ...) {
  ##reads matrix to m
  m <- x$getmatrix()
  ##if this not the first time m is used
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if this is the first time the matrix is inverted
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
