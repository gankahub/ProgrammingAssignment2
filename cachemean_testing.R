makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  ##adding this part to test run time of code
  start_time <- Sys.time()
  
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    
    ##print if null
    end_time <- Sys.time()
    time_duration <- end_time - start_time
    print(time_duration)
    
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  
  ##added section 
  end_time <- Sys.time()
  time_duration <- end_time - start_time
  print(time_duration)
  
  m
}