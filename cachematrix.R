## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeVector <- function(x = numeric()) {
  # The first function, makeVector creates a special “vector”, which is really 
  # a list containing a function to:
  
  # set the value of the vector
  # get the value of the vector
  # set the value of the mean
  # get the value of the mean
  # copied from the README.md
  
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
  # The following function calculates the mean of the special “vector” created with 
  # the above function. However, it first checks to see if the mean has already been 
  # calculated. If so, it gets the mean from the cache and skips the computation. 
  # Otherwise, it calculates the mean of the data and sets the value of the mean in 
  # the cache via the setmean function.
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # This function computes the inverse of the special "matrix" returned 
  # by makeCacheMatrix above. If the inverse has already been calculated 
  # (and the matrix has not changed), then the cachesolve should retrieve 
  # the inverse from the cache.
}
