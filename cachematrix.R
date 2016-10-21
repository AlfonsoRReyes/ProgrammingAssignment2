## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeVector <- function(x = numeric()) {
  # copied from the README.md
  # The first function, makeVector creates a special “vector”, which is really 
  # a list containing a function to:
  
  m <- NULL
  # set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the value of the vector
  get <- function() x
  # set the value of the mean
  setmean <- function(mean) m <<- mean
  # get the value of the mean
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

  m <- NULL
  # set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the value of the matrix
  get <- function() x  
  # set the value of the mean
  setinverse <- function(inverse) m <<- inverse
  # get the value of the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  # This function computes the inverse of the special "matrix" returned 
  # by makeCacheMatrix above. If the inverse has already been calculated 
  # (and the matrix has not changed), then the cachesolve should retrieve 
  # the inverse from the cache.
  i <- x$getinverse()               # get the inverse if it cached
  if(!is.null(i)) {                 # if matrix not empty
    message("getting cached data")
    return(i)                       # return cached value
  }
  data <- x$get()                   # get matrix
  i <- solve(data, ...)             # calculate the inverse
  x$setinverse(i)                   # set the inverse
  i                                 # return calculated value
}
