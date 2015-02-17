## Functions to calculate the inverse of a matrix
## It caches the results to avoid repeated operations


# Execute how-to
#----------------------------------------------
# Create matrix
# c=rbind(c(1, -1/4), c(-1/4, 1))

# Create cache with a key
# cache <- makeCacheMatrix(c)

# Get inverse
# cacheSolve(cache)
# If not cached, read message: Calculating inverse 
# If cached, read message: Getting cached data 


# Update matrix
# cache$set(c)

# --------------------------------------------


## Creates functions to get and set data, and to get cached inverse
# x - initial matrix data for this cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Tries to recover matrix inverse from cache
## If not present, calculate inverse and update cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  message("Calculating inverse")
  data <- x$get()
  #cat("Got data: ", data)
  m <- solve(data, ...)
  #cat("Inverse: ", m)
  x$setinverse(m)
  m
}
