## First function creates a special "matrix"
## Second function calculates the inverse of the matrix

## The function has a matrix as an argument and creates for it other four functions:
## set, get, getinv and setinv

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix(nrow=nrow(x),ncol=ncol(x)) 
  set <- function(y) { 
    x <<- y 
    m <<- matrix(NULL,nrow=nrow(x),ncol=ncol(x))
  }
  get <- function() x 
  setinv <- function(inv) m <<- inv 
  getinv <- function() m 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function calculates the inverse of a matrix if it's not already calculated and 
## returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() 
  if (sum(is.na(m))==0) {
    ## If we don't have any NA value in the matrix means the inverse it is already calculated
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...) 
  x$setinv(m) 
  m 
}
