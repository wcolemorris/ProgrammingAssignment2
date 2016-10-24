## This script will create a function that can create, invert,
## and cache the inverse of a matrix for optimization

## Container function to create and cache a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function to seek cached inverse or calculate if missing

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          print("getting cached inverse")
          return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
