## the function makeCacheMatrix creates a matrix and calculates its inverse


## We will define the inverse as NULL and if the matrix is not invert-able we will get NULL
## If inverted it will stores the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- Null
  set <- function(y){
    x <<- y
    m <<- Null
  }
  
  get <- function()x
  setinverse <- function(inverse)m <<- inverse
  getinverse <- function()m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Checks if the inverse is stored in cache, or it will calculate it and return the inverse
## if it is invert-able

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}