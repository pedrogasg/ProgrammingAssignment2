## Put comments here that give an overall description of what your
## functions do

## A special matrix for coursera that have the memoization feuture of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Chache Resolver to check if the inver matrix is cached.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(is.null(inv)) {
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
  }else{
    message("getting cached data.")
  }
  inv
}
