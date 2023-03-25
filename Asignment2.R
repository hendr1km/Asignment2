makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y) {
  x <<- y
  inv <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) inv <<- inverse
 getinverse <- function() inv
 list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

cacheinverse <- function(x, ...) {
 inv <- x$getinverse()
 if(!is.null(inv)) {
  message("getting cached data")
  return(inv)
 }
 matrix_to_invert <- x$get()
 inv <- solve(matrix_to_invert, ...)
 x$setinverse(inv)
 inv
}

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 i <- x$getinverse()
 if(!is.null(i))  {
  message("getting cached data")
  return(i)
 }
 data <- x$get()
 i <- solve(data, ...)
 x$setinverse(i)
 i
}

