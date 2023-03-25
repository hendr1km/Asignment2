makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL # Setting a blank for for the inversed matrix
 set <- function(y) {
  x <<- y # the set matrix will become the new get function (x)
  inv <<- NULL 
 }
 get <- function() x # get command prints the loaded matrix (which was loaded into the matrix)
 setinverse <- function(inverse) inv <<- inverse # sets the inverse to global env
 getinverse <- function() inv # prints out the inverse
 list(set = set, get = get, # creates a list containing the functions
      setinverse = setinverse,
      getinverse = getinverse)
}

cacheinverse <- function(x, ...) { # loads the inverse into the "cache"-function 
 inv <- x$getinverse() 
 if(!is.null(inv)) {
  message("getting cached data")
  return(inv) # if the function is not NULL and therefore a inverse has been created yet, it returns the following
 }
 matrix_to_invert <- x$get() # otherwise it creates the inv based on the matrix (x)
 inv <- solve(matrix_to_invert, ...)
 x$setinverse(inv)
 inv
}

cacheSolve <- function(x, ...) { # the final functionality: this function will create the inverse if it hasent beent already (the matrix must be leaded in the get function before)
  # otherwiese it will create a inversed one
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

