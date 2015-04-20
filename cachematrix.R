

##makeCacheMatrix takes as input a matrix (or a default empty matrix)
##and creates a list containing four functions:  'get' and 'set'
##cache and retrieve a matrix; 'setinv' and 'getinv' caches and retrieves
##the cacheSolve computed inverse of the original matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(inv) i <<- inv
     getinv <- function() i
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


##cacheSolve takes as input the list of functions created by
##makeCacheMatrix and first checks to see if an 
##inverse for the original matrix exists and then returns it.
##Otherwise it computes the inverse and caches the result for
##later lookup as well as returning the value

cacheSolve <- function(x, ...) {
	i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     matrix <- x$get()
     i <- solve(matrix, ...)
     
     x$setinv(i)
     i		
}
