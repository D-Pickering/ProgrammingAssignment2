# makeCacheMatrix creates a list containing a function to
#    - set the value of the matrix
#    - get the value of the matrix
#    - set the value of inverse of the matrix
#    - get the value of inverse of the matrix

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

# The next function returns the inverse of the matrix. It first checks
# the inverse has already been computed. If it returns as TRUE, it gets  
# the result and skips the computation. If not, it computes the inverse,  
# sets the value in the cache with the setinverse function.

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv    
}

# A small example of use for the two functions is as follows:
#
# x <- matrix(1:4, nrow = 2, ncol = 2)
# xInv <- makeCacheMatrix(x)
# cacheSolve(xInv)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5