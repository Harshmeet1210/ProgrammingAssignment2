> makeCacheMatrix <- function(x = matrix()) { ## This function creates a special "matrix" object that can cache its inverse.
+     inv <- NULL
+     set <- function(y) {
+         x <<- y
+         inv <<- NULL
+     }
## function for getting the value of the matrix
+     get <- function() x
+     setInverse <- function(inverse) inv <<- inverse
+     getInverse <- function() inv
+     list(set = set,
+          get = get,
+          setInverse = setInverse,
+          getInverse = getInverse)
+ }
> cacheSolve <- function(x, ...) {
+     ## Return a matrix that is the inverse of 'x'
+     inv <- x$getInverse()
+     if (!is.null(inv)) {
+         message("getting cached data")
+         return(inv)
+     }
+     mat <- x$get()
+     inv <- solve(mat, ...)
+     x$setInverse(inv)
+     inv
+ }
> x=matrix(c(1,2,3,0,1,4,5,6,0),nrow = 3,ncol = 3,byrow = TRUE)
## x is an invertible matrix
> solve(x)
## it finds the inverse
