> makeCacheMatrix <- function(x = matrix()) {
+     inv <- NULL
+     set <- function(y) {
+         x <<- y
+         inv <<- NULL
+     }
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
> solve(x)
