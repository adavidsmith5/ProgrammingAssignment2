## These functions will compute the inverse of a matrix and cache results.
> ## If the inverse of a matrix has already been computed, the function will
> ## return the cached result instead of doing new calculations.
> 
> ## This will create an object with the matrix and cache its inverse.
> 
> makeCacheMatrix <- function(x = matrix()) {
+   inv <- NULL
+   set <- function(y) {
+     x <<- y
+     inv <<- NULL
+   }
+   
+   get <- function() x
+   setinv <- function(solve) inv <<- solve
+   getinv <- function() inv
+   list(set = set, get = get, setinv = setinv, getinv = getinv)
+ }
> 
> 
> ## This function will calculate the inverse of the special matrix returned
> ## from the makeCacheMatrix. If inverse has already been calculated, the
> ## function will return the inverse from the cache.
> 
> cacheSolve <- function(x, ...) {
+   ## Return a matrix that is the inverse of 'x'
+   inv <- x$getinv()
+   if(!is.null(inv)) {
+     message("getting cached inverse")
+     print("cached")
+     return(inv)
+   }
+   print("not cached")
+   data <- x$get()
+   inv <- solve(data, ...)
+   x$setinv(inv)
+   inv
+ }
