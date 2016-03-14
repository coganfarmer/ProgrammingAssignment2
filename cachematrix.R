##The makeCacheMatrix Function creates the matrix object that caches its inverse. 

##makeCacheMatrix contains 4 functions: set, get, setinv, getinv.
##(1)set is a function that changes the vector stored in the main function.
##(2)get is a function that returns the vector x stored in the main function.
##(3)setinv and getinv are functions very similar to set and get.
##   They don't calculate the mean, they simply store the value of the input in the variable n.
##   into the main function makeVector (setinv) and return it (getinv).

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinv <- function(solve) n <<- solve
  getinv <- function() n
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The Function cacheSolve computes the inverse of the special matrix 
## It uses the special matric returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache. 
## If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, 
## n calculates the inverse, and x$setinv(n) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getinv()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinv(n)
  n
}
