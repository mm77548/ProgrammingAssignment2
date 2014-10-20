## These functions will take a matrix, check to see if the inverse has been calculated. 
## If the inverse has already been calculated it will pull it from the cache and display it.
## If an inverse has not been calculated yet, it will calculate it and display it then store it in the cache.


## The first function, makeCacheMatrix creates a matrix (a type of vector) to 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## cacheSolve calculates the inverse of the matrix created with the above function. 
## It first checks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
       
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)    ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inv <- solve(data, ...)    ## need to use solve() to calulate inverse
  x$setinverse(inv)
  inv
    
}
