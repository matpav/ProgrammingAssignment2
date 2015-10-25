
## makeCacheMatrix function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x= matrix()) {
  Inv <- NULL
  set <- function(y){
    x<<- y
    Inv<<- NULL
  }
  get <- function() x
  
  setInverse <- function(InvVal) {
    Inv <<- InvVal
    return = Inv
  }
  getInverse <- function() Inv
  list((set = set, get = get,setInverse = setInverse,getInverse = getInverse))
  
}

## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,  
## the cachesolve should retrieve the inverse from the cache.

## it first checks if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the matrix 
## in the cache via the setmatrix.

cacheSolve <- function(x, ...) {
  Inv<- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data,...)
  x$setinverse(Inv)
  Inv
}
