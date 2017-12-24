## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function (y){
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function () x
    
  setsolve <- function (solve) invMatrix <<- solve
  
  getsolve <- function () invMatrix

  list (set = set, get = get, setsolve = setsolve, getsolve = getsolve )
  }


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getsolve() 
  
  if (!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  dataMat <- x$get()
  invMatrix <- solve(dataMat, ...)
  x$setsolve(invMatrix)
  invMatrix
}

