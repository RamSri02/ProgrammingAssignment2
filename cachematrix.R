## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(), n) {
    inverse <- NULL
    set <- function(y)
      {
        x <<- y
        inverse <<- NULL
      }
    get <- function()
      {	
        dim(x) <- c(n, n)
        x
      }
    setInverse <- function(z) inverse <<- z
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse))
      {
        message("getting cached data")
        return(inverse)
      }
    Data <- x$get()
    if(nrow(Data) == ncol(Data) && det(Data))
      {
        inverse <- solve(Data)
        x$setInverse(inverse)
        inverse
      }
}
