## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(), n) {
    inverse <- NULL
    ##Explicitly setting the value of x
    set <- function(y)
      {
        x <<- y
        inverse <<- NULL
      }
    ##In order to obtain a matrix I have used the concept of setting the dimensions of x 
    ##returns a (n by n) sqaure matrix 
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
        
    inverse <- x$getInverse()
    if(!is.null(inverse))
      {
        message("getting cached data")
        return(inverse)
      }
    Data <- x$get()
    ##condition checks for the property of square invertible matrix 
    ##Square matrix (no. of rows & columns should be equal)
    ##determinant of a matrix should not be equal to zero
    if(nrow(Data) == ncol(Data) && det(Data))
      {
        inverse <- solve(Data)
        x$setInverse(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
      }
}
