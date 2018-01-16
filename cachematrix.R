## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(), n) {
    ## n - Denotes the no. of rows and columns to construct a square matrix
    
    ## variable created to store the final output
    inverse <- NULL
    
    ##Explicitly storing the value of x through set function
    set <- function(y)
      {
        x <<- y
        inverse <<- NULL
      }
    
      ##In order to obtain a matrix I have used the concept of setting the dimensions of x 
     ##And the function returns a (n by n) sqaure matrix 
    get <- function()
      {	
        dim(x) <- c(n, n)
        x
      }
    
    ##Returns the Inverse of x 
    setInverse <- function(z) 
      {
        inverse <<- z
      }
    
    ##Returns the value of inverse that was stored initially 
    ##And the corresponding values of Inverse of x after the execution of Solve(x)
    getInverse <- function() 
      {
        inverse
      }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ##Assigning the value from the above function
    inverse <- x$getInverse()
    
    ##returns the inverse value cached from the memory
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
        ##Calculates the Matrix Inverse
        inverse <- solve(Data)
        
        x$setInverse(inverse)
        
        ## Return a matrix that is the inverse of 'x'
        inverse
      }
}
