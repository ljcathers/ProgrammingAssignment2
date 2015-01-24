## Put comments here that give an overall description of what your
## functions do


## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.   First the matrix is defined, then a variable is assigned for the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
 # Set the matrix inverse equal to NULL
 mat_inv <- NULL 
 	   # assign an argument to define the matrix x using the set function
        set <- function(y){
                x <<- y
                mat_inv <<- NULL
        }
	   # get returns the original matrix
        get <- function() x
        # set the value of mat_inv as the inverse of x
        setInverse <- function(solve) mat_inv <<- solve
        # get the inverse of matrix x
        getInverse <- function() mat_inv
        # Create a list of the functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the inverse of matrix x, store it as x_inv
        x_inv <- x$getInv() 
        # Check to see if the inverse of matrix x has been computed, if it has than return the cached matrix, if not it will be null
       if(!is.null(x_inv)) { 
	  message("getting cached data")
	  return(x_inv) 
      }
      # If there is no cached inverse of matrix x, use get to get original matrix
      data <- x$get()
      # Solve for the inverse of matrix x
      x_inv <- solve(data) 
      # Use setInv to cache the newly computed inverse matrix
      x$setInv(x_inv)
      # Return the inverse matrix
      x_inv
}
