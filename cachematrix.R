## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.

## The following two functions are used to calculate the inverse of a matrix, 
## if the matrix has been cached before, the inverse of a matrix will be returned right now 
## rather than compute it repeatedly.

## Start of first function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL ##Initializing the inverse property to NULL
        
        ## Method for setting/resetting the matrix 'x'
        set <- function(y) {
                x <<- y # Reassigning the new matrix 'y' to 'x'
                inverse <- NULL # Reinitializing the inverse property to NULL
        }
        
        ## Method for getting the matrix 'x'
        get <- function() {
                x # Returning the matrix 'x'
        }
        
        ## Method for setting the inverse of the matrix
        setInverse <- function(inverse) {
                inverse <<- inverse ##Assigning the inverse property
        }
        
        ## Method for getting the inverse of the matrix
        getInverse <- function() {
                inverse ## Returning the inverse property
        }
        
        ## Returning a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## End of first function

## Start of second function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse() ## Returning a matrix that is inverse of the matrix 'x'
        
        ## Returning the inverse if it is already set
        if(!is.null(inverse) ) {
                message("Getting cached data...") ## Displays a message
                return(inverse) ## Returning the old result
        }
        
        data <- x$get()  ## Getting the uncalculated matrix if the inverse is not yet set
        
        inverse <- solve(data, ...) ## Calculating the inverse of the matrix
        
        x$SetInverse(inverse) ## Reassigning the inverse of the matrix
        
        inverse ## Printing the inverse of the matrix
}

#End of second function
