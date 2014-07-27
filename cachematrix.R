# makeCacheMatrix is a function that returns a list of functions and stores a martix and a cached value of the inverse of the matrix. 
# Contains the following functions:
# setMatr: set the value of the matrix.
# getMatr: get the value of the matrix.
# setInv:  get the cached value of inverse of the matrix.
# getInv:  get the cached value of inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
        # cacheInv: holds the cached Inverse of Matrix or NULL if nothing is cached.
	# Initially nothing is cached so set it to NULL
        cacheInv <- NULL
        
        # store a matrix
        setMatr <- function(newValue) {
                x <<- newValue	        # Holds the matrix.
                cacheInv <<- NULL	# Flushing out the previous value.
        }

        # returns the matrix 
        getMatr <- function() x

        # cache the Inverse of Matrix
        setInv <- function(InvMatr) {
                cacheInv <<- InvMatr
        }

        # get the cached value of Inverse of Matrix
        getInv <- function() {
                cacheInv
        }
        
        # return a list. Each named element of the list is a function.
	list(setMatr = setMatr, getMatr = getMatr, setInv = setInv, getInv = getInv)
}


## The following function calculates the inverse of a "special" matrix created with makeCacheMatrix.
# Before that it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        
        Inv <- x$getInv()       # gets the cached value.
        # if a cached value exists, returns it.
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        # otherwise get the matrix, calculate the inverse and store it in the cache.
        data <- x$getMatr()
        Inv <- solve(data)
        x$setInv(Inv)
        
        # return the inverse.
        Inv
}
