## These functions cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL # i will be our "inverse" and it's reset to NULL every time 
        # makeCacheMatrix() is called
        
        set <- function(y) { # takes an input matrix
                x <<- y      # saves the input matrix
                i <<- NULL   # resets the inverse to NULL
        }
        
        get <- function() x  # this function return the valuer of the original matrix
        
        setinverse <- function(inverse) i <<- inverse # this is called by cacheSolve()
        # during the first cacheSolve()
        
        getinverse <- function() i                    # this will return the cached value to 
        # cacheSolve() on subsequent accesses
        
        list(set = set, get = get,     # This is accessed each time makeCacheMatrix() is
             setinverse = setinverse,  # is called. This is a list of the internal function
             getinverse = getinverse)
        
        
}


## This function computes the inverse of the matrix returned by the funtion above. If the
## inverse has already been calculates, then this function should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) { #the input x is an object created by makeCacheMatrix
        
        i <- x$getinverse()  #accessess the object 'x' and gets the value of the inverse
        
        if(!is.null(i)) {  # if inverse was already cached (not NULL) ...
                
                message("getting cached data") # ... send this message to the console
                
                return(i) # ... and return the inverse, "returns ends the function
                # cacheSolve()
        }
        
        data <- x$get()  # we reach this code only if x$getinverse() returned NULL
        
        i <- solve(data, ...) # if i was NULL then we have to calculate the inverse
        
        x$setinverse(i) # stored the calculated inverse value in x
        
        i  # return the inverse to the code that called this function
}
