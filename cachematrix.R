## The makeCacheMatrix and cacheSolve functions work together to store/retrieve a matrix
## and to calculate the inverse of that matrix, cache that value of that inverse so 
## if it is needed again, it won't need to recalculate the inverse of the matrix which can
## be a long process for larger matrices.

## The makeCacheMatrix is a function with nested functions inside that cache a matrix(set)
## display/return the matrix (get), calculate the inverse (solve) of the matrix (setinv)
## and return the inverse of the matric (getinv).  It includes a list() of the function values
## for retrieval

makeCacheMatrix <- function(x = matrix()) {
	## x a square matrix that can be inverted
	## functions: 
	##  set: the matrix based on input
	##  get: the matrix that has been stored
	##  setinv: the inverse of the stored matrix
	##  getinv: the inverse of the stored matrix
	## list( <function1>, function2,...) of function values

        # inverse default value to null
        inv = NULL  
        
        #function to get the matrix via input
        set = function(y) {
                # scope of these variables is outside the current scope so use <<-
                x <<- y
                inv <<- NULL
        }
        
        #function to return the matrix unchanged
        get = function() x  
      
        #function to set value to the inverse of the original matrix
        setinv = function(inverse) inv <<- inverse   
        
        #function to get the matrix used for inversion of the original matrix
        getinv = function() inv
        
        # list( <function1>, function2,...) inputs to cacheSolve()
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The cacheSolve is a function that gets the makeCacheMatrix stored matrix and either calculates
## the inverse of the matrix and caches that inverse, or returns the already cached value of the matrix

cacheSolve <- function(x, ...) {
        ## x output of makeCacheMatrix()
        ## return: inverse of matrix input to the original matrix
        
        # set inv matrix by getting the value from the list getinv
        inv = x$getinv() 
        
        # if inverse has already been calculated (not null)
        if (!is.null(inv)){
                
                #message is better than print because it doesn't show as a vector, good for testing/debugging
                message("getting cached data") 
                # get it from cache and don't recompute it. 
                return(inv)
        }
        
        # otherwise get the original matrix
        mat.data = x$get()
        # and calculate inverse
        inv = solve(mat.data, ...)  
        
        # sets value of inverse in cache list via setinv function.
        x$setinv(inv) 
        
        #return the inverse of the matrix
        return(inv)
}
