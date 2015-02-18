## The functions makeCacheMatrix and cacheSolve allows you to compute the 
## inverse of a matrix and cache its inverse. If the inverse of the matrix
## has already been computed, the inverse will be retrieved from the cache 
## instead of computing it repeatedly.

## makeCacheMatrix takes in a matrix and creates a special "matrix" object
## that can cache its inverse. A list of 4 functions is returned they are,
## get, set, getInvMatrix, setInvMatrix.

makeCacheMatrix <- function(x = matrix()) {
        
        # assigns NULL to the cache variable when the makeCacheMatrix is called
        invMatrix <- NULL
        
        # function for setting a new matrix. When a new matrix is set,
        # the cache is set to NULL
        set <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        
        # function to retrieve the stored matrix
        get <- function() x
        
        # function to set the cache to the computed inverse matrix
        setInvMatrix <- function(im) invMatrix <<- im
        
        # function to retrieve cache of the computed inverse matrix 
        getInvMatrix <- function() invMatrix
        
        # returns a list of the above 4 functions, 
        # get, set, getInvMatrix, setInvMatrix
        list(get = get, set = set, 
             getInvMatrix = getInvMatrix,
             setInvMatrix = setInvMatrix)
}

## cacheSolve takes in the special "matrix" object created by makeCacheMatrix
## and computes the inverse of the special "matrix". If the inverse of the
## matrix has already been computed, cacheSolve retrieves the inverse from the
## cache

cacheSolve <- function(x, ...) {
        
        # retrieves the cache containing the inverse of the matrix in the 
        # special "matrix" object.
        invMatrix <- x$getInvMatrix()
        
        # checks if the cache returned is NULL. If not NULL, retrieves the 
        # cached result, returns it and exits the function.
        if(!is.null(invMatrix)){
                message("getting cached data")
                return(invMatrix)
        }
        
        # if the cache is NULL, get the matrix from the special "matrix" 
        # object and computes its inverse.
        data <- x$get()
        invMatrix <- solve(data,...)
        
        # after computing the inverse of the matrix, use the setInvMatrix
        # function to store it in the cache and return the result
        x$setInvMatrix(invMatrix)
        invMatrix
}