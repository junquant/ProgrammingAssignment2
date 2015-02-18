# makeCacheMatrix 
# Description : 
# Takes in a matrix and saves it in the variable x
# Returns a list of 4 functions get, set, getInvMatrix, setInvMatrix
# get() - returns the matrix stored in variable x
# set() - sets the matrix stores in variable x with y 
#       - resets the cached invMatrix to NULL
# getInvMatrix() - 
# setInvMatrix() - 
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(im) invMatrix <<- im
        getInvMatrix <- function() invMatrix
        list(get = get, set = set, 
             getInvMatrix = getInvMatrix,
             setInvMatrix = setInvMatrix)
}
# important is form and comment


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInvMatrix()
        if(!is.null(invMatrix)){
                message("getting cached data")
                return(invMatrix)
        }
        data <- x$get()
        invMatrix <- solve(data,...)
        x$setInvMatrix(invMatrix)
        invMatrix
}

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}