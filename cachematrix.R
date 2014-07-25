## This set of two functions stores the matrix and its inverse to the 
## environment provided by makeCacheMatrix for further computation.

## makeCacheMatrix stores a matrix and its inverse for further computation
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # generated empty inverse matrix 'i' when first assigning function
    set <- function(y) {
        x <<- y  # store 'y' to var 'x' in envir
        i <<- NULL  # empty (set to NULL) var 'i' in envir
    }
    get <- function() x   # get func. to check 'get for computation'
    getinverse <- function() i   # external get function to check for stored inverse matrix 'i' of matrix x
    setinverse <- function(inverse) i <<- inverse   # store inverse matrix 'inverse' of matrix 'x' to environment var 'i'

    # cached + returned list element in function environment, providing functions "set, get, setinverse, getinverse")
    list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix stored in the makeCacheMatrix environment
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()  # check if inverse is cached
    if(!is.null(i)) {  # if inverse is cached, show msg + return cache
            message("Retrieving cached inverted matrix")
            return(i)
    }
    origMat <- x$get()  # retrieve original matrix
    i <- solve(origMat, ...)  # calculate inverse of origMat
    x$setinverse(i)  # store origMat in makeCacheMatrix environment
    i  # output inverse matrix
}


# e.g.
#foo <- matrix(c(1,0,5,2,1,6,3,5,0),ncol=3,nrow=3)
#
#bar <- makeCacheMatrix()
#bar$set(foo)
#cacheSolve(bar)

