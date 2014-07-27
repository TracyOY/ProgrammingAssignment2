# The makeCacheMatrix function creates a special "matrix" object that can cache
# its inverse, and returns a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse matrix value to NULL
        inv <- NULL
        # set the value of the matrix and the inverse
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the value of the inverse matrix
        setinv <- function(inversion) inv <<- inversion
        # get the value of the inverse matrix
        getinv <- function() inv
        # return a list of functions
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


# The cacheSolve function calculates the inverse of the special "matrix" created
# with the makeCacheMatrix function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the 
# inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check if the inverse is already cached,
        # if so, get the inverse from the cache
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        # if inverse is not cached, we calculate the inverse using solve(matrix)
        mat <- x$get()
        inv <- solve(mat, ...)
        # cache the inverse calculated
        x$setinv(inv)
        # return the inverse
        inv
}
