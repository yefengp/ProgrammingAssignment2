# Below are two functions that are used to create a special object that stores 
# a matrix and caches its inverse.
# 
# 
# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    im <- NULL    ## im is inverse matrix
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) im <<- solve
    getsolve <- function() im
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setsolve`
# function.

cacheSolve <- function(x, ...) {
    im <- x$getsolve()
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setsolve(im)
    im
}
