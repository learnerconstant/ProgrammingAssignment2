#' Defines functions to cache matrix inversion so that the inversion does not have to be calculated
#' more than once in order to speed up processing.

makeCacheMatrix <- function(x = matrix()) {
        #' Defines a set of functions to generate a special matrix that can hold the inverse in addition to
        #' matrix itself.
        #'
        #' @param x matrix. Input matrix
        #' @return The list of functions to operate on the matrix
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        #' Defines optimized matrix inverse calculation by looking up the cache to see if
        #' the inverse is already available for the special matrix.
        #' If the inverse is not found in cache, it calculates the inverse and stores in cache.
        #'
        #' @param x makeCacheMatrix. Special matrix returned by makeCacheMatrix
        #' @return The inverse of the matrix
        inv <- x$getinverse()
        if(!is.null(inv)) {
            print("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

example <-function(){
        #' Defines example code for cacheSolve
        print("Starting")
        m = makeCacheMatrix(matrix(1:4, 2))
        print("First call")
        cacheSolve(m)
        print("Second call")
        cacheSolve(m)

        print("Resetting the matrix")
        print("First call")
        m$set(matrix(5:8, 2))
        cacheSolve(m)
        print("Second call")
        cacheSolve(m)
        print("Completed")
}

# Uncomment below to run the example
# example()
