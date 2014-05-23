## The functions in this file invert a matrix. Since matrix inversion is expensive, we cache the 
## matrix if we've already solved it and retrieve it from the cache.
## 
## To use: 
## 1. Create a matrix - m = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## 2. Verify your matrix - m$get()
## 3. Retrieve the inverted matrix - cacheSolve(m) 
## 4. Verify the inverted matrix - m$getinverse() 
## 5. Verify that cacheSolve now returns inverted matrix from the cache - cacheSolve(m)

## This function is a series of subfunctions that make it possible to cache the
## result of the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function inverts a matrix. It will retrieve it from cache if it exists there.
## Otherwise, it inverts it and saves it to cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()

    message("calculating inverse")
    inverse <- solve(data, ...)
  
    message("saving inverse to cache")
    x$setinverse(inverse)
    inverse
}
