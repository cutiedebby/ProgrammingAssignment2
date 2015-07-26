## It takes a square matrix and cache it.
## If the matrix has already been inversed
## it will return the previous inverseed maxtrix
## Otherwise, it will inverse the matrix, 
## cache it and return the result.

## Set the matrix, get the matrix, set the inverse, get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


##get an inverse
## if the inverse is already computated
## return the previous computated value with a message ("getting cached data")
## otherwise, compute inverse  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinv()
        ## if the inverse has already been calcaultated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## or retrieve inverse from the cache and skips the computation
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
