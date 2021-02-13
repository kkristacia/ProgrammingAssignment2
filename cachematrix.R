## The 2 functions solve and cache the inverse of an inputted matrix

## Store and cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inver) i <<- inver
    getinverse <- function() i
    list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
}


## Calculates the matrix inverse and store it in cache
## Retrieves the inverse if there is already an existing one in cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()    
    if (!is.null(i)) {
            message("retrieving inverse from cache")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

