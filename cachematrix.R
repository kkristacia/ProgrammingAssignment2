## The 2 functions solve and cache the inverse of an inputted matrix

## Set and cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL               #initialize an empty vector
    set <- function(y) {
        x <<- y             #assigning value of x
        i <<- NULL          #reset i when there is new y
    }
    get <- function() x     #to call x
    setinverse <- function(inver) i <<- inver  #to set inverse i
    getinverse <- function() i                 #to call i
    
    #creating object vector in the form of list
    list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
}


## Calculates the matrix inverse and store it in cache
## Retrieves the inverse if there is already an existing one in cache

cacheSolve <- function(x, ...) {
    i <- x$getinverse()    #define i
    if (!is.null(i)) {     #return existing i if it is not null
            message("retrieving inverse from cache")
            return(i)
        }
        data <- x$get() 
        i <- solve(data, ...)  #solving for value of i
        x$setinverse(i)        #setting i
        i
}

