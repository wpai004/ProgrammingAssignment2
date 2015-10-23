## The purpose of these functions are to provide the ability to store and/or retrieve a stored matrix
## along with calculating and caching its inverse while being able to retrieve this inverse figure without
## having to re-do the initial inverse computation

## The makeCacheMatrix function allows a user to store and get a matrix along with its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of a matrix but can also retrieve the inverse if it is already
## cached

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        
        if(!is.null(i)) {
            message ("getting cached data")
            return(i)
        }
        
        data <- x$get()
        print(data)
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
