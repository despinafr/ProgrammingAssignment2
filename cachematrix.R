## This function creates a special "matrix" that can cache its inverse.
## It uses: set, get,setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #initialization of inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x #fuction to get matrix x
    setinverse <- function(inverse) inv <- inverse
    getinverse <- function () inv
    list(set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function computes the inverse of the makeCacheMatrix

cacheSolve <- function(x, ...) { 
    inv <- x$getinverse()
    if(!is.null(inv)) { #checking if inverse is NULL
        message("getting cached data")
        return(inv) #returning inverse value
    }
    data <- x$get()
    inv <- solve(data, ...) #calculating inverse value
    x$setinverse(inv)
    inv
}
