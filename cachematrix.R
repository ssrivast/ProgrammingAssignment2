## Function makeCacheMatrix stores tbe inverse of matrix in global variable


## makeCacheMatrix stores the inverser in a global variable and creates vector with specific functions as list

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) mi <<- inverse
        getInv <- function() mi
        list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}


## cacheSolve checks if the inverser matrix already exists, if found returns the change matrix else calculates the invers

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getInv()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data)
        x$setInv(mi)
        mi
}

