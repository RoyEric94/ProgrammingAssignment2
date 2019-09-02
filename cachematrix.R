## The first function declares a new object that caches the inverse of the matrix it received as a parameter.

## Second function checks to see if the inverse of the matrix was already determined and cached,
## then computes it (if not cached) and returns the inverse.


## Creates a new object from a matrix object (x). 
## Allows caching of the inverse of the initial matrix (x).
## Contains setter and getter for the matrix itself.
## Also contains setter and getter for the inverse of the initial matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the matrix x.
## Checks to see if the inverse has already been computed. 
## If it has, it simply returns the inverse.
## If it has not, the matrix is fetched and the inverse is calculated using solve.
## Then caches the inverse using the setter and returns the inverse.

cacheSolve <- function(x, ...) {
    i<- x$getinverse()
    
    if(!is.null(i)){
        message("Fetching cached inverse")
        return(i)
    }
    basematrix <- x$get()
    i <- solve(basematrix)
    x$setinverse(i)
    i
}
