## makeCacheMatric 
## function creates a special "matrix" object that can cache its inverse
## If the inverse exists in the cache it is returned else inverse is computed and returned
## args matrix x
## functions: getInverse, setInverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # Inverse of matrix stored in cache
    im <- NULL
    # Set function which stores the matrix values in matrix y
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    # Get function which returns the value of the matrix
    get <- function() x
    # SetInverse function which assigns the inverse of matrix to cache
    setInverse <- function(inverse) im <<- inverse 
    # GetInverse function which retrieves the inverse of matrix from the cache
    getInverse <- function() im
    # Assign the getter / setter functions to simple names via a list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computing the inverse of a square matrix
## If the value is avaiable in the cache same is returned else
## new inverse is calculated, stored in cache and value is returned
## args matrix x

cacheSolve <- function(x, ...) {
    ## Look for inverse in cache
    im <- x$getInverse()
    if(!is.null(im)) {
      message("getting cached inverse matrix")
      ## Return a matrix that is the inverse of 'x' from cache
      return(im)
    }
    ## Inverse matrix not found in cache, Calculate inverse
    data <- x$get()
    im <- solve(data, ...)
    x$setInverse(im)
    ## Return a matrix that is the inverse of 'x'
    im
}
