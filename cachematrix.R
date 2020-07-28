## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y)  	                        ## set the value of the matrix
    {
        x <-- y
        inv <<- NULL
    }
    
    get <- function() x		                        ## get the value of the matrix
    setInverse	<- function(solve) inv <<- solve    ## set the value of the inverse
    getInverse	<- function() inv	                ## get the value of the inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse ) ## Return special matrix object
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()                                 ## Get the special objec  matrix
    inv <- solve(data)                              ## Calculate the inverse matrix
    x$setInverse(inv)                               ## set the inverse matrix
    inv                                             ## Return a matrix that is the inverse of 'x' 
}
