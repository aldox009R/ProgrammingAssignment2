## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## In the makeCacheMatrix () function the methods of the matrix special X are initialized.
## Defines the setInverse () function that stores the value of the inverse of the matrix X 
##  for future calls of the cacheSolve () function.
## Defines the getInverse () function that returns the inverse of the matrix special X
##  calculated in a previous call
##

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y)  	                        ## set the value of the matrix
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x		                        ## get the value of the matrix
    setInverse	<- function(solve) inv <<- solve    ## set the value of the inverse solve () is a function built into R that allows obtaining the inverse of a matrix
    getInverse	<- function() inv	                ## get the value of the inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse ) 
}


## Write a short comment describing this function
##
## When the cacheSolve () function is called, a call is made to the 
## function getInverse () of the object x, when this function returns 
## true it means that the inverse of the object x has already been calculated.
## If x$getInverse () returns false we proceed to calculate the inverse of the matrix x

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
