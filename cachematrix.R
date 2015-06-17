## These two functions work together to create a matrix
## object and caches its inverse inside of that matrix
## object. They then check if the inverse of the matrix
## has been calculated and pulls the inverse of the matrix
## from cache if it has, otherwise it calculates the inverse
## and stores it in the matrix object.

## Creates a matrix object to store the input matrix
## and its inverse into.

makeCacheMatrix <- function(x = matrix()) {

    ## Sets the i variable to NULL on creation
    ## of the matrix object.

    i <- NULL

    ## Saves the list of the functions defined
    ## within the makeCacheMatrix function into
    ## the matrix variable which was originally
    ## passed as an argument to the makeCacheMatrix
    ## function, replacing its contents

    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## Returns the value of "x" when called

    get <- function() x

    ## Saves the inverse of the matrix input
    ## into makeCacheMatrix as "i"

    setinverse <- function(solve) i <<- solve

    ## Returns the value of "i" when called

    getinverse <- function() i

    ## Returns the functions defined in makeCacheMatrix
    ## as a list
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Calculates the inverse of the matrix object created
## in the makeCacheMatrix function or pulls it from cache
## if it has already been calculated.

cacheSolve <- function(x, ...) {

    ## Attempt to pull the already-calculated inverse
    ## of the matrix from cache into "i"

    i <- x$getinverse()

    ## Check to see if anything has been stored in "i"
    ## to determine if the inverse has already been cached

    if(!is.null(i)) {
        
        ## Print a message to the screen indicating that
        ## the inverse has already been calculated and
        ## stored to cache and the cache is being retrieved
    
        message("getting cached data")

        ## Return the cached value of the inverse

        return(i)
    }

    ## Stores the value of the matrix "x" in a variable "data"
   
    data <- x$get()
    

    ## Calculates the inverse of the matrix stored in "data"
    ## and saves it to variable "i"

    i <- solve(data, ...)

    ## Saves the variable "i" into the cache of the 
    ## "x" matrix
    
    x$setinverse(i)

    ## Returns the value of the inverse of the matrix

    i
}
