## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(theMatrix = matrix()) 
{
    cachedInverse = NULL
    # The set function takes a new matrix and resets the cachedInverse
    setMatrix = function(y) 
    {
        theMatrix <<- y
        # clear previoused cached results
        cachedInverse <<- NULL
    }
    # Get returns the matrix (yes, the return() and {}'s are unnecessary 
    # but they make the code easier to read)
    getMatrix = function() { return (theMatrix) }
    
    # Set (Save) the inverse matrix
    setInverse = function(inverse) {  cachedInverse <<- inverse }
    
    # Get the inverse Matrix
    getInverse = function() { return (cachedInverse)  }
    
    # setup the function list
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    result = x$getInverse()
    if(!is.null(result)) # if the result has been cached dont' compute it again
    {
        message("getting cached data")
        return(result)
    }
    # get the matrix
    data = x$getMatrix()
    # solve this inverse (if possible)
    result <- solve(data, ...)
    x$setInverse(result)
    return (result) # return the result (explicitly)
}
