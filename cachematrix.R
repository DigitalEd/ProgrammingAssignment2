## makeCacheMatrix and cacheSolve functions work together to cache a matrix inversion result.
## Usage:
## m = <some matrix>
## cm = makeCacheMatrix(m)
## inverse = cacheSolve(cm)


## function makeCacheMatrix
##  Make a Inverse caching matrix
##
## Args:
##  theMatrix - The matrix to cache the inverse of
##
## Returns:
##  A list of functions:
##      setMatrix - sets (saves) the matrix
##      getMtrix - get the original matrix
##      setInverse - sets (saves) the inverse
##      getInverse - gets the saved inverse or null if nothing saved
##
makeCacheMatrix <- function(theMatrix = matrix()) 
{
    cachedInverse = NULL
    # The set function takes a new matrix and resets/clears the saved cachedInverse
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



## function cacheSolve
##  solve and cache the inversion of the given matrix
##
##  Args:
##      x   - the matrix to solve
##      ... - other parameters to the standard solve() function
##
##  Returns:
##      the inverse matrix, if possible.
##
cacheSolve <- function(x, ...) 
{
    result = x$getInverse()
    if(!is.null(result)) # if the result has been cached don't compute it again
    {
        message("getting cached data")
        return(result)
    }
    # get the matrix
    data = x$getMatrix()
    # solve this inverse (if possible)
    result <- solve(data, ...)
    # cache the result
    x$setInverse(result)
    
    return (result) # return the result (explicitly)
}
