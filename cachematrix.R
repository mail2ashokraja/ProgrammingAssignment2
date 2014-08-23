## The following functions performs the 'Inverse' of given matrix,
## it retrieves the 'Inverse' of the matrix from the cache, if the given matrix has not changed, 


## The following function takes the matrix as input and 
## returns the list of useful functions (viz. setMatrix, getMatrix, setInverse, getInverse)

makeCacheMatrix <- function(x = matrix()) {
        
	invMatrix <- NULL	## erases the contents of invMatrix (if any) and sets it to NULL
        
	setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
	getMatrix <- function() x
        setInverse <- function(solve) invMatrix <<- solve
        getInverse <- function() invMatrix
        
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## This functions takes the given matrix as input and
## returns it's Inverse from cache, if matrix has not changed
## otherwise, it returns by calculating its inverse, also sets it to cache

cacheSolve <- function(x, ...) {

        invMatrix <- x$getInverse()		## checks whether Inverse of x exits, if so, pass it to invMatrix

        if(!is.null(invMatrix)) {		## checks if invMatrix is NULL,
                message("getting cached data")	## if not, fetches the Inverse of Matrix from cache
                return(invMatrix)		##
        }

        datMatrix <- x$getMatrix()		## if a new Matrix is given, pass it to datMatrix
        invMatrix <- solve(datMatrix, ...)	## calculates its inverse and store in invMatrix
        x$setInverse(invMatrix)			
	invMatrix			        ## Return a matrix that is the inverse of 'x'
}
