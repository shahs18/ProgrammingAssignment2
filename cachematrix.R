
## creates a matrix that can cache teh inverse
makeCacheMatrix <- function(x = matrix()) {
        ## x is a square invertible matrix
           a <- NULL
           set <- function(y) {
        ## '<<' defines varibales in an environment diffrent from current      
                x <<- y
                a <<- NULL
        }
        get <- function() x
        setmat <- function(inverse) a <<- inverse 
        getmat <- function() a
        list(set=set, get=get, setmat=setmat, getmat=getmat)
}

## computes the inverse of matrix from makeCacheMatrix(). if the inverse is already calculated retrieves it from cache.
cacheSolve <- function(x, ...) {
        ## x is output of makeCacheMatrix()
        
        a <- x$getmat()
        
        # if the inverse has already been calculated
        if (!is.null(a)){
                # get it from the cache and skip computation. 
                message("getting cached data")
                return(a)
        }
        
        # otherwise, calculate inverse 
        temp <- x$get()
        a <- solve(temp, ...)
        
        x$setmat(a)
        
        return(a)
}
