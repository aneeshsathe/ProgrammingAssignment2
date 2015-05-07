## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(, ncol = 2, nrow = 2)) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinv <- function(inv_mat) m <<- inv_mat
        getinv <- function() m        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        
        data <- x$get()
        out_inv=solve(data,...)
        x$setinv(out_inv)
        out_inv
}
