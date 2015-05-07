## cacheSolve() calculates the inverse of the input matrix. If the inverse already exists
## then it is retrieved from the cached. matrix.
##makeCacheMatrix creates a vector of functions for an input matrix which
## cacheSolve() uses to retrieve an already calculated inverse


## Creates vector to store retrieve and get input matrices and their inverses

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


## Returns the inverse of input matrix. If the inverse already exists in cache then 
## that value is returned.

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
