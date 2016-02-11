## Put comments here that give an overall description of what your
## function makeCacheMatrix contains two variables: x and m
## x is the input matrix
## m is the inverse of x

makeCacheMatrix <- function(x = matrix()) {
        # m is the inverse of matrix x, which is null at first
        m <- NULL
        
        ## assign value of y to global variable x
        ## assign NULL to m, which is the inverse of input matrix x
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        ## return x
        get <- function() x
        
        ## assign inverted matrix to m
        setinverse <- function(solve) m <<- solve
        
        ## return inverted matrix
        getinverse <- function() m
        
        
        ## function makeCacheMatrix returns a list, in which exist four elements
        ## each element is a function predefined
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## input of cacheSolve is a makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## return the intverse of x
        m <- x$getinverse()
        
        ## if m is not null, means the inverse of m was calculated before
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## if m is null, means this is the first time calculating inverse of matrix x
        data <- x$get()
        
        ## assign inverse of x to m
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        ## return m
        m
}
        
