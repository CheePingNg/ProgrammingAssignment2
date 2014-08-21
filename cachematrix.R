## Put comments here that give an overall description of what your
## functions do

## The pair of functions here, makeCacheMatrix and cachematrix,
## cache the inverse of a matrix.
## Matrix inversion is usually a costly computation. Thus, it may be
## beneficial to caching the inverse of a matrix rather than
## computing it repeatedly.  

## Write a short comment describing this function
## makeCacheMatrix
## This function creates a special "matrix" object that can cache
## its inverse.  It is a list containing a function to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse matrix (setInvMatrix)
## 4. get the value of the inverse matrix (getInvMatrix) 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInvMatrix <- function(solve) m <<- solve
    getInvMatrix <- function() m
    ## following names attributes of the list and assigns to objects on the right side 
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
         
}

## Write a short comment describing this function
## cacheSolve
## This function computes the inverse of the special 
## "matrix" created by makeCacheMatrix above. It first checks to see
## if the inverse of matrix have been calculated.  If the inverse
## has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache and
## skips the computation.  Otherwise, it calculates the inverse and 
## sets the value of the inverse in the cache using the setInvMatrix
## function.

cacheSolve <- function(x, ...) {
        
    m <- x$getInvMatrix()
    ## following checks to see if inverse has been calculated
    ## if yes, retrieves the inverse from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## following gets the matrix value from x 
    data <- x$get()
    ## Returns a matrix that is the inverse of 'x' using solve
    m <- solve(data, ...)
    ## Sets the value of the inverse in the cache
    x$setInvMatrix(m)
    m
    
}


