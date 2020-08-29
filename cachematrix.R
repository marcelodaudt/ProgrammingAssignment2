## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss
## here). Your assignment is to write a pair of functions that cache the
## inverse of a matrix.


## The "makeCacheMatrix" function creates a special "matrix" object that can
## cache its inverse, containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	set <- function(i) {
		x <<- i
                i <<- NULL
        }
        get <- function() x
        setinversa <- function(inversa) i <<- inversa
        getinversa <- function() i
        list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)

}


## The "cacheSolve" function computes the inverse of the special "matrix"
## returned by "makeCachaMatrix" function above. If the inverse has already
## been calculated (and the matrix has not changed), then the "cacheSolve" 
## function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        i <- x$getinversa()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinversa(i)
        i

}
