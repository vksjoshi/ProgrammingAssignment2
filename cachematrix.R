"
Functions in this script inverses a square matrix assuming that the matrix
supplied is always invertible. Computing the inverse of a square matrix can be
done with the solve function in R.
"


"
makeCacheMatrix creates a special matrix object that caches its inverse.
It provides functions to get and set matrix whose inverse needs to be computed.
It also provides functions to set and get the inverse of the matrix.
Actual matrix inversion is done by cacheSolve function.
"
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

        ##Returns the martix
        get <- function() x

        ##Sets new matrix and would flush the already stored matrix inverse
        set <- function(mat) {
                x <<- mat
                inverse <<- NULL
        }

        ##Sets the inverse of the matrix
        setInverse <- function(inv) inverse <<- inv

        ##Returns the inverse of the matrix
        getInverse <- function() inverse

        list(get=get,set=set,
             setInverse=setInverse,getInverse=getInverse)
}

"
cacheSolve computes the inverse of the special matrix returned by
makeCacheMatrix above. If the inverse has already been calculated return the
same inverse value. But if the inverse has not been calculated earlier(i.e is
null), then it calculated the inverse using solve function, sets the inverse in
the special matrix object and returns the inverse.
"

cacheSolve <- function(x, ...) {
        inverse = x$getInverse()
        if(!is.null(inverse)){
                message("from cache")
                return (inverse)
        }
        inverse = solve(x$get())
        x$setInverse(inverse)
        inverse
}
