## Put comments here that give an overall description of what your
## functions do


##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		#Creation a function that cache initializes 'x' and 'm'
		#so any time makeCacheMatrix is called those variables get initialized
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		#function that returns the given matrix 'x'
        get <- function() x
		#function that sets the cached variable 'm'
        setSolve <- function(inv) m <<- inv
		#function that retrieves the cached variable 'm'
        getSolve <- function() m
		#list of the above 4 functions to return
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## The first parameter 'x' is a list of the functions set, get, setSolve, getSolve
		m <- x$getSolve() 	#returns the cached variable 'm'
        if(!is.null(m)) { 	#checks if 'm' is not null. If this it is true, notify with the message "getting cached data" and return m
                message("getting cached data")
                return(m)
        }
		## if otherwise m is null
        data <- x$get() #retrieving the cached variable 'x' and setting it to 'data'
		
		##solve() is a R function that returns the inverse of a square matrix
		##We don't check to see if the supplied matrix is invertible. We assume that the matrix supplied is always invertible
        m <- solve(data, ...) # computing the inverse of data and setting it to 'm'
        x$setSolve(m) # caching the 'm' variable
        m #returns 'm' the inverse of 'x'
}
