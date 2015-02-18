## Functions to cache the inverse of a matrix (matrix inversion) using the “superassignment” operator, <<-

## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                   ## set placeholder for future value
    set <- function(y) {                        ## define a function that invalidates the cache and set the internals of the vector to new values
        x <<- y                                    ## set the vector x to a new vector y
        m <<- NULL                                 ## reset mean, m, to NULL
    }
    get <- function() x                         ## return the vector x
    setinverse <- function(solve) m <<- solve   ## set m to solve
    getinverse <- function() m                  ## return m -- the inverse matrix
    list(set = set, get = get,                  ## return vector of all functions defined above
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by the makeCacheMatrix above.
    ## If inverse has already been calculated and the matrix has not changed, 
    ## then cachesolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    m <- x$getinverse()         ## get inverse if it's already been calculated
    if(!is.null(m)) {           
        message("getting cached data of inversed matrix")
        return(m)
    }                           
    data <- x$get()             ##  if cache does't exist, get the value of the matrix
    m <- solve(data, ...)        
    x$setinverse(m)             ##  set the value of the inverse
    m
}

## TESTING THE FUNCTIONS
# > test_matrix = matrix( c(1, 2, 3, 4), nrow = 2, ncol = 2)
# > x <- makeCacheMatrix(test_matrix)
# > x$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(x)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(x)
# getting cached data of inversed matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > test_matrix = matrix( c(11, 12, 13, 14), nrow = 2, ncol = 2)
# > x <- makeCacheMatrix(test_matrix)
# > x$get()
# [,1] [,2]
# [1,]   11   13
# [2,]   12   14
# > cacheSolve(x)
# [,1] [,2]
# [1,]   -7  6.5
# [2,]    6 -5.5
