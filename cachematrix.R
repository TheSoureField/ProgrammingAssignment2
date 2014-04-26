## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly 
#   
#  we use two functions to achieve the goal:
# (1) makeCacheMatrix(): This function will create a closure environment, which contains
#     the matrix, its cached inverse value, and four functions - get/set the matrix, 
#     getInverse/setInverse for the inverse matrix
#
# (2) cacheSolve(): this function uses solve() to create an inverse, if there is no cached value
#


## makeCacheMatrix function:
#   argument: take a matrix as argument
#   return value: returns a list containing four functions - get the value of the matrix,
#   set the value of the matrix, get the value of the inverse matrix, and set the value of 
#   the inverse matrix


makeCacheMatrix <- function(x1 = matrix()) {
    v <- NULL
    x <- x1
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) v <<- inv
    getInverse <- function() v
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function:
#   arguments: take a matrix as an argument and assume that the matrix supplied is always invertible
#              only process the first argument and pass the rest of the args to solve().
#   return value: Returns a matrix that is the inverse of 'x' (the argument)
#
#  This function computes the inverse of the special "matrix"  returned by  makeCacheMatrix()
#  If the inverse has already been calculated and cached, then  cacheSolve  should retrieve the inverse from the cache.
#  otherwise, computing the inverse with the  solve  function in R

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
