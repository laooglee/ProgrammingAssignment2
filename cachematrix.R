##Short disclaimer - this is my first serious programming experience
## (after some tens of VBA macros :) so i've tried to be as particular
## as possible, explaining each and every step.
## Hope it won't bother reviewers too much :)

##In this source file I prepared two functions for cashing results of matrix 
## inversion (by 'solve' function) and retreiving cached results in case the
## original matrix has not been changed.

### First function - 'makeCacheMatrix'.
## Creats an object to set a matrix and cache results of matrix inversion.
## It consists of a list of subfunctions to do the following:
## 
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inversed matrix
## 4.  get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        ##initialize variable to store (to cache) results and drop any chached result to NULL
        inv <- NULL
        ##set a new matrix for inversion and drop to NULL any chached result of former calculations
        set <- function(y) {      
                x <<- y
                inv <<- NULL
        }
        ## retreive matrix addigned at the previous step
        get <- function() x 
        
        ##assign to caching variable (inv) whatever is passed to the function as argument
        setinverse <- function(inverse) inv <<- inverse 
        ## retreive cached result from the cashing variable
        getinverse <- function() inv
        
        ##set a named list of prepared functions to ease the access to them from the solving function
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function 'cacheSolve' checks if any value is assigned to caching variable 'inv'
## and return this value, otherwise makes the calculation and store the result in 'inv'

cacheSolve <- function(x, ...) {
        ## 1. Step of checking and returning cache. 
        ## Return a matrix that is the inverse of 'x'
        ## initialise local (inside this function) variable 'inv' and assign to it wahtever have been
        ## stored in the caching variable 'inv' from first function
        inv <- x$getinverse()
        ## chech if any value have been asssigned. If yes, return it. If no, go to calculation step (2).
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##2. Calculation step.
        ##Get matrix, stored in the 'x' variable from the first function.
        data <- x$get()
        ##calculate inversion of the matrix and assign it to the 'inv' variable
        inv <- solve(data, ...)
        ##pass calculation results to the caching variable 'inv' in the first function through "<<-" operator within
        ## subfunction 'setinverse' from the first function
        x$setinverse(inv)
        #return results of the calculation
        inv
}

## Example of usage:

## my_mx <- makeCacheMatrix()
## my_mx$set(matrix(1:4,2,2))

## cacheSolve(my_mx)

## > my_mx$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > my_mx$getinverse()
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
