## The two functions below work together to store an inverted matrix in a cache and to fetch the data back when required

## To make use of the functions you can run:-
## 
## source("cachematrix.R")    
## x <- makeCacheMatrix()     
## x$set_matrix(matrix(rnorm(100),10,10))   
## cacheSolve(x)  - this 1st run returns an inverted matrix from working environment
##
## cacheSolve(x)  - this 2nd and further runs returns and inverted matrix from the cache along with a print statement

## makeCacheMatrix is a function which creates 4 smaller functions which can be used to store and fetch data from a cache

makeCacheMatrix <- function(x = matrix()) {
        
        cache <- NULL 
        ##sets cache to NULL initially
        
        set_matrix <- function(y) {
                x <<- y
                cache <<- NULL
        }
        ##recreates the matrix in the fuction environment
        
        get <- function() x
        ##function to get the matrix values
        
        store_inverted <- function(solve) cache <<- solve
        ##function to store the inverted values in the cache
        
        get_inverted_from_cache <- function() cache
        ##function to return values from the cache
        
        list(set_matrix = set_matrix, get = get, store_inverted = store_inverted, get_inverted_from_cache = get_inverted_from_cache)
        ##return functions to environment outside of the makeCacheMatrix functions
        
}


## cacheSolve is a function which goes on to make use of the 4 functions created within makeCacheMatrix

cacheSolve <- function(x, ...) { 
        
        cache<- x$get_inverted_from_cache()
        ##populates cache if any data is available
        
        if(!is.null(cache)) { 
                message("data fetched from cache")
                return(cache)
        }
        ##evaluates cache and returns data from it if cache is not NULL
        ##also prints a messahe to inform user that data is from cache
        
        else
                data <- x$get()
        cache<- solve(data, ...)
        x$store_inverted(cache)
        
        cache
        ##if there is no data in cache when the cacheSolve function is called then it is stored
        
}