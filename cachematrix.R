## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL 
    ## set x with y in the main function and inverse(m) is assigned to null
    set<- function(y){
        x<<- y
        m<<-NULL
    }
    # get the value of x stored in main function
    get<- function() x
    
    # set the value of inv to m in the main function
    setInvert<- function(inv) {
        m<<- inv
    }
    # get the value of m in the main function
    getInvert <- function(){
        m
    }
    list(set = set, get = get,
         setInvert = setInvert, 
         getInvert = getInvert)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # check if the invert is already calculated, if yes then return the invert matrix
    # with the specified message
    m<- x$getInvert()
    if(!is.null(m)){
        message("Getting from Cache")
        return(m)
    }
    dataM<- x$get()
    # invert the matrix with solve() and store in m 
    m <- solve(dataM)
    x$setInvert(m)
    return(m)
}
