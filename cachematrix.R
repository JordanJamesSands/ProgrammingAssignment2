## Put comments here that give an overall description of what your
## functions do
##The inverse and the matrix itself are stored in 'x' and 'inv' variables outside this environment


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    get <- function() x
    set <- function(t) {
        #rassign x
        x <<- t
        #x has been reassigned, so we need to forget the previous inverse
        inv <<- NULL
    }
    setInv <- function(t) {
        inv <<- t
    }
    getInv <- function() inv
    
    list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseValue <- x$getInv()
    
    if(!is.null(inverseValue)) {
        print('Inverse is already cached, returning')
        return(inverseValue)
    } else {
        print("need to compute inverse")
        inverse <- solve(x$get())
        x$setInv(inverse)
        return(inverse)
    }

}
