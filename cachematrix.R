## Put comments here that give an overall description of what your
## functions do
##The inverse and the matrix itself are stored in 'x' and 'inv' respectively. These are variables defined inside the makeCacheMatrix environment


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


#If the inverse is stored in 'inv' defined in the makeCacheMatrix environment, 
#then return it. Otherwise compute and save it to 'inv' inside makeCacheMatrix
#environment

cacheSolve <- function(x, ...) {
    # Retrieve the saved 'inv' value
    inverseValue <- x$getInv()
    
    if(!is.null(inverseValue)) {
        #the inverse has already been computed, return it
        print('Inverse is already cached, returning')
        return(inverseValue)
    } else {
        #the inverse is yet to be computed, compute, save and return it
        print("need to compute inverse")
        inverse <- solve(x$get())
        x$setInv(inverse)
        return(inverse)
    }

}
