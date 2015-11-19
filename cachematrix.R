makeCacheMatrix <- function(as.matrix(x)) {
        inverse <- NULL           # initialize the inverse matrix
        set <- function(y) {      # changes the input matrix stored in makeCacheMatrix. Use only if changes are needed
                x <<- y                   # substitute input matrix x with y in makeCacheMatrix because using <<-
                inverse <<- NULL       # restores null since old inverse not needed 		
        }
        get <- function() x       # returns the matrix x from makeCacheMatrix
        setinv <- function(temp) inverse <<- temp   # store value of input in a variable temp in MakeCacheMatrixr
        getinv <- function() inverse                              # return value of inverse in MakeCacheVector
        list(set = set, get = get,          # stores four functions in makeCacheMatrix
             setinv = setinv,
             getinv = getinv)

}
cacheSolve <- function(x, ...) {    # cacheSolve input is object where makeCacheMatrix is stored
        inv <- x$getinv()               
        if(!is.null(m)) {                        # if inverse exists in memory, return a message and the value of inverse
                message("getting cached inverse")
                return(inverse                 
        }           
        data <- x$get()                  # retrieves vector stored in makeCacheMatrix  
        inverse <- Solve(data)     # calculate inverse matrix
        x$setinv(inverse)             # stores the inverse in the object assigned with makeCacheMatrix
        inv
}

