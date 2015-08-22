# The functions below are used to find and cache the inverse
# of a special matrix. This saves computation if the matrix
# for which an inverse needs to be found hasn't changed. 

# MakeCacheMatrix is used to create a special matrix and 
# store the value of it's inverse for caching. Further 
# descriptions are provdied for each object within this 
# function

makeCacheMatrix <- function(x = matrix()) {
        #initialised value of the inverse
        i <- NULL
        
        # set is used to reset the value of
        # x and i without having to re-run
        # makeCacheMatrix. Using set can be 
        # computationally prudent especially
        # when there are alot of objects to 
        # run
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # get is used to return the matrix 
        # stored in makeCacheMatrix 
        get <- function() x
        
        
        # setinverse is used to store the 
        # inverse as i in it's parent 
        # environment. getinverse is 
        # used to return i 
        
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        # list is used to store the above 
        # 4 functions so that when we assign
        # makeCacheMatrix to an object, the
        # object has all 4 fucntions
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The cacheSolve fucntion is used to first verify the value 
# of the existing inverse. Once verified, it returns the 
# cached inverse if the matrix hasn't changed or calculates
# the inverse if the matrix changes (i.e i is Null)

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # an else statement which gets the 
        # new matrix and calculates the inverse
        data <- x$get()
        i <- solve(data, ...)
        
        # The calculated inverse is stored in
        # makeCacheMatrix for caching
        x$setinverse(i)
        
        # inserve of the matrix is returned
        i
}

# The input of cacheSolve is the object where makeCacheMatrix
# is stored
b <- makeCacheMatrix(matrix(c(2, 4, 3, 7), nrow = 2, 
                            ncol = 2, byrow = TRUE))
cacheSolve(b)


