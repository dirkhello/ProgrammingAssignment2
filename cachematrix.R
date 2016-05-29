## cacheSolve will return the inverse of a matrix that has before been submitted to
## makeCacheMatrix
## Use of the function is:
## m<-makeCacheMatrix(testmatrix)
## testinverse<-cacheSolve(m)
## where testmatrix is the matrix to be inversed, and testinverse 
## is the resulting inverse of testmatrix
## if the inverse doesn't exist, it it calculated. If it does exist it is returned 
## an the comment "getting cached data" is displayed

## Write a short comment describing this function
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) create the inverse of the matrix
## 4) get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        ## create a special matrix containing the inverse of a matrix
        ## x: matrix whose inverse needs to be created
        
        minverse <- NULL
        set <- function(y) {
                x <<- y
                minverse <<- NULL
        }
        get <- function() x
        setinverse <- function(x) minverse <<- x
        getinverse <- function() minverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will return the inverse matrix of x either a) by looking up the cached value or 
## b) in case this doesn't exist, by using solve to determine the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix minverse that is the inverse of 'x'
        ## x: matrix whose inverse needs to be returned

        ## 1) retrieve minverse with x$getinverse(), 2) check if minverse already exists
        ## 3) if it does return the cached value
        minverse <- x$getinverse()
        if(!is.null(minverse)) {
                message("getting cached data")
                return(minverse)
        }
        ## since the inverse matrix minverse doesn't exist yet, get minverse and calculate inverse using solve()
        data <- x$get()
        minverse <- solve(data, ...)
        ## cache the inverse
        x$setinverse(minverse)
        minverse
}