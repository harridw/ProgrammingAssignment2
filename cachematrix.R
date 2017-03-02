## Create matrix object to cache inverse matrix
## If matrix provided is not square (i.e. nrow == ncol) or otherwise non-invertible, cacheSolve will fail
## Use test matrices recommended by Alan E Berger for assignment
## m1 <- matrix(c(1/2,-1/4,-1,3/4), nrow=2, ncol=2)  --> or other non-singular square matrix would work
## mtrx <- makeMatrix(m1)  --> use matrix 'm1' for makeMatrix() function
## cacheSolve(mtrx) --> use makeMatrix() as input arguments for cacheSolve(mtrx) to compute inverse of matrix

makeMatrix <- function(x = matrix()) {    ## initialize 'x' where class(x) is a matrix
      imtrx  <- NULL   ## initialize as object in makeMatrix() environment
      
      ## NOTE:    '<<-' is form of the assignment operator, which assigns the value on the right side of the operator
      ##          to an object in the parent environment named by the object on the left side of the operator
      
      set  <- function(y){    ## Comment: function that defines objects of matrix in makeMatrix()
            x <<- y           ## Comments: assigns input for 'x' in makeMatrix(), the parent environment
            imtrx <<- NULL    ## Comments:
                                    ## 1 Assign value of 'NULL' to inverse matrix (imtrx) in makeMatrix(), parent environment
                                    ## 2 clear imtrx value that may be stored with prior execution of cacheSolve()
      }
      get  <- function() x     ## Comment: retrieves values for function from makeMatrix() --> result of lexical scoping
      setinverse  <- function(inverse) imtrx  <<- inverse   ## Comments:
                                                                  ## 1 function to set inverse matrix
                                                                  ## 2 assign the input argument to the value of imtrx in parent environment
      
      getinverse  <- function() imtrx     ## Comments:
                                                ## 1 inverse matrix 'imtrx' defined in makeMatrix
                                                ## 2 uses lexical scoping to retreive correct value
      
      list(set= set, get = get,      ## Assigns each of functions defined above as an element of a list and returns to parent environment
           setinverse = setinverse,  ## Gives name to result for each function  
           getinverse = getinverse)  ## Allows use of '$' extract operator to get contents
      
}

## Computes the inverse matrix for the matrix created above. If inverse matrix already stored ('cached') and the matrix has not changed,
## then cacheSolve should retrieve the inverse matrix from cache
cacheSolve <- function(x, ...) {    ## Comments:
                                          ## 1 function to obtain/retrieve inverse matrix
                                          ## 2 Requires input arguments from makeMatrix(), not atomic vectors
      
      imtrx  <- x$getinverse() ## Comments:
                                    ## 1 require input arguments from makeMatrix(), else result in error ['$' does not work with atomic vectors ]
                                    ## 2 retrieves 'cached' result in makeMatrix()
      
      if (!is.null(imtrx)){   ## if 'imtrx' is not NULL = 'TRUE', it returns the 'cached' inverse matrix
            message("getting cached data")
            return(imtrx)    
      }
      data  <- x$get()              ## if !is.null(imtrx) = 'FALSE', cacheSolve() retrieves matrix from makeMatrix() 
      imtrx  <- solve(data, ...)    ## calculates inverse of matrix for data <- x$get
      x$setinverse(imtrx)           ## returns the inverse matrix ('imtrx') to parent environment 
      imtrx                         ## prints the inverse matrix to screen 
}
