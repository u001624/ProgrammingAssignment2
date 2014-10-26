## Put comments here that give an overall description of what your
## functions do

# Function makeCacheMatrix creates a cached matrix and a list of functions
# Function cacheSolve find the inverse of the matrix, either if it has been cached

## Write a short comment describing this function
# Function makeCacheMatrix creates a list containing a function to
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse of matrix
#4.  get the value of the inverse of matrix
# these functions maniplulate a cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # creates a variable in the environment of the makeCacheMatrix() function, to store/cache a result
  # why isn't the cached x also need a variable in this environment, so a single function could used instead of forcing the calling program to know when to set a new result?
  set <- function(y) { # function used internally
    x <<- y # store the inputs in x but is x in the environment of the caller or this function ie. makeCacheMatrix parameter, which is a copy of a variable in the callers environment
    m <<- NULL # clear the previous 'cached' result
  }
  get <- function() {x} # this is callable from the object returned from makeCacheMatrix
  setInverseOfMatrix <- function(InverseOfMatrix) {m <<- InverseOfMatrix} # do NOT call setInverseOfMatrix() directly despite it being accessible
  getInverseOfMatrix <- function() {m} # return the 'cached' result
  list(set = set, get = get, # return a list, containing four functions
       setInverseOfMatrix = setInverseOfMatrix,
       getInverseOfMatrix = getInverseOfMatrix)
}


## Write a short comment describing this function
#Function cacheSolve calculates the inverse of matrix, which is initially created with the function makeCacheMatrix. 
#It first checks to see if the inverse of matrix has already been calculated. If so, it `get`s the inverse of matrix from the
#cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix of data and sets the value of the inverse of matrix in the cache via the `setInverseOfMatrix` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseOfMatrix()
  if(!is.null(m)) { # cache contain something, a result, but where is the test that input x matches the non-existent cached/stored inputs?
    message("getting cached data")
    return(m)
  }
  data <- x$get() # store/cache the inputs. Why can't this simply be coded as data <- x ?
    m <- solve(data, ...) # process inputs to produce result
  x$setInverseOfMatrix(m) # store/cache result. Why isn't this simply m <<- m. Lexical Scoping would resolve both sides to the same variable, which isn't what is needed.
  m # return result
}
