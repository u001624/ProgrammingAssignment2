## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a list containing a function to
#1.  set the value of the vector
#2.  get the value of the vector
#3.  set the value of the mean
#4.  get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
#makeCacheMatrix <- function(x = numeric()) {
  m <- NULL # creates a variable in the environment of the makeCacheMatrix() function, to store/cache a result
  set <- function(y) { # function used internally
    x <<- y # store the inputs
    m <<- NULL # clear the previous 'cached' result
  }
  get <- function() {x} # this is callable from the object returned from ?
  setmean <- function(mean) {m <<- mean} # do NOT call setmean() directly despite it being accessible
  getmean <- function() {m} # return the 'cached' result
  list(set = set, get = get, # return a list, containing four functions
       setmean = setmean,
       getmean = getmean)
}


## Write a short comment describing this function
# function calculates the mean of the special "vector"
#created with the above function. However, it first checks to see if the
#mean has already been calculated. If so, it `get`s the mean from the
#cache and skips the computation. Otherwise, it calculates the mean of
#the data and sets the value of the mean in the cache via the `setmean`
#function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() # store/cache the inputs. Why isn't this simply data <- x ?
  m <- mean(data, ...) # process inputs to produce result
  x$setmean(m) # store/cache result. Why isn't this simply m <<- m obviously because parent m can't be referenced this way
  m # return result
}
