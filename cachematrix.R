## The overall goal of functions makeCacheMatrix and cacheSolve is to show the inverse
## of a matrix. When first computed, the inverse is supposed to be cached so that it
## can be reused. From that point on, a new recomputation of the inverse is only
## supposed to be carried out if the original matrix itself changes.


## makeCacheMatrix function outputs a list of 3 elements, associated respectivley with
## reading the cahce, storing new values and printing the original matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  ## This "inverse" variable will ultimately hold the inverse matrix, so it is set to NULL
  ## at the outset, when a new matrix has been provided:
  
  inverse <- NULL 
  
  ## Below is the function that will read the cached inverse and print it:
  
  fetch_cache <- function() inverse
  
  ## Below is the function that will write the inverse to the cache. The value to be
  ## written is passed as an argument to this function after being computed from
  ## the cacheSolve function further down the code (see line 62)
  
  store_inverse <- function(computed_inverse) {
    inverse <<- computed_inverse
    
    ## Superassignment in the line above. This is so that "inverse" takes this value
    ## even outside this function environment.
  }
  
  ## Below is a simple function to get the matrix that was passed as input to the
  ## makeCacheMatrix function. This is necessary so that the computation of the inverse,
  ## when it is not cached, can be executed.
  
  get_matrix <- function() x
  
  ## This creates the list, so that the variables to which the functions have been
  ## assigned can be accessed
  
  list (cache = fetch_cache, store = store_inverse, get = get_matrix)
}


## cacheSolve function will actually test to see if there is anything in cache
## and, if there isn't, do the computation, passing the result to the store function

cacheSolve <- function(x, ...) {
  
  ## Logical test: if inverse is not NULL (because of prior caching), return inverse.
  
  inverse <- x$cache()
  if (!is.null (inverse) == TRUE) {
    message("Data being retrieved from cache")
    return(inverse)
  }
  else
    
    ## if it is NULL, do the computation, pass it to the store function and hand it over
    ## to the store function, so that it can be cached for the future. Also, computed
    ## inverse is returned.
    
    values <- x$get()
  computed_inverse <- solve(values)
  x$store(computed_inverse)
  computed_inverse 
}

## This is how it is supposed to work, but there seems to be an issue with storing
## computed value. Couldn't figure it out yet. Any remarks on the grading phase will
## be highly appreciated.