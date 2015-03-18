## These functions allow for the caching of matrix inversion to help speed things up
## The first function does the work and returns a list of the functions

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x<<-y
            m<<- NULL
      }
      get <- function() x
      setInv <- function (inv) m <<- solve
      getInv <- function() m
      list(set =set, get = get, setInv = setInv, getInv = getInv )
}



## this function will check if the answer is already cached.
## if not it will then call to have the inverse calculated. 
## if so it will sue the cached version.

cacheSolve <- function(x, ...) {
       m <- x$getInv()
       if(!is.null(m)){
             message("getting the chashed data")
             return (m)
       }
       data<- x$get()
       m <- solve(data, ...)
       x$setInv(m)
       m
}
