## This function creates an object that can be used to efficiently invert a matrix

## Write a short comment describing this function

MakeCacheMatrix <- function(x = matrix()) {    ## the function essentially gets a matrix - which we will invert
  i <- NULL                                  ##  creates a new null vector
  set <-function(y) { 
    x <<-y                                     ## here we use <<- to assign a particular value to the object in a temporal environment from the current one
    i<<-NULL                          ## same thing
  }     
  get <-function() x                            
  setinv <-function(inverse) i <<- inverse 
  getinv <-function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv) ## gets with cached files
}
## Write a short comment describing this function

CacheSolve <- function(x, ...) {    ## So this basically returns a matrix that is inverse to the original one
i <- x$getinv()
  if (!is.null(i)){     # checks if the inverse has been done
        print("in cache")
    return(i)}
   matrix.frame = x$get()    # if not it - it calculates the inverse matrix 
  i <- solve(matrix.frame, ...) ## the solve function does it for us   
  x$setinv(i)
  return(i)}             ###  we return the data
 
 
##reference 1: https://guides.github.com/introduction/getting-your-project-on-github/
##reference 2: http://stackoverflow.com/questions/25374803/returning-the-inverse-matrix-from-a-cached-object-in-r-checking-that-input-matri
##reference 3: http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/
