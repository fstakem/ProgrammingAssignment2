## This program creates a special data structures for square invertable matrices
## so that the inverse of a matrix can be cache to speed up its retreival

## This function stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) 
{
    data <- x
    inverse <- NULL
    
    get_data <- function()
    {
        return(data)
    }
    
    set_data <- function(d)
    {
        data <<- d
        inverse <<- NULL
    }
    
    get_inverse <- function()
    {
        return(inverse)
    }
    
    set_inverse <- function(i)
    {
        inverse <<- i
    }
    
    list(get_data=get_data, set_data=set_data,
         get_inverse=get_inverse, set_inverse=set_inverse)
}


## This function retreives the inverse of a matrix either from its cache or it calculates it and stores it in the object
cacheSolve <- function(x, ...) 
{
    inverse <- x$get_inverse()
    
    if(!is.null(inverse))
    {
        message('Getting inverse from cache')
        return(inverse)
    }
    
    m <- x$get_data()
    inverse = solve(m, ...)
    x$set_inverse(inverse)
    
    return(inverse)
}
