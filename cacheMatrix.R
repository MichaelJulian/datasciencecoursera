## This is CacheMatrix via Coursera's R Programming class.


makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    
    # Function get_matrix allows x to be retrieved via the parent scopes.
    # We're able to do this because of lexical scoping. 
    get_matrix <- function() x
    
    # When we call this function, we set the inverse matrix.
    set_solved <- function(mean) m <<- mean
    
    # The get_solved function will RETURN the inverse matrix, but return NULL if one does not exist.
    get_solved <- function() m
    
    # list will return the list of functions available
    # and the normal matrix 
    list(get_matrix = get_matrix,
         set_solved = set_solved,
         get_solved = get_solved)
    
}


## Cache Solve function to return the inverse matrix. 
cacheSolve <- function(x) {
    
    ## this function will return the inverse solved matrix.
    m <- x$get_solved()
    
    if(!is.null(m)) 
    {
        message("retrieving cache") # friendly message 
        return(m)
    }
    
    
    data <- x$get_matrix()
    
    inverse_matrix <- solve(data)
    x$set_solved(inverse_matrix)
    
    # last line of the code is the returned object.
    inverse_matrix
}
