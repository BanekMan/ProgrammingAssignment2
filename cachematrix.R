funkcja1 <- function( m = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    get <- function() {
        m
    }
    funkcjaDoInversji <- function(inverse) {
        i <<- inverse
    }
    funkcjaDoPobieraniaInversji <- function() {
        i
    }
    
    list(set = set, get = get,
         funkcjaDoInversji = funkcjaDoInversji,
         funkcjaDoPobieraniaInversji = funkcjaDoPobieraniaInversji)
}

funkcja2 <- function(x, ...) {
    
    m <- x$funkcjaDoPobieraniaInversji()
    
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}