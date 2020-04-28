
## Function for spatial contraints: the North, East, West and South neighbors
## for the visited box
findNEWSneighbors <- function(m, i, j){
    N <- tryCatch(
        {
            m[i - 1,j]
        },
        error = function(err1){
            return(NA)
        }
    )
    E <- tryCatch(
        {
            m[i,j + 1]
        },
        error = function(err2){
            return(NA)
        }
    )
    W <- tryCatch(
        {
            m[i,j - 1]
        },
        error = function(err3){
            return(NA)
        }
    )
    S <- tryCatch(
        {
            m[i + 1,j]
        },
        error = function(err4){
            return(NA)
        }
    )
    return(c(N,E,W,S))
}

## Function for spatial constraint that only looks for North and South
findNSneighbors <- function(m, i, j){

    N <- tryCatch(
        {
            m[i - 1,j]
        },
        error = function(err1){
            return(NA)
        }
    )

    S <- tryCatch(
        {
            m[i + 1,j]
        },
        error = function(err4){
            return(NA)
        }
    )
    return(c(N,S))
}

## Function for spatial constraint that only looks for West and East
findWEneighbors <- function(m, i, j){
    # for right neighboor
    E <- tryCatch(
        {
            m[i,j + 1]
        },
        error = function(err2){
            return(NA)
        }
    )

    # For left neighboor
    W <- tryCatch(
        {
            m[i,j - 1]
        },
        error = function(err3){
            return(NA)
        }
    )
    return(c(W,E))
}

checkConstraints <- function(m, row, col, mode){

    if (mode == "NS") {
        neighbors <- findNSneighbors(m,row,col)
    }else if (mode == "WE") {
        neighbors <- findWEneighbors(m, row, col)
    }else if (mode == "NEWS") {
        neighbors <- findNEWSneighbors(m, row, col)
    }else{
        logging::logerror("The mode provided is not correct")
    }
    return(neighbors)
}
