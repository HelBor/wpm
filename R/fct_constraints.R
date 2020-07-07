##' Find the 4 cardinal neighbors of an element of a matrix
##'
##' @description Function for spatial contraints: the North, East, West and
##' South neighbors of the current element (i,j) of the matrix m.
##'
##' @param m, matrix
##' @param i, integer, line index in the matrix
##' @param j, integer, column index in the matrix
##'
##' @return A vector containing the North, East, West and South neighbors of
##' the element (i,j) of the matrix being processed.
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

##' Find the top and bottom neighbors of an element of a matrix
##'
##' @description Function for spatial constraint that only looks for North (top)
##' and South (bottom) neighbors of the current element (i,j) of the matrix m.
##'
##' @param m, matrix
##' @param i, integer, line index in the matrix
##' @param j, integer, column index in the matrix
##'
##' @return A vector containing the North and South neighbors of the element
##' (i,j) of the matrix being processed.
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

##' Find the left and right neighbors of an element of a matrix
##'
##' @description Function for spatial constraint that only looks for West (left)
##'  and East (right) neighbors of the current element (i,j) of the matrix m.
##'
##' @param m, matrix
##' @param i, integer, line index in the matrix
##' @param j, integer, column index in the matrix
##'
##' @return A vector containing the West and East neighbors of the element
##' (i,j) of the matrix being processed
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

##' Check for spatial constraints
##'
##' @description Finds the neighbors of the current element (row, col) in the
##' matrix m, depending on the chosen constraint pattern. Currently, there are
##' only 3 valid patterns (NS, WE and NEWS)
##'
##' @param m matrix
##' @param row current selected row in the matrix m
##' @param col current selected column in the matrix m
##' @param mode spatial constraint
##'
##' @return A vector containing the neighbors of element (row,col) of the
##' matrix m.
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
