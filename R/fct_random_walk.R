##' Random walk of the matrix to fill
##' 
##' @description Returns the user dataframe updated after choosing randomly a well on the 
##' plate (matrix) and randomly choosing a sample ID that satisfies all the 
##' constraints.
##' @param m is a matrix corresponding to the plate to be filled.
##' @param toVisit contains the wells in form "A1", and contains only the 
##' wells authorized to be filled in
##' @param d is the dataframe containing the data supplied by the user.
##' @param constraint character string corresponding to the spatial constraint 
##' selected by the user
##' @return a dataframe corresponding to the user-supplied data. This dataframe 
##' is an updated version, where the columns `Row` and `Column` are filled with 
##' the coordinates of the chosen well. If no solution is found for the current
##' selected well, then this function returns 1.
randomWalk <- function(m, toVisit, d, constraint){
    
    if (!methods::is(m, "matrix")) {
        logging::logerror("m is not a matrix, m: %s", class(m))
        stop("Need m to be a matrix")
    }
    visited <- c()
    ret = m
    ## samples already picked up and placed
    placed = c()
    
    while (length(visited) != nrow(d)) {
        cell <- resample(toVisit, size = 1)
        visited <- c(visited, cell)
        LETTERS702 <- c(LETTERS, vapply(LETTERS,
                                        FUN.VALUE = as.character(seq_len(26)),
                                        function(x) paste0(x, LETTERS)))
        
        i <- as.numeric(match(toupper(stringr::str_extract(cell, "[aA-zZ]+")),
                              LETTERS702))
        j <- as.numeric(stringr::str_extract(cell, "[0-9]+"))
        ## ensure plate uniformity by solving the constraints for the current 
        ## well
        test <- solveCell(m = ret,
                          d = d,
                          i = i,
                          j = j,
                          already_drawn = placed,
                          constraint = constraint)
        if (methods::is(test, "numeric")) {
            return(1)
        }else{
            ret <- test$m
            placed <- test$already_drawn
            ## we look after the last placed element
            d[which(d$ID == placed[length(placed)]),]$Row <- i
            d[which(d$ID == placed[length(placed)]),]$Column <- j
            toVisit <- toVisit[!toVisit == cell]
        }
    }
    return(d)
}
