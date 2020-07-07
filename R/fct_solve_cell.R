##' Affects a sample to the chosen cell in the plate
##'
##' This function chooses a sample randomly from among those who respect the
##' neighborhood constraints and who have not yet been assigned to a well.
##'
##' @param m Matrix representing the plate plan.
##' @param d Dataframe containing the samples to place.
##' @param i Line index of the chosen well.
##' @param j Column index of the chosen well.
##' @param already_drawn Vector of samples already affected to wells.
##' @param constraint Character. Corresponds to the neighborhood constraint mode.
##' @return If there is no possibility to find a valid sample, the function
##' returns an error value (1).
##' If a sample is chosen, then this function returns two objects:
##' * __m__ The matrix updated with the new added sample.
##' * __already_drawn__ The vector of already placed samples updated.
solveCell <- function(m, d, i, j, already_drawn, constraint){

    if (!methods::is(m,"matrix")) {
        logging::logerror("m is not a matrix, m: %s", class(m))
        warning("Need m to be a matrix")
    }
    if (!methods::is(d, "data.frame")) {
        logging::logerror("d is not a dataframe, d: %s", class(d))
        warning("Need d to be a dataframe")
    }

    ## we look at which individuals are neighbors of the current box
    if (constraint != "none") {
        neighbors <- checkConstraints(m, row = i, col = j, mode = constraint)
    }else{
        neighbors <- c(NA,NA,NA,NA)
    }

    ## identify which group the neighbors belong to in order to obtain a reduced
    ## list of possibilities of groups for the current cell to fill
    forbidden_groups <- unique(d$Group[which(d$ID %in% neighbors)])
    possible_groups <- levels(d$Group)[which(!levels(d$Group) %in% forbidden_groups)]


    if (length(possible_groups) == 0) {
        ## there are no more possibilities
        return(1)
    }else{
        ## only take in individuals belonging to the possible groups
        ## and who are not in already_drawn
        possible_ind <- d$ID[which(d$Group %in% possible_groups)]
        available_ind <- d$ID[which(d$ID %in% possible_ind & !(d$ID %in% already_drawn))]

        if (length(available_ind) == 0) {
            ## there are no more possibilities
            return(1)
        }else{
            ## use resample because this function also works as expected when there is
            ## only one element in the set to be sampled.
            chosen_ind <- resample(available_ind, size = 1)
            m[i, j] <- chosen_ind
            already_drawn <- c(already_drawn, chosen_ind)
        }
    }

    return(list("m" = m, "already_drawn" = already_drawn))
}
