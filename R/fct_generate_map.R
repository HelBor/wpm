##' Generate a plate map according to the input parameters
##'
##' @description Returns a dataframe or NUL or 0 obtained by applying the backatracking
##' algorithm on the matrix.
##'
##' @param user_df dataframe containing 9 features: Sample, ID, Group,
##' Sample.name, Well, Status, Row, Column, Plate
##' @param nb_rows numeric, number of lines on the plate
##' @param nb_cols numeric, number of columns on the plate
##' @param df_forbidden dataframe with the same structure than user_df, but for
##' the forbidden, buffer solutions and Not randomized wells.
##' @param mod character, neighborhood spatial constraint
##' @param max_it numeric, maximum number of attempts to generate a plate plan
##' before returning a failure.
##' @param updateProgress shiny object, reports progress to the user.
##' @details A number of attempts is allowed. Consequently, if the maximal
##' number if attempts is reeched and no solution was found with the
##' backtracking (i.e. the randomWalk does not return a dataframe), this
##' function prints a warning message and returns NULL. If a solution is
##' found, then it returns the dataframe.
##' @return Returns a dataframe containing all the data of the plate map(s)
generateMap <- function(user_df, nb_rows, nb_cols, df_forbidden, mod, max_it,
                        updateProgress = NULL){

    nb_attempts = 1
    ret = 1
    LETTERS702 <- c(LETTERS, vapply(LETTERS,
                                    FUN.VALUE = as.character(seq_len(26)),
                                    function(x) paste0(x, LETTERS)))
    while (ret == 1 & nb_attempts <= max_it) {

        ## If we were passed a progress update function, call it
        if (is.function(updateProgress)) {
            text <- paste0("attempt number", nb_attempts)
            updateProgress(detail = text)
        }

        mat = matrix(NA,nrow = nb_rows, ncol = nb_cols)

        ## Generate all the cells that are allowed to be filled
        toVisit <- NULL
        for (i in LETTERS702[seq_len(nb_rows)]) {
            for (j in seq_len(nb_cols)) {
                toVisit <- c(toVisit, paste0(i,j))
            }
        }

        if (!is.null(df_forbidden$Well)) {
            toVisit <- toVisit[!toVisit %in% df_forbidden$Well]
        }

        if (length(toVisit) < nrow(user_df)) {
            return(0)
        }

        ret <- randomWalk(m = mat,
                          toVisit = toVisit,
                          d = user_df,
                          constraint = mod
        )

        if (methods::is(ret, "data.frame")) {
            ret$Well <- paste0(LETTERS702[ret$Row], ret$Column, sep = "")
            ret <- dplyr::mutate_if(ret, is.factor, as.character)
            ret <- dplyr::bind_rows(ret, df_forbidden)
            logging::logwarn("number of attempts: %d",
                             nb_attempts,
                             logger = "fonctions.generateMapPlate")
            return(ret)
        }

        nb_attempts = nb_attempts + 1
    }
    logging::logwarn("we reeched the maximal number of iterations with no success",
                     logger = "fonctions.generateMapPlate")
    return(NULL)
}
