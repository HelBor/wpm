##' Binds multiple dataframes together
##'
##' @description Function that merges dataframes that contain wells of
##' different types. To do this, it verifies that all the conditions provided
##' are compatible with each other in order to be able to launch WPM on this
##' data.
##' @param forbidden_w dataframe, the forbidden wells
##' @param buffer_w datarame, the buffer wells
##' @param fixed_w dataframe, the quality control wells
##' @param nb_samples numeric, the number of samples to place using the
##' backtracking algorithm.
##' @param totalNbWells, numeric, the total number of wells that can be filled.
##' @param nb_p numeric, number of plates to fill
##' @return a dataframe containing all the special wells
joinDataframes <- function(forbidden_w = NULL, buffer_w = NULL,
                           fixed_w = NULL, nb_samples, totalNbWells, nb_p){
    ret <- NULL
    if (is.null(forbidden_w)) {
        nb_f <- 0
    }else{
        nb_f <- nrow(forbidden_w)
    }

    if (is.null(buffer_w)) {
        nb_b <- 0
    }else{
        nb_b <- nrow(buffer_w)
    }

    if (is.null(fixed_w)) {
        nb_nR <- 0
    }else{
        nb_nR <- nrow(fixed_w)
    }

    if (nb_samples >= (totalNbWells)) {
        ret <- "The dimensions of the plate are not compatible with the number
        of samples to be placed. Please increase the number of plates to fill
        or provide a dataset with fewer samples."
        return(ret)
    }

    ## if there are forbidden wells
    if (!is.null(forbidden_w)) {
        ## if there are buffer wells
        if (!is.null(buffer_w)) {
            ## if there are fixed wells
            if (!is.null(fixed_w)) {
                if (nb_samples >= (totalNbWells - (nb_b * nb_p) - (nb_f * nb_p) - (nb_nR * nb_p))) {
                    ret <- "The dimensions of the plate are not compatible with
                    the number of samples to be placed. Maybe are you specifying
                    to many forbidden/buffers/fixed wells."
                }else{
                    ## We put the forbidden wells first because they have priority over
                    ## the buffers ones.
                    result <- base::rbind(forbidden_w,
                                          buffer_w,
                                          fixed_w)
                    result <- dplyr::distinct(result, .data$Row, .data$Column,
                                              .keep_all = TRUE)
                    ret <- result
                }
            ## If there is no fixed wells
            }else{
                if (nb_samples >= (totalNbWells - (nb_b * nb_p) - (nb_f * nb_p))) {
                    ret <- "The buffer mode and/or forbidden wells selected are
                    not compatible with the plate's dimensions and the number of
                    samples to be placed. If you want to keep this buffer mode,
                    please increase the number of plates to fill or provide a
                    dataset with fewer samples. Otherwise, please change the
                    buffer mode."
                }else{
                    ## We put the forbidden wells first because they have priority over
                    ## the buffers ones.
                    result <- base::rbind(forbidden_w, buffer_w)
                    result <- dplyr::distinct(result, .data$Row, .data$Column,
                                              .keep_all = TRUE)
                    ret <- result
                }
            }
        ## if there is no buffer wells
        }else{
            ## if there is fixed well
            if (!is.null(fixed_w)) {

                if (nb_samples >= (totalNbWells - (nb_f * nb_p) - (nb_nR * nb_p) )) {
                    ret <- "The dimensions of the plate are not compatible with
                    the number of samples to be placed. Maybe are you specifying
                    to many forbidden/fixed wells."
                }else{
                    result <- base::rbind(forbidden_w, fixed_w)
                    result <- dplyr::distinct(result, .data$Row, .data$Column,
                                              .keep_all = TRUE)
                    ret <- result
                }
            ## if there is no fixed well
            }else{
                if (nb_samples >= (totalNbWells - (nb_f * nb_p))) {
                    ret <- "The forbidden wells selected are not compatible with
                    the plate's dimensions and the number of samples to be
                    placed. To solve this issue, please:
                    - decrease the number of forbidden wells
                    - or increase the number of plates to fill
                    - or provide a dataset with fewer samples."
                }else{
                    ret <- forbidden_w
                }
            }
        }
    }else{
        ## if there are buffer wells
        if (!is.null(buffer_w)) {
            ## if there are fixed wells
            if (!is.null(fixed_w)) {
                if (nb_samples >= (totalNbWells - (nb_b * nb_p) - (nb_nR * nb_p) )) {
                    ret <- "The dimensions of the plate are not compatible with
                    the number of samples to be placed. Maybe are you specifying
                    to many buffers/fixed wells."
                }else{
                    result <- base::rbind(buffer_w, fixed_w)
                    result <- dplyr::distinct(result, .data$Row, .data$Column,
                                              .keep_all = TRUE)
                    ret <- result
                }
            ## if there is no fixed well
            }else{
                if (nb_samples >= (totalNbWells - (nb_b*nb_p))) {
                    ret <- "The buffer mode selected is not compatible with the
                    plate's dimensions and the number of samples to be placed.
                    If you want to keep this buffer mode, please increase the
                    number of plates to fill or provide a dataset with fewer
                    samples. Otherwise, please change the buffer mode."
                }else{
                    ret <- buffer_w
                }
            }
        ## if there is no buffer well
        }else{
            ## if there are fixed wells
            if (!is.null(fixed_w)) {
                if (nb_samples >= totalNbWells - (nb_nR*nb_p)) {
                    ret <- "The dimensions of the plate are not compatible with
                    the number of samples to be placed. Maybe are you specifying
                    to many fixed wells."
                }else{
                    ret <- fixed_w
                }
            ## si pas de fixed
            }else{
                ret <- NULL
            }
        }
    }
    return(ret)
}
