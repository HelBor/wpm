##' Backtracking Function
##'
##' @description Function used to launch the backtracking algorithm on a
##' dataframe with the corresponding plate parameters, number of iterations and
##' special wells
##'
##' @param max_iter numeric, the maximal number of iterations to do, default
##' value is 20
##' @param user_data dataframe, user samples to place randomly on the plate
##' @param wells dataframe, special wells not to be placed randomly on the plate
##' @param rows numeric, number of lines on the plate(s)
##' @param columns numeric, number of columns on the plate(s)
##' @param nb_plates numeric, number of plates
##' @param constraint character, spatial mode
##' @param prog progress bar used for shiny app only
##'
##' @return a dataframe containing user samples and special wells with their
##' coordinates for the corresponding plates.
backtracking <- function(max_iter = 20, user_data, wells, rows, columns,
                        nb_plates, constraint, prog = NULL){
    final_df <- data.frame(
        "Sample" = as.character(NA),
        "Group" = as.character(NA),
        "ID" = as.integer(NA),
        "Well" = as.character(NA),
        "Status" = as.character(NA),
        "Row" = as.numeric(NA),
        "Column" = as.numeric(NA),
        "Plate" = as.numeric(NA)
    )

    final_df <- dplyr::mutate_if(final_df, is.factor, as.character)

    if (nb_plates > 1) {
        if (is.null(wells)) {
            nb_forbid <- 0
        }else{
            nb_forbid <- nrow(wells)
        }

        logging::loginfo("nb_forbid:%s",
                        nb_forbid,
                        logger = "backtrack/map")
        logging::loginfo("number of wells available for a plate: %s",
                        (rows*columns) - nb_forbid,
                        logger = "backtrack/map")
        logging::loginfo("maximum number of samples that can be placed on a
                        plate: %s",
                        ceiling(nrow(user_data)/nb_plates),
                        logger = "backtrack/map")
        nb_max <- ceiling(nrow(user_data) / nb_plates)
        res <- balancedGrpDistrib(d = user_data,
                                nb_p = nb_plates,
                                df_max_size = nb_max
        )

    }else{
        res <- list("p1" = user_data)
    }

    for (c in res) {
        logging::loginfo("nrow(c): %s", nrow(c), logger = "backtrack/map")
    }

    p <- 1
    # We generate a map for each plate
    for (current_p in res) {
        new_df <- NULL
        logging::loginfo("plate number %s", p)

        new_df <- generateMap(
            user_df = current_p,
            nb_rows = rows,
            nb_cols = columns,
            df_forbidden = wells,
            mod = constraint,
            max_it = max_iter,
            prog
        )
        logging::loginfo("class(new_df): %s",
                        class(new_df),
                        logger = "backtracking")

        if (is.null(new_df)) {
            return(new_df)
        }else{
            if (methods::is(new_df, "data.frame")) {
                new_df$Plate <- p
                final_df <- dplyr::bind_rows(final_df, new_df)
            }else if (new_df == 0) {
                message("WARNING, number of available cells is less than number
                    of samples to place.")
                return(NULL)
            }
        }
        p <- p + 1

    }

    final_df <- final_df[!is.na(final_df$Status), ]
    final_df$Status <- NULL

    return(final_df)

}
