##' Determines buffer wells coordinates on a plate
##'
##' @description function to place the buffer solutions on the plate according
##' to the selected mode: it generates a dataframe containing the row and column
##' coordinates for each buffer solution.
##'
##' @param p_lines number of rows on the plate
##' @param p_cols number of columns on the plate
##' @param mod character, can be "none", "by_column", "by_row" or "checkerboard"
##' @param start_buffer character, "even" means that the buffers will be
##' positioned on the even rows of the plate. Otherwise, they will be
##' positioned on the odd rows.
##' @importFrom rlang .data
##' @return a dataframe containing the buffer wells with their coordinates.
defineBufferCoords <- function(p_lines, p_cols, mod = "none", start_buffer){
    p_lines <- as.numeric(p_lines)
    p_cols <- as.numeric(p_cols)

    if (!(mod %in% c("none","by_row","by_column","checkerboard"))) {
        return("wrong pattern provided. Choose between 'none', 'by_row',
               'by_column' or 'checkerboard'")
    }
    if (mod == "none") {
        result <- NULL
    }else{
        if (start_buffer == "even") {
            start = 2
        }else{
            start = 1
        }

        if (p_lines == 0 | p_cols == 0) {
            result <- NULL
        }else{
            switch(
                mod,
                "by_column" = {
                    nb_rows <- p_lines * ceiling(p_cols / 2)
                    df <- data.frame(lapply(c(NA, NA, NA, NA, NA, NA, NA),
                                            function(...) character(nb_rows)),
                                     stringsAsFactors = FALSE)
                    colnames(df) <- c("Sample", "Group", "ID", "Well",
                                      "Status", "Row", "Column")
                    k = 1
                    for (j in seq(from = start, to = p_cols, by = 2)) {
                        for (i in seq_len(p_lines)) {
                            df$Row[k] <- i
                            df$Column[k] <- j
                            k = k + 1
                        }
                    }

                },
                "by_row" = {
                    nb_rows <- p_cols * ceiling(p_lines / 2)
                    df <- data.frame(lapply(c(NA, NA, NA, NA, NA, NA, NA),
                                            function(...) character(nb_rows)),
                                     stringsAsFactors = FALSE)
                    colnames(df) <- c("Sample", "Group", "ID", "Well",
                                      "Status", "Row", "Column")

                    k = 1
                    for (i in seq(from = start, to = p_lines, by = 2)) {
                        for (j in seq_len(p_cols)) {
                            df$Row[k] <- i
                            df$Column[k] <- j
                            k = k + 1
                        }
                    }
                },
                "checkerboard" = {
                    nb_rows <- p_cols * ceiling(p_lines / 2)
                    df <- data.frame(lapply(c(NA, NA, NA, NA, NA, NA, NA),
                                            function(...) character(nb_rows)),
                                     stringsAsFactors = FALSE)
                    colnames(df) <- c("Sample", "Group", "ID", "Well",
                                      "Status", "Row", "Column")
                    k = 1
                    for (j in seq(from = 1, to = p_cols)) {
                        if (j %% 2 == 0) {
                            ## j is even
                            for (i in seq(from = 2, to = p_lines, by = 2)) {
                                df$Row[k] <- i
                                df$Column[k] <- j
                                k = k + 1
                            }
                        }else{
                            ## j is odd
                            for (i in seq(from = 1, to = p_lines, by = 2)) {
                                df$Row[k] <- i
                                df$Column[k] <- j
                                k = k + 1
                            }
                        }
                    }
                }
            )
            df$Group <- as.character("buffer")
            df$ID <- as.integer(NA)
            df$Status <- as.character("buffer")
            df$Row <- as.numeric(df$Row)
            df$Column <- as.numeric(df$Column)

            LETTERS702 <- c(LETTERS,
                            vapply(LETTERS,
                                   FUN.VALUE = as.character(seq_len(26)),
                                   function(x) paste0(x, LETTERS)))
            df$Letters <- LETTERS702[df$Row]
            df$Well <- apply( df[ , c("Letters", "Column") ] ,
                              1, paste0, collapse = "" )
            df$Letters <- NULL
            ## remove space between letters and numbers
            df$Well <- stringr::str_remove(df$Well, " ")

            result <- dplyr::distinct(df,
                                      .data$Row,
                                      .data$Column,
                                      .keep_all = TRUE)

            ## delete extra rows (Row and Column contain NAs)
            if (anyNA(result$Row) | anyNA(result$Column)) {
                result <- stats::na.omit(result, cols = c("Row", "Column"))
            }
        }
    }
    return(result)
}

