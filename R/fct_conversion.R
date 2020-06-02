##' Convert a vector of plate coordinates into a dataframe
##'
##' @description Function converting the format of "Letter-Digit" coordinates
##' into a dataframe containing these coordinates in Row, Column.
##' @param chr_wells character string containing the wells
##' @param max_Row integer, maximal number of lines in the plate
##' @param max_Col integer, maximal number of columns in the plate
##' @param status character, the status of the wells
##' @return result, dataframe containing wells coordinates
##' @importFrom rlang .data
##' @examples
##' # convert the vector of well coordinates into a dataframe
##' convertVector2Df("A1,C2,A3,B12,C42",3,42,"specify_status")
##'
##' # supports uppercase / lowercase letters
##' convertVector2Df("a1,C2,A3,b12,C42",3,42,"specify_status")
##'
##' @export
convertVector2Df <- function(chr_wells, max_Row, max_Col, status = NA){

    if (!methods::is(chr_wells, "character")) {
        stop("the object passed in chr_wells must be a character string.")
    }
    if (!methods::is(max_Row, "numeric")) {
        stop("max_Row must be numeric")
    }
    if (!methods::is(max_Col, "numeric")) {
        stop("max_Col must be numeric")
    }
    if (is.null(status)) {
        stop("status can not be NULL")
    }

    wells <- toupper(as.vector(unlist(base::strsplit(as.character(chr_wells),
                                             split = ","))))
    LETTERS702 <- c(LETTERS, vapply(LETTERS,
                                    FUN.VALUE = as.character(seq_len(26)),
                                    function(x) paste0(x, LETTERS)))

    if (length(wells) > 0) {
        check_columns <- as.numeric(stringr::str_extract(wells, "[0-9]+"))
        check_rows <- stringr::str_extract(wells, "[aA-zZ]+")
        rows_indices <- as.numeric(match(check_rows, LETTERS702))

        ## actually simulates if user hasn't finished typing everything
        if (anyNA(rows_indices) | anyNA(check_columns)) {
            return(NULL)
        }else if ((max(rows_indices) > max_Row)
                  | (max(check_columns) > max_Col) ) {
            # depending on the plate sizes that have been provided
            logging::logwarn("the dimensions of the plate are incompatible with
                             the wells.")
            result <- NULL
        }else{
            # transform the wells into a df
            df <- data.frame(
                lapply(c(NA, NA, NA, NA, NA, NA, NA),
                       function(...) character(length(wells))),
                stringsAsFactors = FALSE)
            colnames(df) <- c("Sample", "Group", "ID", "Well", "Status", "Row",
                              "Column")
            df$Sample <- as.character(NA)
            df$Group <- as.character(status)
            df$ID <- as.integer(NA)
            df$Well <- as.character(wells)
            df$Status <- as.character(status)
            df$Row <- as.numeric(NA)
            df$Column <- as.numeric(NA)
            ## converts Well names to Row / Column coordinates as this is what
            ## is used to calculate the backtracking step.
            df <- dplyr::mutate(df, Row = rows_indices, Column = check_columns)
            ## erase all duplicated rows
            result <- dplyr::distinct(df, .data$Row, .data$Column,
                                      .keep_all = TRUE)
        }
    }else{
        ## no wells provided, so doesn't return a dataframe
        result <- NULL
    }
    return(result)
}



