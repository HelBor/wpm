
#*******************************************************************************
# Function to determine the coordinates of the forbidden wells for the plot
# and generates the dataframe containing the Row and Column coordinates
#*******************************************************************************
#' @importFrom rlang .data
convertVector2Df <- function(forbidden_wells, max_Row, max_Col, status){

  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

  if (length(forbidden_wells) > 0) {
    check_columns <- as.numeric(stringr::str_extract(forbidden_wells, "[0-9]+"))
    check_rows <- stringr::str_extract(forbidden_wells, "[aA-zZ]+")

    rows_indices <- as.numeric(match(toupper(check_rows), LETTERS702))

    ## actually simulates if user hasn't finished typing everything
    if (anyNA(rows_indices) | anyNA(check_columns)) {
      return("oh oh... there is a problem")
    }else if ((max(rows_indices) > max_Row) | (max(check_columns) > max_Col) ) {

      # depending on the plate sizes that have been provided
      result <- NULL
    }else{
      # put the forbidden wells into the df
      forbidden <- data.frame(lapply(c(NA, NA, NA, NA, NA, NA, NA),
                                     function(...) character(length(forbidden_wells))),
                       stringsAsFactors = FALSE)
      colnames(forbidden) <- c("Sample", "Group", "ID", "Well", "Status", "Row", "Column")
      forbidden$Sample <- as.character(NA)
      forbidden$Group <- as.character(status)
      forbidden$ID <- as.integer(NA)
      forbidden$Well <- as.character(forbidden_wells)
      forbidden$Status <- as.character(status)
      forbidden$Row <- as.numeric(NA)
      forbidden$Column <- as.numeric(NA)

      # converts Well names to Row / Column coordinates as this is what is used
      # to calculate the backtracking step.
      forbidden <- dplyr::mutate(forbidden,
                          Row = rows_indices,
                          Column = check_columns)
      #erase all duplicated rows
      result <- dplyr::distinct(forbidden, .data$Row, .data$Column, .keep_all = TRUE)

    }

  }else{
    # no forbidden wells, so doesn't return a dataframe
    result <- NULL
  }
  return(result)
}



