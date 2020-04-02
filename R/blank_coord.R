
# function to place the blanks on the plate according to the selected mode: it
# generates a dataframe containing the row and column coordinates for each blank
defineBlankCoords <- function(p_lines, p_cols, mod = "none", start_blank){

  p_lines <- as.numeric(p_lines)
  p_cols <- as.numeric(p_cols)
  if(mod == "none"){
    result <- NULL
  }else{
    if(start_blank == "even"){
      start = 2
    }else{
      start = 1
    }
    if(p_lines == 0 | p_cols == 0){
      result <- NULL
    }else{
      switch (mod,
              "by_column" = {

                nb_rows <- p_lines * ceiling(p_cols/2)
                df <- setnames(data.table::setDF(lapply(c(NA, NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample", "Group", "Sample.name", "Well", "Status", "Row", "Column"))
                k=1
                for(j in seq(from = start, to = p_cols, by=2)){
                  for(i in 1:p_lines){
                    df$Row[k] <- i
                    df$Column[k] <- j
                    k = k + 1

                  }
                }

              },
              "by_row" = {

                nb_rows <- p_cols * ceiling(p_lines/2)
                df <- setnames(data.table::setDF(lapply(c(NA, NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample", "Group", "Sample.name", "Well", "Status", "Row", "Column"))

                k=1
                for(i in seq(from = start, to = p_lines, by=2)){
                  for(j in 1:p_cols){
                    df$Row[k] <- i
                    df$Column[k] <- j
                    k = k + 1

                  }
                }
              },
              "checkerboard" = {
                nb_rows <- p_cols * ceiling(p_lines/2)
                df <- setnames(data.table::setDF(lapply(c(NA, NA, NA, NA, NA, NA, NA), function(...) character(nb_rows))),
                               c("Sample", "Group", "Sample.name", "Well", "Status", "Row", "Column"))
                k=1
                for(j in seq(from = 1, to = p_cols)){
                  if(j%%2 == 0){ # j is even
                    for(i in seq(from = 2, to = p_lines, by=2)){
                      df$Row[k] <- i
                      df$Column[k] <- j
                      k = k + 1
                    }
                  }else{ # j is odd
                    for(i in seq(from = 1, to = p_lines, by=2)){
                      df$Row[k] <- i
                      df$Column[k] <- j
                      k = k + 1
                    }
                  }
                }
              }
      )

      # df$Sample <- NA
      df$Group <- as.character("blank")
      df$Sample.name <- as.integer(NA)
      df$Status <- as.character("blank")
      df$Row <- as.numeric(df$Row)
      df$Column <- as.numeric(df$Column)

      LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
      df$Letters <- LETTERS702[df$Row]
      df$Well <- apply( df[ , c("Letters", "Column") ] , 1 , paste0 , collapse = "" )
      df$Letters <- NULL
      # remove space between letters and numbers
      df$Well <- str_remove(df$Well, " ")

      result <- df %>%
        distinct(Row, Column, .keep_all = TRUE)

      # delete extra rows (Row and Column contain NAs)
      if(anyNA(result$Row) | anyNA(result$Column)){
        result <- na.omit(result, cols = c("Row", "Column"))
      }

    }
  }


  return(result)
}

