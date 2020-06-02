##' Convert a CSV File into a dataframe
##'
##' @description This function converts a CSV into a dataframe to make it
##' usable by the shiny application of wpm as well as by the wrapper function
##' (version of wpm in command line)
##'
##' @param dt_path file path.
##' @param head logical value, indicates if the file has a header on it's first
##' line.
##' @param qt character, the set of quoting characters. To disable quoting
##' altogether, use quote = "".
##' @param sep character, the field separator character. Values on each line
##' of the file are separated by this character.
##' @param row_names logical value, does the file have rownames?
##'
##' @return a dataframe containing 3 fields: Sample, Group and ID.
##'
##' @examples
##' test <- data.frame("Sample" = c("s1","s2","s3","s4"),
##'                    "Group" = c("A","A","B", "C"))
##' tf <- tempfile()
##' write.csv2(test, tf, row.names = FALSE)
##' convertCSV(tf, head = TRUE, sep = ";")
##'
##' # if there are row names in the CSV file
##' write.csv2(test, tf)
##' convertCSV(tf, head = TRUE, sep = ";", row_names = TRUE)
##' @export
convertCSV <- function(dt_path, head = FALSE, qt = "", sep = "",
                       row_names = FALSE){
    if (row_names) {
        df <- utils::read.csv2(dt_path,
                               header = head,
                               quote = qt,
                               sep = sep,
                               stringsAsFactors = FALSE,
                               row.names = 1
        )
    }else{
        df <- utils::read.csv2(dt_path,
                               header = head,
                               quote = qt,
                               sep = sep,
                               stringsAsFactors = FALSE
        )
    }
    ## check if file contains groups or not
    if (length(colnames(df)) == 1) {
        colnames(df) <- "Sample"
        df$Group <- as.factor(1)
    }else{
        colnames(df) <- c("Sample", "Group")
    }
    df$Sample <- as.character(df$Sample)
    ## a unique ID for each sample which will be used by the drawMap function
    df$ID <- seq_len(nrow(df))

    return(df)
}


##' Convert an ExpressionSet or MsnSet into a dataframe
##'
##' @description This function converts an ExpressionSet/MsnSet object into a
##' dataframe to make it usable by the shiny application of wpm as well as by
##' the wrapper function (version of wpm in command line)
##'
##' @param eSet_obj an ExpressionSet/MsnSet object contaning at least the
##' phenotype Data
##' @param gp_field character, corresponding to the phenotype data used to
##' categorize samples into distinct groups if any
##'
##' @return a dataframe containing 3 fields: Sample, Group and ID.
##'
##' @examples
##' sample_names <- c("s1","s2","s3","s4", "s5")
##' M <- matrix(NA, nrow = 4, ncol = 5)
##' colnames(M) <- sample_names
##' rownames(M) <- paste0("id", LETTERS[1:4])
##' pd <- data.frame(Environment = rep_len(LETTERS[1:3], 5),
##'                  Category = rep_len(1:2, 5), row.names = sample_names)
##' rownames(pd) <- colnames(M)
##' x <- MSnbase::MSnSet(exprs = M,pData =  pd)
##' convertESet(x, "Environment")
##'
##' @export
convertESet <- function(eSet_obj, gp_field = NULL){

    pD_df <- Biobase::pData(eSet_obj)

    if (is.null(gp_field)) {
        df <- data.frame(Sample = rownames(pD_df), Group = as.factor(1))
    }else{
        df <- data.frame(Sample = rownames(pD_df),
                         Group = as.factor(pD_df[[gp_field]]))
    }

    df$ID <- seq_len(nrow(df))

    return(df)
}
