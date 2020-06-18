##' Convert a CSV File into a valid dataframe for WPM
##'
##' @description This function converts a CSV into a dataframe to make it
##' usable by the shiny application of wpm as well as by the wrapper function
##' (version of wpm in command line). Be sure that the first column of the CSV
##' file corresponds to samples names.
##'
##' @param dt_path file path.
##' @param row_names logical value, indicates if the file has rownames or not.
##' @param gp_field the column name indicating the grouping factor for the
##' samples in the csv. If there is no grouping factor, then gp_field must be
##' set to NULL or "none".
##' @param ... parameters to give to read.csv2 function
##' @return a list containing a dataframe containing the data of the imported
##' CSV and a dataframe containing 3 fields (Sample, Group and ID) which will be
##'  used by WPM. Or NULL if there is an error when giving wrong parameters.
##'
##' @examples
##' test <- data.frame("Sample" = c("s1","s2","s3","s4"),
##'                    "Group" = c("A","A","B", "C"))
##' tf <- tempfile()
##' write.csv2(test, tf, row.names = FALSE)
##' convertCSV(tf, gp_field = "Group", header = TRUE, sep = ";")
##'
##' # if there are row names in the CSV file
##' write.csv2(test, tf)
##' convertCSV(tf, row_names = TRUE, gp_field="Group", header = TRUE, sep = ";")
##'
##' # if there is no grouping factor in the CSV file
##' convertCSV(tf, row_names = TRUE, gp_field ="none", header = TRUE, sep = ";")
##' # gives the same output as the previous example
##' convertCSV(tf, row_names = TRUE, header = TRUE, sep = ";")
##' @export
convertCSV <- function(dt_path, row_names = FALSE, gp_field = NULL, ...){
    if (row_names) {
        df_csv <- tryCatch({
            utils::read.csv2(dt_path, row.names = 1, ...)
            },
            error=function(cond) {
                message(cond)
                return(NULL)
            },
            warning=function(cond) {
                message(cond)
                return(NULL)
            })
    }else{
        df_csv <- tryCatch({
            utils::read.csv2(dt_path, ...)
            },
            error=function(cond) {
                message(cond)
                return(NULL)
            },
            warning=function(cond) {
                message(cond)
                return(NULL)
            })

    }
    # if the given parameters (wrong rownames parameter) lead to a wrong
    # structure of dataframe
    if(length(colnames(df_csv)) == 0){
        return(NULL)
    }

    logging::loginfo("gp_fiels: %s, class: %s",gp_field,class(gp_field))
    if (is.null(gp_field)) {
        df_wpm <- tryCatch({
            data.frame(Sample = df_csv[,1], Group = as.factor(1))
            },
            error=function(cond) {
                message(cond)
                return(NULL)
            },
            warning=function(cond) {
                message(cond)
                return(NULL)
            })
    }else if (gp_field == "none") {
        df_wpm <- tryCatch({
            data.frame(Sample = df_csv[,1], Group = as.factor(1))
        },
        error=function(cond) {
            message(cond)
            return(NULL)
        },
        warning=function(cond) {
            message(cond)
            return(NULL)
        })
    }else{
        # check if user enter an existing field
        if (gp_field %in% colnames(df_csv)) {
            df_wpm <- tryCatch({
                data.frame(Sample = df_csv[,1],
                           Group = as.factor(df_csv[[gp_field]]))
            },
            error=function(cond) {
                message(cond)
                return(NULL)
            },
            warning=function(cond) {
                message(cond)
                return(NULL)
            })
        }else{
            message("The group field must be an existing column name for the
                phenotype Data", call. = FALSE)
            return(NULL)
        }
    }

    if(!is.null(df_wpm)){
        df_wpm$Sample <- as.character(df_wpm$Sample)
        ## a unique ID for each sample which will be used by the drawMap function
        df_wpm$ID <- seq_len(nrow(df_csv))
        toReturn <- list("df_csv" = df_csv, "df_wpm" = df_wpm)
    }
    return(toReturn)
}

##' Reshape a phenotype dataframe for WPM
##' @param pD_df a dataframe containing the phenotype data
##' @param gp_field the column name indicating the grouping factor for the
##' samples
##' @return a dataframe containing 3 fields: Sample, Group and ID.
##'
##' @noRd
reshapeDataframe <- function(pD_df, gp_field){
    if (is.null(gp_field)) {
        df <- data.frame(Sample = rownames(pD_df), Group = as.factor(1))
    }else{
        # check if user enter an existing field
        if (gp_field %in% colnames(pD_df)) {
            df <- data.frame(Sample = rownames(pD_df),
                             Group = as.factor(pD_df[[gp_field]]))
        }else{
            stop("The group field must be an existing column name for the
                phenotype Data", call. = FALSE)
        }
    }
    df$Sample <- as.character(df$Sample)
    df$ID <- seq_len(nrow(df))
    return(df)
}



##' Convert the phenotype data of an ExpressionSet or MsnSet into a dataframe
##' for WPM
##'
##' @description This function converts an ExpressionSet/MsnSet object into a
##' dataframe to make it usable by the shiny application of wpm as well as by
##' the wrapper function (version of wpm in command line)
##'
##' @param eSet_obj an ExpressionSet/MsnSet object contaning the phenotype data
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
    df <- reshapeDataframe(pD_df, gp_field)
    return(df)
}



##' Convert the phenotype data of a SummarizedExperiment into a dataframe for WPM
##'
##' @param se_object a SummarizedExperiment object containing the phenotype data
##' @param gp_field character, corresponding to the phenotype data used to
##' categorize samples into distinct groups if any
##' @return a dataframe containing 3 fields: Sample, Group and ID.
##' @examples
##' nrows <- 200
##' ncols <- 6
##' counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
##' colData <- data.frame(Treatment=rep(c("ChIP", "Input"), 3),
##'                     row.names=LETTERS[1:6])
##' se <- SummarizedExperiment::SummarizedExperiment(assays=list(counts=counts), colData=colData)
##' convertSE(se, "Treatment")
##' @export
convertSE <- function(se_object, gp_field = NULL){
    pD_df <- SummarizedExperiment::colData(se_object)
    df <- reshapeDataframe(pD_df, gp_field)
    return(df)
}
