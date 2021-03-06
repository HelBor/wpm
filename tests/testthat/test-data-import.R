# check that the function returns a list when giving a CSV file
testthat::test_that(
    "CSV conversion dataframe expectation", {
        test <- data.frame("Sample" = c("s1","s2","s3","s4"),
                            "Group" = c("A","A","B", "C"))
        tf <- tempfile()
        write.csv2(test, tf)
        imported_file <- convertCSV(tf, header = TRUE, sep = ";", gp_field = "Group")
        testthat::expect_type(imported_file, "list")
        testthat::expect_s3_class(imported_file$df_csv, "data.frame")
        testthat::expect_s3_class(imported_file$df_wpm, "data.frame")
    }
)


# check that the function returns a dataframe when giving a MSnSet
testthat::test_that(
    "MSnSet conversion dataframe expectation", {
        sample_names <- c("s1","s2","s3","s4", "s5")
        M <- matrix(NA, nrow = 4, ncol = 5)
        colnames(M) <- sample_names
        rownames(M) <- paste0("id", LETTERS[1:4])
        pd <- data.frame(Environment = rep_len(LETTERS[1:3], 5),
                         Category = rep_len(1:2, 5),
                         row.names = sample_names)
        rownames(pd) <- colnames(M)
        x <- MSnbase::MSnSet(exprs = M,pData =  pd)
        testthat::expect_s3_class(convertESet(x, "Environment"), "data.frame")
    }
)


# check that the function returns a dataframe when giving a
# SummarizedExperiment
testthat::test_that(
    "SummarizedExperiment conversion dataframe expectations", {
        nrows <- 200
        ncols <- 6
        counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
        colData <- data.frame(Treatment = rep(c("ChIP", "Input"), 3),
                            row.names = LETTERS[1:6])
        se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = counts),
                                   colData = colData)
        testthat::expect_s3_class(convertSE(se, "Treatment"), "data.frame")
        # check that returns an error when giving a wrong gp_field parameter
        testthat::expect_error(convertSE(se, "wrong_column_name"))
    }
)


