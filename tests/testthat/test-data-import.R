# check that the function returns a dataframe when giving a CSV file
testthat::test_that(
    "CSV conversion dataframe expectation", {
        test <- data.frame("Sample" = c("s1","s2","s3","s4"),
                            "Group" = c("A","A","B", "C"))
        tf <- tempfile()
        write.csv2(test, tf, row.names = FALSE)
        testthat::expect_s3_class(
            convertCSV(tf, head = TRUE, sep = ";"),
            "data.frame")
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





