# check that the function returns a dataframe
testthat::test_that(
    "conversion dataframe expectation",
    {
        testthat::expect_s3_class(
            convertVector2Df("A1,C2,A3,B12,C42",3,42,"blank"),
            "data.frame"
        )
    }
)

# check that the function returns an error message when user gives a wrong object
testthat::test_that(
    "Wrong object passed for argument chr_wells",
    {
        testthat::expect_error(convertVector2Df(48,3,42,"blank"))
    }
)

# check that the function returns an error when the user gives wells that do 
# not correspond to the dimensions of the plate he has provided.
# In this test, we give a wrong number of columns (5), we should have given 
# a number> = 42 because of well C42. 
testthat::test_that(
    "incompatibility between plate dimensions and wells.",
    {
        testthat::expect_null(
            convertVector2Df("A1,C2,A3,B12,C42",3,5,"blank")
        )
        
        testthat::expect_null(
            convertVector2Df("A1,C2,A3,B12,C42",2,45,"blank")
        )
    }
    
)


