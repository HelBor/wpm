##' Randomly take a number of elements in a vector
##' 
##' @description This function allows to pick up the last element in a vector 
##' when the parameter size is equal to 1. Passes parameters to `sample.int` 
##' like size.
##' 
##' @param x is a vector
##' @param ... parameters given to the function sample.int
##' @return a vector of length equal to size parameter.
resample <- function(x, ...) x[sample.int(length(x), ...)]
