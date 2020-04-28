## This function allows to pick up the last element in a vector when the parameter
## size is equal to 1
resample <- function(x, ...) x[sample.int(length(x), ...)]
