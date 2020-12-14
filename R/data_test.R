##' @title data_test
##' @docType data
##' @description A demo dataset containing the age and other attributes of 193
##' fictitious patients. It aims to help the user to test the shiny application
##' of the wpm package.
##' @format A data frame with 193 rows and 7 variables:
##' \describe{
##'     \item{samples}{the samples to be analyzed representing fictitious patients.}
##'     \item{age}{age of the patients under 5 age groups, in years}
##'     \item{gender}{gender of the patients, F (for Female) and M (for Male)}
##'     \item{treatment}{the treatment each patient received, Ctrl (Control), treatment A, treatment B and treatment C}
##'     \item{diabetes}{presence of diabetes, 0 (no) and 1 (yes) }
##'     \item{gender.treatment}{A combination between the gender and treatment fields}
##'     \item{age.diabetes}{A combination between the age and the diabetes fields}
##' }
##' @source 
"data_test"
