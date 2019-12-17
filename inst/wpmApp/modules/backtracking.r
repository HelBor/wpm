backtrackUI <- function(id, label = NULL) {


}


plateSpec <- function(input, output, session, df, df_forbidden, nbh_mode) {

  user_data <- reactive({
    datafile()$Sample.name <- as.integer(datafile()$Sample.name)
    datafile()$Group <- as.factor(datafile()$Group)
    datafile()$Well <- as.character(NA)
    datafile()$Status <- as.factor("allowed")
    datafile()$Row <- NA
    datafile()$Column <- NA
    datafile()
  })
}