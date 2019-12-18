backtrackUI <- function(id, label = NULL) {


}


plateSpec <- function(input, output, session) {

  user_data <- reactive({
    df$Sample.name <- as.integer(df$Sample.name)
    df$Group <- as.factor(df$Group)
    df$Well <- as.character(NA)
    df$Status <- as.factor("allowed")
    df$Row <- NA
    df$Column <- NA
    df
  })
}