backtrackUI <- function(id, label = NULL) {
  ns <- NS(id)
  fluidRow(
    box(title = h3("Your dataset"),
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 4,
        status = "success",
        dataTableOutput(ns("df_modif"))
    ),
    box(title = h3(),
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 8,
        status = "warning",
        dataTableOutput(ns("forbiddenWells"))


    )
  )

}


backtrack <- function(input, output, session, df, max_iter, forbidden_wells, rows, columns) {

  user_data <- reactive({
    df$Sample.name <- as.integer(df$Sample.name)
    df$Group <- as.factor(df$Group)
    df$Well <- as.character(NA)
    df$Status <- as.factor("allowed")
    df$Row <- NA
    df$Column <- NA
    return(df)
  })

  output$df_modif <- renderDataTable(datatable({
    user_data()
  }, rownames=FALSE)
  )

  output$forbiddenWells <- renderDataTable(datatable({
    forbidden_wells()
  }, rownames = FALSE)
  )

}