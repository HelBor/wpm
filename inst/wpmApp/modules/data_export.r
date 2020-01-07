exportUI <- function(id, label = NULL){
  ns <- NS(id)
  fluidRow(
    column(width =4,
      box(status="warning",
          width = 12,
          height = 400,
          title = h3("Please choose a file name"),
          textInput(ns("filename"), label = NULL),
      column(width = 6,
        downloadBttn(ns("downloadData"),
                     label = "Download dataframe",
                     style = "unite",
                     size = "sm",
                     color = "warning")),
      column(width = 6,
        downloadBttn(ns("downloadPlot"),
                     label = "Download Plot as png",
                     style = "unite",
                     size = "sm",
                     color = "warning"))


      )
    )


  )
}




export <- function(input, output, session, df, plot){

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, ".csv", sep = "")
    },
    content = function(file) {
      write_excel_csv2(df(),
                file)
    }
  )


  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0(input$filename, ".png", sep = "")
    },
    content = function(file) {
      ggsave(file,
             plot = plot(),
             device = "png",
             width = 10,
             height = 7,
             units = "in")
    }
  )
}