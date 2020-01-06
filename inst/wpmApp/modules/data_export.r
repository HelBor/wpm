exportUI <- function(id, label = NULL){
  ns <- NS(id)
  fluidRow(
    box(status="primary",
        width = 6,
        height = 400,
        title = h3("Please choose a file name"),
        textInput(ns("filename"), label = NULL),
        downloadBttn(ns("downloadData"),
                     label = "Download dataframe",
                     style = "unite",
                     color = "warning"),
        downloadBttn(ns("downloadPlot"),
                     label = "Download Plot as png",
                     style = "unite",
                     color = "warning")
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