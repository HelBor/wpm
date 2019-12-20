exportUI <- function(id, label = NULL){
  ns <- NS(id)
  fluidRow(
    textInput(ns("filename"), label = "Please choose a file name"),
    downloadButton(ns("downloadData"), "Download dataframe"),
    downloadButton(ns("downloadPlot"), "Download Plot as png")
  )
}




export <- function(input, output, session, df, plot){

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df(),
                file,
                row.names = FALSE,
                quote = FALSE,
                sep = ";",
                fileEncoding = "UTF-8")
    }
  )


  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("plotMap", ".png", sep = "")
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