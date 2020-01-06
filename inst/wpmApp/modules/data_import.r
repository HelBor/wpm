# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    fileInput(ns("file"), label),
    materialSwitch(
      inputId = ns("heading"),
      label = "Header",
      value = FALSE,
      status = "warning",
      right = TRUE
    ),
    # checkboxInput(ns("heading"), "Header"),
    # Input: Select quotes
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Single quote" = "'",
      "Double quote" = "\""
    )),
    # Input: Select separator
    radioButtons(ns("sep_input"), "Separator",
                 choices = c(Semicolon = ";",
                             Comma = ",",
                             Tab = "\t"),
                 selected = ";")
  )
}

# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  observe({
    loginfo("File %s was uploaded", userFile()$name, logger = "data_import")

  })

  # The user's data, parsed into a data frame
  dataframe <- reactive({
    df <- read.csv2(userFile()$datapath,
             header = input$heading,
             quote = input$quote,
             sep = input$sep_input,
             col.names = c("Sample.name", "Group"),
             stringsAsFactors = stringsAsFactors)

    return(df)
  })


  # We can run observers in here if we want to


  # Return the reactive that yields the data frame
  return(dataframe)
}
