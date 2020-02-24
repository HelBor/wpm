backtrackUI <- function(id, label = NULL) {
  ns <- NS(id)
  fluidRow(
    box(title = h3("Your dataset"),
        collapsible = TRUE,
        width = 5,
        status = "warning",
        withLoader(
          dataTableOutput(ns("df_modif")),
          type = "html",
          loader = "loader3"
        )

    ),
    box(title = h3("Plate Layout Experiment"),
        width = 7,
        status = "warning",
        withLoader(
          uiOutput(ns("mapPlot")),
          type = "html",
          loader = "loader3"
        )


    )
  )

}


# Server function for backtracking module
# df : dataframe - user data
# nb_g : total number of distinct groups
# max_iter : integer - maximal number of iterations
# forbidden_wells : dataframe  - containing all the blanks and forbidden wells
# rows : integer - plate's number of rows
# columns : integer - plate's number of columns
# constraint : character - neighborhood spatial constraint mode
backtrack <- function(input, output, session, df, max_iter, forbidden_wells, rows, columns, nb_plates, constraint, project_name) {

  toReturn <- reactiveValues(
    final_df = NULL,
    final_map = NULL
  )

  user_data <- reactive({
    df$Group <- as.factor(df$Group)
    df$Well <- as.character(NA)
    df$Status <- as.factor("toRandom")
    df$Row <- NA
    df$Column <- NA
    return(df)
  })


  # map is a list containing:
  #     a map plate : dataframe (containing user data + blanks + forbidden wells, ready to
  #                   be plotted or/and downloaded)
  #     the number of attempts to success
  map <- reactive({
    # look for withProgress but seems to need that we no longer use generateMapPlate
    #since it will be the code needed here to update the progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "WPM running...", value = 0)

    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
      }
      progress$set(value = value, detail = detail)
      progress$inc(amount = 1/max_iter)
    }

    final_df <- data.frame("Sample.name" = as.integer(NA),
                           "Group" = as.factor(NA),
                           "Well" = as.character(NA),
                           "Status" = as.factor(NA),
                           "Row" = as.numeric(NA),
                           "Column" = as.numeric(NA),
                           "Plate" = as.numeric(NA))


    if(nb_plates() > 1){
      # loginfo("on est dans le if nb_plate > 1")
      res <- balancedGrpDistrib(d = user_data(),
                              nb_p = nb_plates())

    }else{
      res <- list("p1" = user_data())
    }

    p <- 1
    for(current_p in res){
      new_df <- NULL
      # print(str(current_p))
      new_df <- generateMapPlate(user_df = current_p,
                                 nb_rows = rows(),
                                 nb_cols = columns(),
                                 df_forbidden = isolate(forbidden_wells()),
                                 mod = isolate(constraint()),
                                 max_it = max_iter,
                                 updateProgress
                                )
      # loginfo("class(new_df): %s",class(new_df), logger = "backtracking")
      if(class(new_df) == "data.frame"){
        new_df$Plate <- p


        final_df <- rbind(final_df, new_df)

      }
      p <- p + 1

    }


    final_df <- final_df[!is.na(final_df$Status),]



    return(final_df)
  })


  output$df_modif <- renderDataTable(datatable({
    map()
  }, rownames=FALSE)
  )

  map_plot <- reactive({
    if(!is.null(map())){
      toPlot = list()
      nb_g <- length(levels(map()$Group))
      toPlot <- lapply(X = 1:nb_plates(),
                       function(x) drawPlateMap(df = map()[which(map()$Plate == x),],
                                                nb_gps = nb_g, plate_lines = rows(),
                                                plate_cols = columns(),
                                                project_title = project_name())
      )
      return(toPlot)
    }
  })


  output$mapPlot <- renderUI({
    lapply(1:length(map_plot()), function(i){
      box(
        title = h4(paste0("Plate ", i)),
        width = 12,
        renderPlot({map_plot()[[i]]})
      )

    }
    )# fin lapply
  })

  observe({
    if(is.null(map())){
      loginfo("map is null so we return errors")
      toReturn$final_df <- "error"
      toReturn$final_map <- NULL
    }else{
      loginfo("map isn't null so we return map and map_plot")
      toReturn$final_df <- map()
      toReturn$final_map <- map_plot()
    }

  })

  return(toReturn)

}