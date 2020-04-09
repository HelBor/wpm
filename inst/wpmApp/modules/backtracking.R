backtrackUI <- function(id, label = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(title = shiny::h3("Your dataset"),
          collapsible = TRUE,
          width = 12,
          status = "warning",
          shinycustomloader::withLoader(
            shiny::uiOutput(ns("data_export")),
            type = "html",
            loader = "loader7"
          )

      )
    ),
    shiny::fluidRow(
      shinydashboard::box(title = shiny::h3("Plate Layout Experiment"),
          width = 12,
          status = "warning",
          shinycustomloader::withLoader(
            shiny::uiOutput(ns("mapPlot")),
            type = "html",
            loader = "loader7"
          )


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
#' @importFrom DT renderDataTable datatable
backtrack <- function(input, output, session, df, max_iter, forbidden_wells, distinct_sample_gps, gp_levels, rows, columns, nb_plates, constraint, project_name) {

  toReturn <- shiny::reactiveValues(
    final_df = NULL,
    final_map = NULL
  )

  user_data <- shiny::reactive({
    df$Group <- as.factor(df$Group)
    df$Well <- as.character(NA)
    df$Status <- as.factor("toRandom")
    df$Row <- as.numeric(NA)
    df$Column <- as.numeric(NA)

    return(df)
  })


  # map is a dataframe containing: user data + blanks + forbidden wells, ready to
  #                   be plotted or/and downloaded)
  map <- shiny::reactive({

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

    final_df <- data.frame("Sample" = as.character(NA),
                           "Group" = as.character(NA),
                           "ID" = as.integer(NA),
                           "Well" = as.character(NA),
                           "Status" = as.character(NA),
                           "Row" = as.numeric(NA),
                           "Column" = as.numeric(NA),
                           "Plate" = as.numeric(NA))

    final_df <- dplyr::mutate_if(final_df, is.factor, as.character)

    if(nb_plates() > 1){
      # loginfo("on est dans le if nb_plate > 1")
      if(is.null(forbidden_wells())){
        nb_forbid <- 0
      }else{
        nb_forbid <- nrow(forbidden_wells())
      }

      logging::loginfo("nb_forbid:%s", nb_forbid, logger = "backtrack/map")
      logging::loginfo("nombre de puits disponibles pour une plaque: %s",(rows()*columns()) - nb_forbid, logger = "backtrack/map")
      logging::loginfo("nombre maximum d'échantillons plaçable sur une plaque : %s", ceiling(nrow(user_data())/nb_plates()), logger = "backtrack/map")
      nb_max <- ceiling(nrow(user_data())/nb_plates())
      res <- balancedGrpDistrib(d = user_data(),
                              nb_p = nb_plates(),
                              df_max_size = nb_max
                              )

    }else{
      res <- list("p1" = user_data())
    }

    for(c in res){
      logging::loginfo("nrow(c): %s", nrow(c), logger = "backtrack/map")

    }

    p <- 1
    for(current_p in res){
      new_df <- NULL
      logging::loginfo("plate n°%s", p)

      new_df <- generateMap(user_df = current_p,
                                 nb_rows = rows(),
                                 nb_cols = columns(),
                                 df_forbidden = shiny::isolate(forbidden_wells()),
                                 mod = shiny::isolate(constraint()),
                                 max_it = max_iter,
                                 updateProgress
                                )
      logging::loginfo("class(new_df): %s",class(new_df), logger = "backtracking")

      if(is.null(new_df)){
        return(new_df)
      }else{
        if(class(new_df) == "data.frame"){
          new_df$Plate <- p
          final_df <- dplyr::bind_rows(final_df, new_df)
        }else if(new_df == 0){
          stop("ERROR, number of available cells is less than number of samples to place.")
        }
      }

      # if(class(new_df) == "data.frame"){
      #   new_df$Plate <- p
      #
      #   final_df <- dplyr::bind_rows(final_df, new_df)
      #
      # }else if(new_df == 0){
      #   stop("ERROR, number of available cells is less than number of samples to place.")
      # }else if(is.null(new_df)){
      #   return(NULL)
      # }
      p <- p + 1

    }


    final_df <- final_df[!is.na(final_df$Status),]
    final_df$Status <- NULL



    return(final_df)
  })

  #-----------------------------------------------------------------------------
  # Dataframe export

  output$data_export <- shiny::renderUI({
    shiny::column(width = 12,
           DT::renderDataTable(DT::datatable({map()}, rownames = FALSE)),
           shiny::downloadHandler(
             filename = function() {
               paste("data-", Sys.Date(), ".csv", sep="")
             },
             content = function(file) {
               write.csv2(map(), file, row.names = FALSE, quote = FALSE)
             }
           )
           )


  })

  #-----------------------------------------------------------------------------
  # Plots export
  map_plot <- shiny::reactive({
    if(!is.null(map())){
      toPlot = list()
      toPlot <- lapply(X = 1:nb_plates(),
                       function(x) drawMap(df = map()[which(map()$Plate == x),],
                                                sample_gps = distinct_sample_gps(),
                                                gp_levels = gp_levels(),
                                                plate_lines = rows(),
                                                plate_cols = columns(),
                                                project_title = project_name())
      )
      return(toPlot)
    }
  })



  output$mapPlot <- shiny::renderUI({
    lapply(1:length(map_plot()), function(i){
      shinydashboard::box(
        title = shiny::h4(paste0("Plate ", i)),
        width = 6,
        shiny::renderPlot({map_plot()[[i]]}),
        shiny::downloadHandler(
          filename = function() {
            paste("plot", i, ".png", sep="")
          },
          content = function(file) {
            ggplot2::ggsave(filename = file,
                   plot = map_plot()[[i]],
                   width = 10,
                   height = 7,
                   units = "in"
            )
          }
        )
      )

    }
    )# fin lapply
  })
  #-----------------------------------------------------------------------------

  shiny::observe({
    if(is.null(map())){
      logging::loginfo("map is null so we return errors")
      toReturn$final_df <- "error"
      toReturn$final_map <- NULL
    }else{
      logging::loginfo("map isn't null so we return map and map_plot")
      toReturn$final_df <- map()
      toReturn$final_map <- map_plot()
    }

  })

  return(toReturn)

}