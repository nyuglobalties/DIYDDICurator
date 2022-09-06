relStdy_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Related Studies",
           rHandsontableOutput(ns("relStdy")),
           tags$br(),
           actionButton(ns("save_relStdy"), "Save related studies"),
           tags$hr(),
           tags$p("Information on the relationship of the current data 
                  collection to others (e.g., predecessors, successors, other 
                  waves or rounds) or to other editions of the same file. This 
                  would include the names of additional data collections 
                  generated from the same data collection vehicle plus other 
                  collections directed at the same general topic. Can take the 
                  form of bibliographic citations.")
  )
}

relStdy_server <-  function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$relStdy <- renderRHandsontable({
      req(dat())
      relStdy <- tibble(
        description = character(),
        biblCit = character(),
        format = character(),
        doi = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$othrStdyMat$relStdy) == 0) {
        relStdy <- add_row(relStdy, description = NA_character_)
      }
      
      for (s in dat()$stdyDscr$othrStdyMat$relStdy) {
        if(is.null(s$biblCit)) s$biblCit <- NA_character_
        if(is.null(s$format)) s$format <- NA_character_
        if(is.null(s$doi)) s$doi <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        relStdy <- add_row(relStdy, 
                           description = s$description, 
                           biblCit = s$biblCit,
                           format = s$format,
                           doi = s$doi,
                           lang = s$lang)
      }
      rht <- rhandsontable(relStdy, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    
    observeEvent(
      input$save_relStdy, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedRelStdy <- hot_to_r(input$relStdy)
          updatedData$stdyDscr$othrStdyMat$relStdy <- NULL
          new_relStdy <- list()
          for(i in 1:length(updatedRelStdy$description)) {
            if(!is.na(updatedRelStdy$description[i]) & updatedRelStdy$description[i] == "") updatedRelStdy$description[i] <- NA_character_
            if(!is.na(updatedRelStdy$biblCit[i]) & updatedRelStdy$biblCit[i] == "") updatedRelStdy$biblCit[i] <- NA_character_
            
            if((!is.na(updatedRelStdy$description[i]) & is.na(updatedRelStdy$biblCit[i])) | 
               (is.na(updatedRelStdy$description[i]) & !is.na(updatedRelStdy$biblCit[i])) |
               (!is.na(updatedRelStdy$description[i]) & !is.na(updatedRelStdy$biblCit[i]))) {
                    new <- list(id = paste0("relStdy_", i),
                            description = updatedRelStdy$description[i],
                            biblCit = updatedRelStdy$biblCit[i],
                            format = updatedRelStdy$format[i],
                            doi = updatedRelStdy$doi[i],
                            lang  = stringr::str_extract(updatedRelStdy$lang[i], "^[a-z]{2}")
                    )
                    new_relStdy <- c(new_relStdy, list(new))
            }
          }
          updatedData$stdyDscr$othrStdyMat$relStdy <- new_relStdy
          updatedData$stdyDscr$othrStdyMat$relStdy <- recurse_write(updatedData$stdyDscr$othrStdyMat$relStdy)
          updatedData$stdyDscr$othrStdyMat$relStdy <- lapply(updatedData$stdyDscr$othrStdyMat$relStdy,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}