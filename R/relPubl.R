relPubl_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Related Publications",
           rHandsontableOutput(ns("relPubl")),
           tags$br(),
           actionButton(ns("save_relPubl"), "Save related publications"),
           tags$hr(),
           tags$p("Bibliographic and access information about articles and 
                  reports based on the data in this collection. Can take the 
                  form of bibliographic citations.")
  )
}

relPubl_server <-  function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$relPubl <- renderRHandsontable({
      req(dat())
      relPubl <- tibble(
        description = character(),
        biblCit = character(),
        format = character(),
        doi = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$othrStdyMat$relPubl) == 0) {
        relPubl <- add_row(relPubl, description = NA_character_)
      }
      
      for (s in dat()$stdyDscr$othrStdyMat$relPubl) {
        if(is.null(s$biblCit)) s$biblCit <- NA_character_
        if(is.null(s$format)) s$format <- NA_character_
        if(is.null(s$doi)) s$doi <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        relPubl <- add_row(relPubl, 
                           description = s$description, 
                           biblCit = s$biblCit,
                           format = s$format,
                           doi = s$doi,
                           lang = s$lang)
      }
      rht <- rhandsontable(relPubl, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    
    observeEvent(
      input$save_relPubl, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedRelPubl <- hot_to_r(input$relPubl)
          updatedData$stdyDscr$othrStdyMat$relPubl <- NULL
          new_relPubl <- list()
          for(i in 1:length(updatedRelPubl$description)) {
            if(!is.na(updatedRelPubl$description[i]) & updatedRelPubl$description[i] == "") updatedRelPubl$description[i] <- NA_character_
            if(!is.na(updatedRelPubl$biblCit[i]) & updatedRelPubl$biblCit[i] == "") updatedRelPubl$biblCit[i] <- NA_character_
            
            if((!is.na(updatedRelPubl$description[i]) & is.na(updatedRelPubl$biblCit[i])) | 
               (is.na(updatedRelPubl$description[i]) & !is.na(updatedRelPubl$biblCit[i])) |
               (!is.na(updatedRelPubl$description[i]) & !is.na(updatedRelPubl$biblCit[i]))) {
                  new <- list(id = paste0("relPubl_", i),
                            description = updatedRelPubl$description[i],
                            biblCit = updatedRelPubl$biblCit[i],
                            format = updatedRelPubl$format[i],
                            doi = updatedRelPubl$doi[i],
                            lang  = stringr::str_extract(updatedRelPubl$lang[i], "^[a-z]{2}")
                  )
                  new_relPubl <- c(new_relPubl, list(new))
            }
          }
          updatedData$stdyDscr$othrStdyMat$relPubl <- new_relPubl
          updatedData$stdyDscr$othrStdyMat$relPubl <- recurse_write(updatedData$stdyDscr$othrStdyMat$relPubl)
          updatedData$stdyDscr$othrStdyMat$relPubl <- lapply(updatedData$stdyDscr$othrStdyMat$relPubl,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}