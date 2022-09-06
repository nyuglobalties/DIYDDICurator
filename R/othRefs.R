othRefs_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Other References",
           rHandsontableOutput(ns("othRefs")),
           tags$br(),
           actionButton(ns("save_othRefs"), "Save other references"),
           tags$hr(),
           tags$p("Indicates other pertinent references. Can take the form of 
                  bibliographic citations.")
  )
}

othRefs_server <-  function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$othRefs <- renderRHandsontable({
      req(dat())
      othRefs <- tibble(
        description = character(),
        biblCit = character(),
        format = character(),
        doi = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$othrStdyMat$othRefs) == 0) {
        othRefs <- add_row(othRefs, description = NA_character_)
      }
      
      for (s in dat()$stdyDscr$othrStdyMat$othRefs) {
        if(is.null(s$biblCit)) s$biblCit <- NA_character_
        if(is.null(s$format)) s$format <- NA_character_
        if(is.null(s$doi)) s$doi <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        othRefs <- add_row(othRefs, 
                           description = s$description, 
                           biblCit = s$biblCit,
                           format = s$format,
                           doi = s$doi,
                           lang = s$lang)
      }
      rht <- rhandsontable(othRefs, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    
    observeEvent(
      input$save_othRefs, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedOthRefs <- hot_to_r(input$othRefs)
          updatedData$stdyDscr$othrStdyMat$othRefs <- NULL
          new_othRefs <- list()
          for(i in 1:length(updatedOthRefs$description)) {
            if(!is.na(updatedOthRefs$description[i]) & updatedOthRefs$description[i] == "") updatedOthRefs$description[i] <- NA_character_
            if(!is.na(updatedOthRefs$biblCit[i]) & updatedOthRefs$biblCit[i] == "") updatedOthRefs$biblCit[i] <- NA_character_
            
            if((!is.na(updatedOthRefs$description[i]) & is.na(updatedOthRefs$biblCit[i])) | 
               (is.na(updatedOthRefs$description[i]) & !is.na(updatedOthRefs$biblCit[i])) | 
               (!is.na(updatedOthRefs$description[i]) & !is.na(updatedOthRefs$biblCit[i]))) {
                   new <- list(id = paste0("othRefs_", i),
                            description = updatedOthRefs$description[i],
                            biblCit = updatedOthRefs$biblCit[i],
                            format = updatedOthRefs$format[i],
                            doi = updatedOthRefs$doi[i],
                            lang  = stringr::str_extract(updatedOthRefs$lang[i], "^[a-z]{2}")
                   )
                   new_othRefs <- c(new_othRefs, list(new))
            }
          }
          updatedData$stdyDscr$othrStdyMat$othRefs <- new_othRefs
          updatedData$stdyDscr$othrStdyMat$othRefs <- recurse_write(updatedData$stdyDscr$othrStdyMat$othRefs)
          updatedData$stdyDscr$othrStdyMat$othRefs <- lapply(updatedData$stdyDscr$othrStdyMat$othRefs,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}