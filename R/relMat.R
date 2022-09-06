relMat_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Related Materials",
           rHandsontableOutput(ns("relMat")),
           tags$br(),
           actionButton(ns("save_relMat"), "Save related materials"),
           tags$hr(),
           tags$p("Describes materials related to the study description, such 
                  as appendices, additional information on sampling found in 
                  other documents, etc. Can take the form of bibliographic 
                  citations.")
  )
}

relMat_server <-  function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$relMat <- renderRHandsontable({
      req(dat())
      relMat <- tibble(
        description = character(),
        biblCit = character(),
        format = character(),
        doi = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$othrStdyMat$relMat) == 0) {
        relMat <- add_row(relMat, description = NA_character_)
      }
      
      for (s in dat()$stdyDscr$othrStdyMat$relMat) {
        if(is.null(s$biblCit)) s$biblCit <- NA_character_
        if(is.null(s$format)) s$format <- NA_character_
        if(is.null(s$doi)) s$doi <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        relMat <- add_row(relMat, 
                           description = s$description, 
                           biblCit = s$biblCit,
                           format = s$format,
                           doi = s$doi,
                           lang = s$lang)
      }
      rht <- rhandsontable(relMat, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    
    observeEvent(
      input$save_relMat, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedRelMat <- hot_to_r(input$relMat)
          updatedData$stdyDscr$othrStdyMat$relMat <- NULL
          new_relMat <- list()
          for(i in 1:length(updatedRelMat$description)) {
            if(!is.na(updatedRelMat$description[i]) & updatedRelMat$description[i] == "") updatedRelMat$description[i] <- NA_character_
            if(!is.na(updatedRelMat$biblCit[i]) & updatedRelMat$biblCit[i] == "") updatedRelMat$biblCit[i] <- NA_character_
            
            if((!is.na(updatedRelMat$description[i]) & is.na(updatedRelMat$biblCit[i])) | 
               (is.na(updatedRelMat$description[i]) & !is.na(updatedRelMat$biblCit[i])) | 
               (!is.na(updatedRelMat$description[i]) & !is.na(updatedRelMat$biblCit[i]))) {
                  new <- list(id = paste0("relMat_", i),
                              description = updatedRelMat$description[i],
                              biblCit = updatedRelMat$biblCit[i],
                              format = updatedRelMat$format[i],
                              doi = updatedRelMat$doi[i],
                              lang  = stringr::str_extract(updatedRelMat$lang[i], "^[a-z]{2}")
                  )
                  new_relMat <- c(new_relMat, list(new))
            }
          }
          updatedData$stdyDscr$othrStdyMat$relMat <- new_relMat
          updatedData$stdyDscr$othrStdyMat$relMat <- recurse_write(updatedData$stdyDscr$othrStdyMat$relMat)
          updatedData$stdyDscr$othrStdyMat$relMat <- lapply(updatedData$stdyDscr$othrStdyMat$relMat,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}