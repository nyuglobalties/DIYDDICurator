ConOps_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Control Operations",
           rHandsontableOutput(ns("ConOps")),
           tags$br(),
           actionButton(ns("save_ConOps"), "Save control operations"),
           tags$hr(),
           p('Methods to facilitate data control performed by the primary 
                 investigator or by the data archive. Specify any special 
                 programs used for such operations. The "agency" attribute maybe 
                 used to refer to the agency that performed the control 
                 operation.')
           )
}

ConOps_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$ConOps <- renderRHandsontable({
      req(dat())
      ConOps <- tibble(
        value = character(),
        agency = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$ConOps) == 0) {
        ConOps <- add_row(ConOps, value = NA_character_, agency = "", lang = "")
      }
      
      
      for (c in dat()$stdyDscr$method$dataColl$ConOps) {
        if(is.null(c$agency)) c$agency <- NA_character_
        if(is.null(c$lang)) c$lang <- NA_character_
        ConOps <- add_row(ConOps, 
                          value = c$value, 
                          agency = c$agency,    
                          lang = c$lang)
      }
      rht <- rhandsontable(ConOps, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_ConOps, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_ConOps <- hot_to_r(input$ConOps)
          updatedData$stdyDscr$method$dataColl$ConOps <- NULL
          new_ConOps <- list()
          for(i in 1:length(updated_ConOps$value)) {
            if(!is.na(updated_ConOps$value[i])) {
              if(updated_ConOps$value[i] != "") {
                new <- list(value = updated_ConOps$value[i],
                        agency = updated_ConOps$agency[i],
                        lang  = updated_ConOps$lang[i]
                )
                new_ConOps <- c(new_ConOps, list(new))
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$ConOps <- new_ConOps
          updatedData$stdyDscr$method$dataColl$ConOps <- recurse_write(updatedData$stdyDscr$method$dataColl$ConOps)
          updatedData$stdyDscr$method$dataColl$ConOps <- lapply(updatedData$stdyDscr$method$dataColl$ConOps,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}