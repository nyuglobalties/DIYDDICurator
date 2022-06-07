instrumentDevelopment_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Instrument Development",
           rHandsontableOutput(ns("instrumentDevelopment")),
           tags$br(),
           actionButton(ns("save_instrumentDevelopment"), "Save instrument development"),
           tags$hr(),
           p('Describe any development work on the data collection 
                 instrument. Type attribute allows for the optional use of a 
                 defined development type with or without use of a controlled 
                 vocabulary.')
           )
}

instrumentDevelopment_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$instrumentDevelopment <- renderRHandsontable({
      req(dat())
      instrumentDevelopment <- tibble(
        value = character(),
        type = character(),
        lang = character()
      )
      for (i in dat()$stdyDscr$method$dataColl$instrumentDevelopment) {
        instrumentDevelopment <- add_row(instrumentDevelopment, 
                                         value = i$value, 
                                         type = i$type,
                                         lang = i$lang)
      }
      rht <- rhandsontable(instrumentDevelopment, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_instrumentDevelopment, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_instrumentDevelopment <- hot_to_r(input$instrumentDevelopment)
          updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- NULL
          new_instrumentDevelopment <- list()
          for(i in 1:length(updated_instrumentDevelopment$value)) {
            new <- list(value = updated_instrumentDevelopment$value[i],
                        type = updated_instrumentDevelopment$type[i],
                        lang  = updated_instrumentDevelopment$lang[i]
            )
            new_instrumentDevelopment <- c(new_instrumentDevelopment, list(new))
          }
          updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- new_instrumentDevelopment
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}