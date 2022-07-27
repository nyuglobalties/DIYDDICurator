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

instrumentDevelopment_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$instrumentDevelopment <- renderRHandsontable({
      req(dat())
      instrumentDevelopment <- tibble(
        value = character(),
        type = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$instrumentDevelopment) == 0) {
        instrumentDevelopment <- add_row(instrumentDevelopment, value = NA_character_, type = "", lang = "")
      }
      
      for (i in dat()$stdyDscr$method$dataColl$instrumentDevelopment) {
        if(is.null(i$lang)) i$lang <- NA_character_
        if(is.null(i$type)) i$type <- NA_character_
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
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
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
            if(!is.na(updated_instrumentDevelopment$value[i])) {
              if(updated_instrumentDevelopment$value[i] != "") {
                new <- list(value = updated_instrumentDevelopment$value[i],
                            type = updated_instrumentDevelopment$type[i],
                            lang  = stringr::str_extract(updated_instrumentDevelopment$lang[i], "^[a-z]{2}")
                        )
                new_instrumentDevelopment <- c(new_instrumentDevelopment, list(new))
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- new_instrumentDevelopment
          updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- recurse_write(updatedData$stdyDscr$method$dataColl$instrumentDevelopment)
          updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- lapply(updatedData$stdyDscr$method$dataColl$instrumentDevelopment,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}