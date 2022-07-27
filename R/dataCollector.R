dataCollector_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Data Collectors",
           rHandsontableOutput(ns("dataCollector")),
           tags$br(),
           actionButton(ns("save_dataCollector"), "Save data collector"),
           tags$hr(),
           p('The entity (individual, agency, or institution) responsible 
               for administering the questionnaire or interview or compiling 
               the data. This refers to the entity collecting the data, not to 
               the entity producing the documentation. Attribute "abbr" may be 
               used to list common abbreviations given to agencies, etc.'), 
           p('Attribute "affiliation" may be used to record affiliation of the 
               data collector. The role attribute specifies the role of person 
               in the data collection process.'))
}

dataCollector_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$dataCollector <- renderRHandsontable({
      req(dat())
      dataCollector <- tibble(
        value = character(),
        abbr = character(),
        affiliation = character(),
        role = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$dataCollector) == 0) {
        dataCollector <- add_row(dataCollector, 
                                 value = NA_character_, 
                                 abbr = "",
                                 affiliation = "",
                                 role = "",
                                 lang = "")
      }
      
      for (d in dat()$stdyDscr$method$dataColl$dataCollector) {
        if(is.null(d$abbr)) d$abbr <- NA_character_
        if(is.null(d$affiliation)) d$affiliation <- NA_character_
        if(is.null(d$role)) d$role <- NA_character_
        if(is.null(d$lang)) d$lang <- NA_character_
        dataCollector <- add_row(dataCollector, 
                                 value = d$value, 
                                 abbr = d$abbr,
                                 affiliation = d$affiliation,
                                 role = d$role,
                                 lang = d$lang)
      }
      rht <- rhandsontable(dataCollector, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(30, 10, 30, 30, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    
    observeEvent(
      input$save_dataCollector, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_dataCollector <- hot_to_r(input$dataCollector)
          updatedData$stdyDscr$method$dataColl$dataCollector <- NULL
          new_dataCollector <- list()
          for(i in 1:length(updated_dataCollector$value)) {
            if(!is.na(updated_dataCollector$value[i])) {
              if(updated_dataCollector$value[i] != "") {
                new <- list(value = updated_dataCollector$value[i],
                        abbr = updated_dataCollector$abbr[i],
                        affiliation = updated_dataCollector$affiliation[i],
                        role = updated_dataCollector$role[i],
                        lang  = stringr::str_extract(updated_dataCollector$lang[i], "^[a-z]{2}")
                )
                new_dataCollector <- c(new_dataCollector, list(new))
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$dataCollector <- new_dataCollector
          updatedData$stdyDscr$method$dataColl$dataCollector <- recurse_write(updatedData$stdyDscr$method$dataColl$dataCollector)
          updatedData$stdyDscr$method$dataColl$dataCollector <- lapply(updatedData$stdyDscr$method$dataColl$dataCollector, function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}