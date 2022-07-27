sampProc_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Sampling Procedure",
           rHandsontableOutput(ns("sampProc")),
           tags$br(),
           actionButton(ns("save_sampProc"), "Save sampling procedure"),
           tags$hr(),
           p('The type of sample and sample design used to select the 
                 survey respondents to represent the population. May include 
                 reference to the target sample size and the sampling fraction.')
           )
}

sampProc_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$sampProc <- renderRHandsontable({
      req(dat())
      sampProc <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$sampProc) == 0) {
        sampProc <- add_row(sampProc, value = NA_character_)
      }
      
      for (s in dat()$stdyDscr$method$dataColl$sampProc) {
        if(is.null(s$lang)) s$lang <- NA_character_
        sampProc <- add_row(sampProc, 
                            value = s$value, 
                            lang = s$lang)
      }
      rht <- rhandsontable(sampProc, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 100),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_sampProc, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_sampProc <- hot_to_r(input$sampProc)
          updatedData$stdyDscr$method$dataColl$sampProc <- NULL
          new_sampProc <- list()
          for(i in 1:length(updated_sampProc$value)) {
            if(!is.na(updated_sampProc$value[i])) {
              if(updated_sampProc$value[i] != "") {
                new <- list(value = updated_sampProc$value[i],
                            lang  = stringr::str_extract(updated_sampProc$lang[i], "^[a-z]{2}")
                )
                new_sampProc <- c(new_sampProc, list(new))
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$sampProc <- new_sampProc
          updatedData$stdyDscr$method$dataColl$sampProc <- recurse_write(updatedData$stdyDscr$method$dataColl$sampProc)
          updatedData$stdyDscr$method$dataColl$sampProc <- lapply(updatedData$stdyDscr$method$dataColl$sampProc,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}