dataKind_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Kind of Data",
           rHandsontableOutput(ns("dataKind")),
           tags$br(),
           actionButton(ns("save_dataKind"), "Save kinds of data"),
           tags$hr(),
           p('The type of data included in the file: survey data, census/enumeration 
             data, aggregate data, clinical data, event/transaction data, program 
             source code, machine-readable text, administrative records data, 
             experimental data, psychological test, textual data, coded textual, 
             coded documents, time budget diaries, observation data/ratings, 
             process-produced data, etc. This element maps to Dublin Core Type 
             element.')
  )
}

dataKind_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$dataKind <- renderRHandsontable({
      req(dat())
      dataKind <- tibble(
        value = character(),
        type = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$dataKind) == 0) {
        dataKind <- add_row(dataKind, value = NA_character_)
      }
      
      for (d in dat()$stdyDscr$stdyInfo$sumDscr$dataKind) {
        if(is.null(d$lang)) d$lang <- NA_character_
        if(is.null(d$lang)) d$type <- NA_character_
        dataKind <- add_row(dataKind, 
                            value = d$value, 
                            type = d$type,
                            lang = d$lang)
      }
      rht <- rhandsontable(dataKind, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_dataKind, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_dataKind <- hot_to_r(input$dataKind)
          updatedData$stdyDscr$stdyInfo$sumDscr$dataKind <- NULL
          new_dataKind <- list()
          for(i in 1:length(updated_dataKind$value)) {
            if(!is.na(updated_dataKind$value[i])) {
              if(updated_dataKind$value[i] != "") {
                new <- list(value = updated_dataKind$value[i],
                            type = updated_dataKind$type[i],
                            lang = stringr::str_extract(updated_dataKind$lang[i], "^[a-z]{2}")
                )
                new_dataKind <- c(new_dataKind, list(new))
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$dataKind <- new_dataKind
          updatedData$stdyDscr$stdyInfo$sumDscr$dataKind <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$dataKind)
          updatedData$stdyDscr$stdyInfo$sumDscr$dataKind <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$dataKind,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}