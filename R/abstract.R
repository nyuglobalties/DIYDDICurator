abstract_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Abstract", 
           rHandsontableOutput(ns("abstract")),
           tags$br(),
           actionButton(ns("saveAbstract"), "Save abstracts"),
           tags$hr(),
           tags$p('Definition: An unformatted summary describing the purpose, nature, 
                      and scope of the data collection, special characteristics 
                      of its contents, major subject areas covered, and what 
                      questions the PIs attempted to answer when they conducted 
                      the study.'), 
           tags$p('Only three elements are allowed in content type: 
                      "abstract"; "purpose"; and "mixed".')
  )
}

abstract_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$abstract <- renderRHandsontable({
      req(dat())
      contentTypeOptions <- c(NA_character_, "abstract", "purpose", "mixed")
      abstract <- tibble(
        value = character(),
        contentType = factor(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$abstract) == 0) {
        abstract <- add_row(abstract, value = NA_character_, contentType = NA_character_, lang = "")
      }
      for (a in dat()$stdyDscr$stdyInfo$abstract) {
        if(is.null(a$contentType)) a$contentType <- NA_character_
        if(is.null(a$lang)) a$lang <- NA_character_
        abstract <- add_row(abstract, value = a$value, contentType = a$contentType, lang = a$lang)
      }
      rht <- rhandsontable(abstract, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("contentType", allowInvalid = FALSE, type = "dropdown", source = contentTypeOptions) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$saveAbstract, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedAbstracts <- hot_to_r(input$abstract)
          updatedData$stdyDscr$stdyInfo$abstract <- NULL
          newAbstract <- list()
          for(i in 1:length(updatedAbstracts$value)) {
            if(!is.na(updatedAbstracts$value[i])) {
              if(updatedAbstracts$value[i] != "") {
                new <- list(value = updatedAbstracts$value[i],
                            contentType = updatedAbstracts$contentType[i],
                            lang  = updatedAbstracts$lang[i]
                )
                newAbstract <- c(newAbstract, list(new))
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$abstract <- newAbstract
          updatedData$stdyDscr$stdyInfo$abstract <- recurse_write(updatedData$stdyDscr$stdyInfo$abstract)
          updatedData$stdyDscr$stdyInfo$abstract <- lapply(updatedData$stdyDscr$stdyInfo$abstract,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}