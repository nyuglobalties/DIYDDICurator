subject_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Subjects",
           rHandsontableOutput(ns("subject")),
           tags$br(),
           actionButton(ns("saveSubject"), "Save subjects"),
           tags$hr(),
           tags$p("Defintion: Words or phrases that describe salient aspects of a data 
             collection's content. Can be used for building keyword indexes and 
             for classification and retrieval purposes. A controlled vocabulary 
             can be employed. Maps to Dublin Core Subject element. The 'vocab' 
             attribute is provided for specification of the controlled 
             vocabulary in use, e.g., LCSH, MeSH, etc. The 'vocabURI attribute 
             specifies the location for the full controlled vocabulary.")
  )
}

subject_server <-  function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$subject <- renderRHandsontable({
      req(dat())
      subject <- tibble(
        keyword = character(),
        vocab = character(),
        vocabURI = character(),
        lang = character()
      )
      for (s in dat()$stdyDscr$stdyInfo$subject) {
        subject <- add_row(subject, 
                           keyword = s$keyword, 
                           vocab = s$vocab,
                           vocabURI = s$vocabURI,
                           lang = s$lang)
      }
      rht <- rhandsontable(subject, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    
    observeEvent(
      input$saveSubject, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedSubjects <- hot_to_r(input$subject)
          updatedData$stdyDscr$stdyInfo$subject <- NULL
          newSubject <- list()
          for(i in 1:length(updatedSubjects$keyword)) {
            new <- list(keyword = updatedSubjects$keyword[i],
                        vocab = updatedSubjects$vocab[i],
                        vocabURI = updatedSubjects$vocabURI[i],
                        lang  = updatedSubjects$lang[i]
            )
            newSubject <- c(newSubject, list(new))
          }
          updatedData$stdyDscr$stdyInfo$subject <- newSubject
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}