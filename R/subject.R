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

subject_server <-  function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$subject <- renderRHandsontable({
      req(dat())
      subject <- tibble(
        keyword = character(),
        vocabu = character(),
        vocab_URI = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$subject) == 0) {
        subject <- add_row(subject, keyword = NA_character_)
      }
      
      for (s in dat()$stdyDscr$stdyInfo$subject) {
        if(is.null(s$vocabu)) s$vocabu <- NA_character_
        if(is.null(s$vocab_URI)) s$vocab_URI <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        subject <- add_row(subject, 
                           keyword = s$keyword, 
                           vocabu = s$vocabu,
                           vocab_URI = s$vocab_URI,
                           lang = s$lang)
      }
      rht <- rhandsontable(subject, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
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
            if(!is.na(updatedSubjects$keyword[i])) {
              if(updatedSubjects$keyword[i] != "") {
                new <- list(keyword = updatedSubjects$keyword[i],
                            vocabu = updatedSubjects$vocabu[i],
                            vocab_URI = updatedSubjects$vocab_URI[i],
                            lang  = stringr::str_extract(updatedSubjects$lang[i], "^[a-z]{2}")
                )
                newSubject <- c(newSubject, list(new))
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$subject <- newSubject
          updatedData$stdyDscr$stdyInfo$subject <- recurse_write(updatedData$stdyDscr$stdyInfo$subject)
          updatedData$stdyDscr$stdyInfo$subject <- lapply(updatedData$stdyDscr$stdyInfo$subject,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}