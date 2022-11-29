collDate_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Dates of Data Collection",
          rHandsontableOutput(ns("collDate")),
          tags$br(),
          actionButton(ns("save_collDate"), "Save data collection dates"),
          tags$hr(),
          p('collDate Definition: Contains the date(s) when the data were 
            collected. Use the event attribute to specify "start", "end", or 
            "single" for each date entered. The "cycle" attribute permits 
            specification of the relevant cycle, wave, or round of data. 
            Maps to Dublin Core Coverage element. Inclusion of this element 
            in the codebook is recommended.')
  )
}

collDate_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$collDate <- renderRHandsontable({
      req(dat())
      eventOptions <- c(NA_character_, "start", "end", "single")
      prds <- tibble(
        value = character(),
        date = as.Date(""),
        event = character(),
        cycle = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$collDate) == 0) {
        prds <- add_row(prds, value = NA_character_)
      }
      
      for (cd in dat()$stdyDscr$stdyInfo$sumDscr$collDate) {
        if(is.null(cd$date)) cd$date <- NA_character_
        if(is.null(cd$event)) cd$event <- NA_character_
        if(is.null(cd$cycle)) cd$cycle <- NA_character_
        if(is.null(cd$lang)) cd$lang <- NA_character_
        prds <- add_row(prds,
                        value = cd$value,
                        date = as.Date(cd$date), 
                        event = cd$event,
                        cycle = cd$cycle,
                        lang = cd$lang
        )
      }
      rht <- rhandsontable(prds, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("event", allowInvalid = FALSE, type = "dropdown", source = eventOptions) %>%
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_collDate, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_prds <- hot_to_r(input$collDate)
          updated_prds$event[is.na(updated_prds$event) | updated_prds$event == "" ] <- "single"
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- NULL
          new_collDate <- list()
          if(length(updated_prds$value) > 0) {
            for(i in 1:length(updated_prds$value)) {
              if(!is.na(updated_prds$value[i])) {
                if(updated_prds$value[i] != "") {
                  new_cd <- list(value = updated_prds$value[i],
                                 date = as.character(updated_prds$date[i]),
                                 event = updated_prds$event[i],
                                 cycle = updated_prds$cycle[i],
                                 lang = stringr::str_extract(updated_prds$lang[i], "^[a-z]{2}")
                  )
                  new_collDate <- c(new_collDate, list(new_cd))
                }
              }
            }
          }
          
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- new_collDate
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$collDate)
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$collDate,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

######################

timeMeth_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Time Method",
           rHandsontableOutput(ns("timeMeth")),
           tags$br(),
           actionButton(ns("save_timeMeth"), "Save time method"),
           tags$hr(),
           p('The time method or time dimension of the data collection.')
  )
}

timeMeth_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$timeMeth <- renderRHandsontable({
      req(dat())
      timeMeth <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$timeMeth) == 0) {
        timeMeth <- add_row(timeMeth, value = NA_character_)
      }
      for (t in dat()$stdyDscr$method$dataColl$timeMeth) {
        if(is.null(t$lang)) t$lang <- NA_character_
        timeMeth <- add_row(timeMeth, 
                            value = t$value, 
                            lang = t$lang)
      }
      rht <- rhandsontable(timeMeth, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_timeMeth, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_timeMeth <- hot_to_r(input$timeMeth)
          updatedData$stdyDscr$method$dataColl$timeMeth <- NULL
          new_timeMeth <- list()
          if(length(updated_timeMeth$value) > 0) {
            for(i in 1:length(updated_timeMeth$value)) {
              if(!is.na(updated_timeMeth$value[i])) {
                if(updated_timeMeth$value[i] != "") {
                  new <- list(value = updated_timeMeth$value[i],
                              lang  = stringr::str_extract(updated_timeMeth$lang[i], "^[a-z]{2}")
                  )
                  new_timeMeth <- c(new_timeMeth, list(new))
                }
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$timeMeth <- new_timeMeth
          updatedData$stdyDscr$method$dataColl$timeMeth <- recurse_write(updatedData$stdyDscr$method$dataColl$timeMeth)
          updatedData$stdyDscr$method$dataColl$timeMeth <- lapply(updatedData$stdyDscr$method$dataColl$timeMeth,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

########################

frequenc_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Frequency of Data Collection",
           rHandsontableOutput(ns("frequenc")),
           tags$br(),
           actionButton(ns("save_frequenc"), "Save frequency"),
           tags$hr(),
           p('For data collected at more than one point in time, the 
                 frequency with which the data were collected.')
  )
}

frequenc_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$frequenc <- renderRHandsontable({
      req(dat())
      frequenc <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$frequenc) == 0) {
        frequenc <- add_row(frequenc, value = NA_character_, lang = "")
      }
      
      for (f in dat()$stdyDscr$method$dataColl$frequenc) {
        if(is.null(f$lang)) f$lang <- NA_character_
        frequenc <- add_row(frequenc, 
                            value = f$value, 
                            lang = f$lang)
      }
      rht <- rhandsontable(frequenc, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_frequenc, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_frequenc <- hot_to_r(input$frequenc)
          updatedData$stdyDscr$method$dataColl$frequenc <- NULL
          new_frequenc <- list()
          if(length(updated_frequenc$value) > 0) {
            for(i in 1:length(updated_frequenc$value)) {
              if(!is.na(updated_frequenc$value[i])) {
                if(updated_frequenc$value[i] != "") {
                  new <- list(value = updated_frequenc$value[i],
                              lang  = stringr::str_extract(updated_frequenc$lang[i], "^[a-z]{2}")
                  )
                  new_frequenc <- c(new_frequenc, list(new))
                }
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$frequenc <- new_frequenc
          updatedData$stdyDscr$method$dataColl$frequenc <- recurse_write(updatedData$stdyDscr$method$dataColl$frequenc)
          updatedData$stdyDscr$method$dataColl$frequenc <- lapply(updatedData$stdyDscr$method$dataColl$frequenc,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

#########################

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
          if(length(updated_dataCollector$value) > 0) {
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
          }
          updatedData$stdyDscr$method$dataColl$dataCollector <- new_dataCollector
          updatedData$stdyDscr$method$dataColl$dataCollector <- recurse_write(updatedData$stdyDscr$method$dataColl$dataCollector)
          updatedData$stdyDscr$method$dataColl$dataCollector <- lapply(updatedData$stdyDscr$method$dataColl$dataCollector, function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

#######################

collMode_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Mode of Data Collection",
           rHandsontableOutput(ns("collMode")),
           tags$br(),
           actionButton(ns("save_collMode"), "Save mode of data collection"),
           tags$hr(),
           p('The method used to collect the data; instrumentation 
                 characteristics.')
  )
  
}

collMode_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$collMode <- renderRHandsontable({
      req(dat())
      collMode <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$collMode) == 0) {
        collMode <- add_row(collMode, value = NA_character_, lang = "")
      }
      
      for (c in dat()$stdyDscr$method$dataColl$collMode) {
        if(is.null(c$lang)) c$lang <- NA_character_
        collMode <- add_row(collMode, 
                            value = c$value, 
                            lang = c$lang)
      }
      rht <- rhandsontable(collMode, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_collMode, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_collMode <- hot_to_r(input$collMode)
          updatedData$stdyDscr$method$dataColl$collMode <- NULL
          new_collMode <- list()
          if(length(updated_collMode$value) > 0) {
            for(i in 1:length(updated_collMode$value)) {
              for(i in 1:length(updated_collMode$value)) {
                if(!is.na(updated_collMode$value[i])) {
                  new <- list(value = updated_collMode$value[i],
                              lang  = stringr::str_extract(updated_collMode$lang[i], "^[a-z]{2}")
                  )
                  new_collMode <- c(new_collMode, list(new))
                }
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$collMode <- new_collMode
          updatedData$stdyDscr$method$dataColl$collMode <- recurse_write(updatedData$stdyDscr$method$dataColl$collMode)
          updatedData$stdyDscr$method$dataColl$collMode <- lapply(updatedData$stdyDscr$method$dataColl$collMode,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

######################

collSitu_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Characteristics of Data Collection Situation",
           rHandsontableOutput(ns("collSitu")),
           tags$br(),
           actionButton(ns("save_collSitu"), "Save characteristics of data collection situation"),
           tags$hr(),
           p('Description of noteworthy aspects of the data collection 
                 situation. Includes information on factors such as c
                 ooperativeness of respondents, duration of interviews, number 
                 of call-backs, etc.')
  )
}

collSitu_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$collSitu <- renderRHandsontable({
      req(dat())
      collSitu <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$collSitu) == 0) {
        collSitu <- add_row(collSitu, value = NA_character_, lang = "")
      }
      
      for (c in dat()$stdyDscr$method$dataColl$collSitu) {
        if(is.null(c$lang)) c$lang <- NA_character_
        collSitu <- add_row(collSitu, 
                            value = c$value, 
                            lang = c$lang)
      }
      rht <- rhandsontable(collSitu, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_collSitu, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_collSitu <- hot_to_r(input$collSitu)
          updatedData$stdyDscr$method$dataColl$collSitu <- NULL
          new_collSitu <- list()
          if(length(updated_collSitu$value) > 0) {
            for(i in 1:length(updated_collSitu$value)) {
              if(!is.na(updated_collSitu$value[i])) {
                if(updated_collSitu$value[i] != "") {
                  new <- list(value = updated_collSitu$value[i],
                              lang  = stringr::str_extract(updated_collSitu$lang[i], "^[a-z]{2}")
                  )
                  new_collSitu <- c(new_collSitu, list(new))
                }
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$collSitu <- new_collSitu
          updatedData$stdyDscr$method$dataColl$collSitu <- recurse_write(updatedData$stdyDscr$method$dataColl$collSitu)
          updatedData$stdyDscr$method$dataColl$collSitu <- lapply(updatedData$stdyDscr$method$dataColl$collSitu,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

########################

collectorTraining_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Collector Training",
           rHandsontableOutput(ns("collectorTraining")),
           tags$br(),
           actionButton(ns("save_collectorTraining"), "Save collector training"),
           tags$hr(),
           p('Describes the training provided to data collectors including 
             interviewer training, process testing, compliance with standards 
             etc. This is repeatable for language and to capture different 
             aspects of the training process. The type attribute allows 
             specification of the type of training being described.')
  )
  
}

collectorTraining_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$collectorTraining <- renderRHandsontable({
      req(dat())
      collectorTraining <- tibble(
        value = character(),
        type = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$collectorTraining) == 0) {
        collectorTraining <- add_row(collectorTraining, value = NA_character_, type = "", lang = "")
      }
      
      
      for (c in dat()$stdyDscr$method$dataColl$collectorTraining) {
        if(is.null(c$type)) c$type <- NA_character_
        if(is.null(c$lang)) c$lang <- NA_character_
        collectorTraining <- add_row(collectorTraining, 
                                     value = c$value, 
                                     type = c$type,
                                     lang = c$lang)
      }
      rht <- rhandsontable(collectorTraining, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_collectorTraining, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_collectorTraining <- hot_to_r(input$collectorTraining)
          updatedData$stdyDscr$method$dataColl$collectorTraining <- NULL
          new_collectorTraining <- list()
          if(length(updated_collectorTraining$value) > 0) {
            for(i in 1:length(updated_collectorTraining$value)) {
              if(!is.na(updated_collectorTraining$value[i])) {
                if(updated_collectorTraining$value[i] != "") {
                  new <- list(value = updated_collectorTraining$value[i],
                              type = updated_collectorTraining$type[i],
                              lang  = stringr::str_extract(updated_collectorTraining$lang[i], "^[a-z]{2}")
                  )
                  new_collectorTraining <- c(new_collectorTraining, list(new))
                }
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$collectorTraining <- new_collectorTraining
          updatedData$stdyDscr$method$dataColl$collectorTraining <- recurse_write(updatedData$stdyDscr$method$dataColl$collectorTraining)
          updatedData$stdyDscr$method$dataColl$collectorTraining <- lapply(updatedData$stdyDscr$method$dataColl$collectorTraining,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

#############################

resInstru_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Type of Research Instrument",
           rHandsontableOutput(ns("resInstru")),
           tags$br(),
           actionButton(ns('save_resInstru'), "Save type of research instrument"),
           tags$hr(),
           p('The type of data collection instrument used. "Structured" 
                 indicates an instrument in which all respondents are asked the 
                 same questions/tests, possibly with precoded answers. If a 
                 small portion of such a questionnaire includes open-ended 
                 questions, provide appropriate comments. "Semi-structured" 
                 indicates that the research instrument contains mainly 
                 open-ended questions. "Unstructured" indicates that in-depth 
                 interviews were conducted.')
  )
}

resInstru_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$resInstru <- renderRHandsontable({
      req(dat())
      resInstru <- tibble(
        value = character(),
        type = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$resInstru) == 0) {
        resInstru <- add_row(resInstru, value = NA_character_)
      }
      
      for (r in dat()$stdyDscr$method$dataColl$resInstru) {
        if(is.null(r$type)) r$type <- NA_character_
        if(is.null(r$lang)) r$lang <- NA_character_
        
        resInstru <- add_row(resInstru, 
                             value = r$value, 
                             type = r$type,
                             lang = r$lang)
      }
      rht <- rhandsontable(resInstru, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_resInstru, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_resInstru <- hot_to_r(input$resInstru)
          updatedData$stdyDscr$method$dataColl$resInstru <- NULL
          new_resInstru <- list()
          if(length(updated_resInstru$value) > 0) {
            for(i in 1:length(updated_resInstru$value)) {
              if(!is.na(updated_resInstru$value[i])) {
                if(updated_resInstru$value[i] != "") {
                  new <- list(value = updated_resInstru$value[i],
                              type = updated_resInstru$type[i],
                              lang  = stringr::str_extract(updated_resInstru$lang[i], "^[a-z]{2}")
                  )
                  new_resInstru <- c(new_resInstru, list(new))
                }
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$resInstru <- new_resInstru
          updatedData$stdyDscr$method$dataColl$resInstru <- recurse_write(updatedData$stdyDscr$method$dataColl$resInstru)
          updatedData$stdyDscr$method$dataColl$resInstru <- lapply(updatedData$stdyDscr$method$dataColl$resInstru,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

########################

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
          if(length(updated_instrumentDevelopment$value) > 0) {
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
          }
          updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- new_instrumentDevelopment
          updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- recurse_write(updatedData$stdyDscr$method$dataColl$instrumentDevelopment)
          updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- lapply(updatedData$stdyDscr$method$dataColl$instrumentDevelopment,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

########################

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

ConOps_server <- function(id, dat, filepth, lang) {
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
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
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
          if(length(updated_ConOps$value) > 0) {
            for(i in 1:length(updated_ConOps$value)) {
              if(!is.na(updated_ConOps$value[i])) {
                if(updated_ConOps$value[i] != "") {
                  new <- list(value = updated_ConOps$value[i],
                              agency = updated_ConOps$agency[i],
                              lang  = stringr::str_extract(updated_ConOps$lang[i], "^[a-z]{2}")
                  )
                  new_ConOps <- c(new_ConOps, list(new))
                }
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

#############################

actMin_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Actions to Minimize Losses",
           rHandsontableOutput(ns("actMin")),
           tags$br(),
           actionButton(ns("save_actMin"), "Save actions to minimize losses"),
           tags$hr(),
           p('Summary of actions taken to minimize data loss. Includes 
                 information on actions such as follow-up visits, supervisory 
                 checks, historical matching, estimation, etc.')
  )
}

actMin_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$actMin <- renderRHandsontable({
      req(dat())
      actMin <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$actMin) == 0) {
        actMin <- add_row(actMin, value = NA_character_, lang = "")
      }
      
      for (a in dat()$stdyDscr$method$dataColl$actMin) {
        if(is.null(a$lang)) a$lang <- NA_character_
        actMin <- add_row(actMin, 
                          value = a$value, 
                          lang = a$lang)
      }
      rht <- rhandsontable(actMin, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_actMin, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_actMin <- hot_to_r(input$actMin)
          updatedData$stdyDscr$method$dataColl$actMin <- NULL
          new_actMin <- list()
          if(length(updated_actMin$value) > 0) {
            for(i in 1:length(updated_actMin$value)) {
              if(!is.na(updated_actMin$value[i])) {
                if(updated_actMin$value[i] != "") {
                  new <- list(value = updated_actMin$value[i],
                              lang  = stringr::str_extract(updated_actMin$lang[i], "^[a-z]{2}")
                  )
                  new_actMin <- c(new_actMin, list(new))
                }
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$actMin <- new_actMin
          updatedData$stdyDscr$method$dataColl$actMin <- recurse_write(updatedData$stdyDscr$method$dataColl$actMin)
          updatedData$stdyDscr$method$dataColl$actMin <- lapply(updatedData$stdyDscr$method$dataColl$actMin,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

#######################

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
          if(length(updated_sampProc$value) > 0) {
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
          }
          updatedData$stdyDscr$method$dataColl$sampProc <- new_sampProc
          updatedData$stdyDscr$method$dataColl$sampProc <- recurse_write(updatedData$stdyDscr$method$dataColl$sampProc)
          updatedData$stdyDscr$method$dataColl$sampProc <- lapply(updatedData$stdyDscr$method$dataColl$sampProc,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

###############################

deviat_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Major Deviations from Sample Design",
           rHandsontableOutput(ns("deviat")),
           tags$br(),
           actionButton(ns("save_deviat"), "Save major deviations from Sample Design"),
           tags$hr(),
           p('Information indicating correspondence as well as discrepancies 
                 between the sampled units (obtained) and available statistics 
                 for the population (age, sex-ratio, marital status, etc.) as a 
                 whole.'))
}

deviat_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$deviat <- renderRHandsontable({
      req(dat())
      deviat <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$deviat) == 0) {
        deviat <- add_row(deviat, value = NA_character_, lang = "")
      }
      
      for (d in dat()$stdyDscr$method$dataColl$deviat) {
        if(is.null(d$lang)) d$lang <- NA_character_
        deviat <- add_row(deviat, 
                          value = d$value, 
                          lang = d$lang)
      }
      rht <- rhandsontable(deviat, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_deviat, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_deviat <- hot_to_r(input$deviat)
          updatedData$stdyDscr$method$dataColl$deviat <- NULL
          new_deviat <- list()
          if(length(updated_deviat$value) > 0) {
            for(i in 1:length(updated_deviat$value)) {
              if(!is.na(updated_deviat$value[i])) {
                if(updated_deviat$value[i] != "") {
                  new <- list(value = updated_deviat$value[i],
                              lang  = stringr::str_extract(updated_deviat$lang[i], "^[a-z]{2}")
                  )
                  new_deviat <- c(new_deviat, list(new))
                }
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$deviat <- new_deviat
          updatedData$stdyDscr$method$dataColl$deviat <- recurse_write(updatedData$stdyDscr$method$dataColl$deviat)
          updatedData$stdyDscr$method$dataColl$deviat <- lapply(updatedData$stdyDscr$method$dataColl$deviat,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}