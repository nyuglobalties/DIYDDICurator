title_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Title",
           h4("Authoritative title of study"),
           rHandsontableOutput(ns("titl")),
           tags$br(),
           h4("Translated title(s)"),
           rHandsontableOutput(ns("parTitl")),
           tags$br(),
           actionButton(ns("save_titl"), "Save titles"),
           tags$hr(),
           p('The title is the full authoritative title for the work at the 
             appropriate level: marked-up document; marked-up document source; 
             study; other material(s) related to study description; other material(s) 
             related to study. The study title will in most cases be identical 
             to the title for the marked-up document. A full title should 
             indicate the geographic scope of the data collection as well as the 
             time period covered.'),
           p('The parallel title or translated title is the title translated into 
             another language. Use the lang column to specify the language.')
           )
}

title_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$titl <- renderRHandsontable({
      req(dat())
      title <- tibble(
        value = character(),
        lang = character()
      )
      
      for (t in dat()$stdyDscr$citation$titlStmt$titl) {
        title <- add_row(title, 
                         value = t$value, 
                         lang = t$lang)
      }
      rht <- rhandsontable(title, stretchH = "all", overflow = "visible") %>% 
        hot_cols(colWidths = c(40, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    output$parTitl <- renderRHandsontable({
      req(dat())
      parTitl <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$citation$titlStmt$parTitl) == 0) {
        parTitl <- add_row(parTitl, value = NA_character_, lang = "")
      }
      
      for (p in dat()$stdyDscr$citation$titlStmt$parTitl) {
        parTitl <- add_row(parTitl, 
                           value = p$value, 
                           lang = p$lang)
      }
      rht <- rhandsontable(parTitl, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_titl, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_title <- hot_to_r(input$titl)
          if(is.na(updated_title$value) |updated_title$value == "") {
            new_titl <- list(list(value = "Placeholder title", lang = NA_character_))
          } else {
            new_titl <- list(list(value = updated_title$value, 
                                  lang = stringr::str_extract(updated_title$lang, "^[a-z]{2}")))
          }
          updatedData$stdyDscr$citation$titlStmt$titl <- NULL
          updatedData$stdyDscr$citation$titlStmt$titl <- new_titl
          updatedData$stdyDscr$citation$titlStmt$titl <- recurse_write(updatedData$stdyDscr$citation$titlStmt$titl)
          updatedData$stdyDscr$citation$titlStmt$titl <- lapply(updatedData$stdyDscr$citation$titlStmt$titl, function(x) x[!is.na(x)])
          
          updated_parTitl <- hot_to_r(input$parTitl)
          updatedData$stdyDscr$citation$titlStmt$parTilt <- NULL
          new_parTitl <- list()
          for(i in 1:length(updated_parTitl$value)) {
            if(!is.na(updated_parTitl$value[i])) {
              if(updated_parTitl$value[i] != "") {
                new <- list(value = updated_parTitl$value[i],
                            lang  = stringr::str_extract(updated_parTitl$lang[i], "^[a-z]{2}")
                )
                new_parTitl <- c(new_parTitl, list(new))
              }
            }
          }
          updatedData$stdyDscr$citation$titlStmt$parTitl <- new_parTitl
          updatedData$stdyDscr$citation$titlStmt$parTitl <- recurse_write(updatedData$stdyDscr$citation$titlStmt$parTitl)
          updatedData$stdyDscr$citation$titlStmt$parTitl <- lapply(updatedData$stdyDscr$citation$titlStmt$parTitl, function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

################################################

authors_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Authors and Contributors",
           h3("Authoring Entity"),
           rHandsontableOutput(ns("authors")),
           h3("Other Identifications/Acknowledgements"),
           rHandsontableOutput(ns("contrib")),
           tags$br(),
           actionButton(ns("save_authors"), "Save authors and contributors"),
           tags$hr(),
           p("The Authoring Entity or Primary Investigator is the person, 
           corporate body, or agency responsible for the work's substantive and 
           intellectual content. Repeat the element for each author, and use 
           'affiliation' attribute if available. Invert first and last name and 
           use commas. Author of data collection (codeBook/stdyDscr/citation/rspStmt/AuthEnty) 
           maps to Dublin Core Creator element. Inclusion of this element 
           in codebook is recommended."),
           p('Other identifications or acknowledgements are statements of 
             responsibility not recorded in the title and statement of responsibility 
             areas. Indicate here the persons or bodies connected with the work, 
             or significant persons or bodies connected with previous editions 
             and not already named in the description. For example, the name of 
             the person who edited the marked-up documentation might be cited in 
             codeBook/docDscr/rspStmt/othId, using the "role" and "affiliation" 
             attributes. Other identifications/acknowledgments for data collection 
             (codeBook/stdyDscr/citation/rspStmt/othId) maps to Dublin Core 
             Contributor element.')
           )
}

authors_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$authors <- renderRHandsontable({
      req(dat())
      authors <- tibble(
        name = character(),
        affiliation = character()
      )
      if(length(dat()$stdyDscr$citation$rspStmt$AuthEnty) == 0) {
        authors <- add_row(authors, name = NA_character_, affiliation = NA_character_)
      }
      
      for (a in dat()$stdyDscr$citation$rspStmt$AuthEnty) {
        if(is.null(a$affiliation)) a$contentType <- NA_character_
        authors <- add_row(authors, 
                           name = a$name, 
                           affiliation = a$affiliation)
      }
      rht <- rhandsontable(authors, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    output$contrib <- renderRHandsontable({
      req(dat())
      contrib <- tibble(
        name = character(),
        role = character(),
        affiliation = character()
      )
      if(length(dat()$stdyDscr$citation$rspStmt$othId) == 0) {
        contrib <- add_row(contrib, name = NA_character_, role = NA_character_, affiliation = NA_character_)
      }
      
      for (a in dat()$stdyDscr$citation$rspStmt$othId) {
        if(is.null(a$role)) a$role <- NA_character_
        if(is.null(a$affiliation)) a$affiliation <- NA_character_
        contrib <- add_row(contrib, 
                           name = a$name, 
                           role = a$role,
                           affiliation = a$affiliation)
      }
      rht <- rhandsontable(contrib, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_authors, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_authors <- hot_to_r(input$authors)
          updatedData$stdyDscr$citation$rspStmt$AuthEnty <- NULL
          new_authors <- list()
          for(i in 1:length(updated_authors$name)) {
            if(!is.na(updated_authors$name[i])) {
              if(updated_authors$name[i] != "") {
                new <- list(name = updated_authors$name[i],
                            affiliation = updated_authors$affiliation[i]
                )
                new_authors <- c(new_authors, list(new))
              }
            }
          }
          updatedData$stdyDscr$citation$rspStmt$AuthEnty <- new_authors
          updatedData$stdyDscr$citation$rspStmt$AuthEnty <- recurse_write(updatedData$stdyDscr$citation$rspStmt$AuthEnty)
          updatedData$stdyDscr$citation$rspStmt$AuthEnty <- lapply(updatedData$stdyDscr$citation$rspStmt$AuthEnty,function(x) x[!is.na(x)])
          
          updated_contrib <- hot_to_r(input$contrib)
          updatedData$stdyDscr$citation$rspStmt$othId <- NULL
          new_contrib <- list()
          for(i in 1:length(updated_contrib$name)) {
            if(!is.na(updated_contrib$name[i])) {
              if(updated_contrib$name[i] != "") {
                new <- list(name = updated_contrib$name[i],
                            role = updated_contrib$role[i],
                            affiliation = updated_contrib$affiliation[i]
                )
                new_contrib <- c(new_contrib, list(new))
              }
            }
          }
          updatedData$stdyDscr$citation$rspStmt$othId <- new_contrib
          updatedData$stdyDscr$citation$rspStmt$othId <- recurse_write(updatedData$stdyDscr$citation$rspStmt$othId)
          updatedData$stdyDscr$citation$rspStmt$othId <- lapply(updatedData$stdyDscr$citation$rspStmt$othId,function(x) x[!is.na(x)])          
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

##############################

series_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Series",
           rHandsontableOutput(ns("seriesName")),
           h3("Series Information"),
           rHandsontableOutput(ns("serInfo")),
           tags$br(),
           actionButton(ns("save_series"), "Save series information"),
           tags$hr(),
           p('The series title is the name of the series to which the work belongs.'),
           p('Series information contains a history of the series and a summary 
             of those features that apply to the series as a whole.')
           )
}

series_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$seriesName <- renderRHandsontable({
      req(dat())
      seriesName <- tibble(
        ID = character(),
        URI = character(),
        value = character(),
        abbr = character(),
        lang = character()
      )
      no_series <- FALSE
      if(length(dat()$stdyDscr$citation$serStmt) == 0) {
        seriesName <- add_row(seriesName, 
                              ID = "series1",
                              value = NA_character_)
        no_series <- TRUE
      }
      
      for (s in dat()$stdyDscr$citation$serStmt) {
        if(is.null(s$ID)) s$ID <- "series1"
        if(is.null(s$URI)) s$URI <- NA_character_
        if(length(s$serName) == 0 & !no_series) {
          seriesName <- add_row(seriesName, 
                                ID = "series1",
                                value = NA_character_)
        }
        
        for(sn in s$serName) {
          if(is.null(sn$abbr)) sn$abbr <- NA_character_
          if(is.null(sn$lang)) sn$lang <- NA_character_
          seriesName <- add_row(seriesName, 
                                ID = s$ID,
                                URI = s$URI,
                                value = sn$value,
                                abbr = sn$abbr,
                                lang = sn$lang)
        }
      }
      rht <- rhandsontable(seriesName, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(10, 20, 40, 10, 10),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    output$serInfo <- renderRHandsontable({
      req(dat())
      serInfo <- tibble(
        ID = character(),
        value = character(),
        lang = character()
      )
      no_series <- FALSE
      if(length(dat()$stdyDscr$citation$serStmt) == 0) {
        serInfo <- add_row(serInfo, 
                           ID = "series1",
                           value = NA_character_)
        no_series <- TRUE
      }
      counter <- 1
      for (s in dat()$stdyDscr$citation$serStmt) {
        if(is.null(s$ID)) s$ID <- "series1"
        
        if(length(s$serInfo) == 0 & !no_series & counter == 1) {
          serInfo <- add_row(serInfo, 
                             ID = "series1",
                             value = NA_character_)
        }
        
        for (i in s$serInfo) {
          if(is.null(i$lang)) i$lang <- NA_character_
          serInfo <- add_row(serInfo, 
                             ID = s$ID,
                             value = i$value, 
                             lang = i$lang)
        }
        counter <- counter + 1
      }
      rht <- rhandsontable(serInfo, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(10, 40, 10),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_series, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_serName <- hot_to_r(input$seriesName)
          updated_serInfo <- hot_to_r(input$serInfo)
          
          ser_name_IDs <- updated_serName %>% select(ID) %>% unique()
          ser_info_IDs <- updated_serInfo %>% select(ID) %>% unique()
          ids <- unique(append(ser_name_IDs$ID, ser_info_IDs$ID))
          
          new_serStmt <- list()
          updatedData$stdyDscr$citation$serStmt <- NULL
          
          for(id in ids) {
            name_check <- FALSE
            info_check <- FALSE
            new_serName <- list()
            new_serInfo <- list()
            filtered_serName <- updated_serName %>% filter(ID == id)
            filtered_serInfo <- updated_serInfo %>% filter(ID == id)
            URI <- filtered_serName[1,]$URI
            
            if(length(filtered_serName$value > 0)) {
              for(i in 1:length(filtered_serName$value)) {
                if(!is.na(filtered_serName$value[i])) {
                  if(filtered_serName$value[i] != "") {
                    name_check <- TRUE
                    new <- list(value = filtered_serName[i,]$value,
                                abbr = filtered_serName[i,]$abbr,
                                lang = stringr::str_extract(filtered_serName[i,]$lang, "^[a-z]{2}")
                    )
                    new_serName <- c(new_serName, list(new))
                  }  
                }
              }
            }
            
            if(length(filtered_serInfo$value) > 0) {
              for(i in 1:length(filtered_serInfo$value)) {
                if(!is.na(filtered_serInfo$value[i])) {
                  if(filtered_serInfo$value[i] != "") {
                    info_check <- TRUE
                    new <- list(value = filtered_serInfo[i,]$value,
                                lang  = stringr::str_extract(filtered_serInfo[i,]$lang, "^[a-z]{2}")
                    )
                    new_serInfo <- c(new_serInfo, list(new))
                  }
                }
              }
            }
            
            new_serName <- recurse_write(new_serName)
            new_serName <- lapply(new_serName,function(x) x[!is.na(x)])
            new_serInfo <- recurse_write(new_serInfo)
            new_serInfo <- lapply(new_serInfo,function(x) x[!is.na(x)])
            
            if(name_check & info_check) {
              new_serStmt <- c(new_serStmt, list(list(ID = id, URI = URI, serName = new_serName, serInfo = new_serInfo)))  
            } else if(name_check & !info_check) {
              new_serStmt <- c(new_serStmt, list(list(ID = id, URI = URI, serName = new_serName)))  
            } else if(!name_check & info_check) {
              new_serStmt <- c(new_serStmt, list(list(ID = id, URI = URI, serInfo = new_serInfo)))  
            }
          }
          
          updatedData$stdyDscr$citation$serStmt <- new_serStmt
          updatedData$stdyDscr$citation$serStmt <- recurse_write(updatedData$stdyDscr$citation$serStmt)
          updatedData$stdyDscr$citation$serStmt <- lapply(updatedData$stdyDscr$citation$serStmt,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

#############################

producers_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Producers",
           h3("Producers"),
           rHandsontableOutput(ns("producers")),
           h3("Place of Production"),
           rHandsontableOutput(ns("prodPlac")),
           h3("Date of Production"),
           rHandsontableOutput(ns("prodDate")),
           tags$br(),
           actionButton(ns("save_producers"), "Save producers"),
           tags$hr(),
           p('The producer is the person or organization with the financial or 
             administrative responsibility for the physical processes whereby 
             the document was brought into existence. Use the "role" attribute 
             to distinguish different stages of involvement in the production 
             process, such as original producer. Producer of data collection 
             (codeBook/stdyDscr/citation/prodStmt/producer) maps to Dublin Core 
             Publisher element.'),
           p('The Place of Production is the address of the archive or 
             organization that produced the work.'),
           p('The Date of Production is the date when the marked-up document/marked-up 
             document source/data collection/other material(s) were produced (not 
             distributed or archived). The ISO standard for dates (YYYY-MM-DD) 
             is recommended for use with the date attribute. Production date for 
             data collection (codeBook/stdyDscr/citation/prodStmt/prodDate) maps 
             to Dublin Core Date element.'))
}

producers_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$producers <- renderRHandsontable({
      req(dat())
      producers <- tibble(
        name = character(),
        abbr = character(),
        affiliation = character(),
        role = character()
      )
      if(length(dat()$stdyDscr$citation$prodStmt$producer) == 0) {
        producers <- add_row(producers, name = NA_character_)
      }
      
      for (a in dat()$stdyDscr$citation$prodStmt$producer) {
        if(is.null(a$abbr)) a$abbr <- NA_character_
        if(is.null(a$affiliation)) a$affiliation <- NA_character_
        if(is.null(a$role)) a$role <- NA_character_
        
        producers <- add_row(producers, 
                             name = a$name, 
                             abbr = a$abbr,
                             affiliation = a$affiliation,
                             role = a$role)
      }
      rht <- rhandsontable(producers, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 60, 60),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    output$prodPlac <- renderRHandsontable({
      req(dat())
      prodPlac <- tibble(
        value = character()
      )
      if(length(dat()$stdyDscr$citation$prodStmt$prodPlac) == 0) {
        prodPlac <- add_row(prodPlac, value = NA_character_)
      }
      
      for (a in dat()$stdyDscr$citation$prodStmt$prodPlac) {
        prodPlac <- add_row(prodPlac, 
                            value = a$value)
      }
      rht <- rhandsontable(prodPlac, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    output$prodDate <- renderRHandsontable({
      req(dat())
      prodDate <- tibble(
        value = character(),
        date = as.Date(""),
        lang = character()
      )
      if(length(dat()$stdyDscr$citation$prodStmt$prodDate) == 0) {
        prodDate <- add_row(prodDate, value = NA_character_)
      }
      
      for (a in dat()$stdyDscr$citation$prodStmt$prodDate) {
        if(is.null(a$date)) a$date <- NA_character_
        if(is.null(a$lang)) a$lang <- NA_character_
        prodDate <- add_row(prodDate, 
                            value = a$value,
                            date = as.Date(a$date),
                            lang = a$lang)
      }
      rht <- rhandsontable(prodDate, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang)
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_producers, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_producers <- hot_to_r(input$producers)
          updatedData$stdyDscr$citation$prodStmt$producer <- NULL
          new_producers <- list()
          for(i in 1:length(updated_producers$name)) {
            if(!is.na(updated_producers$name[i])) {
              if(updated_producers$name[i] != "") {
                new <- list(name = updated_producers$name[i],
                            abbr = updated_producers$abbr[i],
                            affiliation = updated_producers$affiliation[i],
                            role = updated_producers$role[i]
                )
                new_producers <- c(new_producers, list(new))
              }
            }
          }
          updatedData$stdyDscr$citation$prodStmt$producer <- new_producers
          updatedData$stdyDscr$citation$prodStmt$producer <- recurse_write(updatedData$stdyDscr$citation$prodStmt$producer)
          updatedData$stdyDscr$citation$prodStmt$producer <- lapply(updatedData$stdyDscr$citation$prodStmt$producer,function(x) x[!is.na(x)])
          
          updated_prodPlac <- hot_to_r(input$prodPlac)
          updatedData$stdyDscr$citation$prodStmt$prodPlac <- NULL
          new_prodPlac <- list()
          for(i in 1:length(updated_prodPlac$value)) {
            if(!is.na(updated_prodPlac$value[i])) {
              if(updated_prodPlac$value[i] != "") {
                new <- list(value = updated_prodPlac$value[i])
                new_prodPlac <- c(new_prodPlac, list(new))
              }
            }
          }
          updatedData$stdyDscr$citation$prodStmt$prodPlac <- new_prodPlac
          updatedData$stdyDscr$citation$prodStmt$prodPlac <- recurse_write(updatedData$stdyDscr$citation$prodStmt$prodPlac)
          updatedData$stdyDscr$citation$prodStmt$prodPlac <- lapply(updatedData$stdyDscr$citation$prodStmt$prodPlac,function(x) x[!is.na(x)])
          
          updated_prodDate <- hot_to_r(input$prodDate)
          updatedData$stdyDscr$citation$prodStmt$prodDate <- NULL
          new_prodDate <- list()
          for(i in 1:length(updated_prodDate$value)) {
            if(!is.na(updated_prodDate$value[i])) {
              if(updated_prodDate$value[i] != "") {
                new <- list(value = updated_prodDate$value[i],
                            date = as.character(updated_prodDate$date[i]),
                            lang = stringr::str_extract(updated_prodDate$lang, "^[a-z]{2}"))
                new_prodDate <- c(new_prodDate, list(new))
              }
            }
          }
          updatedData$stdyDscr$citation$prodStmt$prodDate <- new_prodDate
          updatedData$stdyDscr$citation$prodStmt$prodDate <- recurse_write(updatedData$stdyDscr$citation$prodStmt$prodDate)
          updatedData$stdyDscr$citation$prodStmt$prodDate <- lapply(updatedData$stdyDscr$citation$prodStmt$prodDate,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

#############################

funders_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Funding Agency",
           rHandsontableOutput(ns("funders")),
           tags$br(),
           actionButton(ns("save_funders"), "Save funders"),
           tags$hr(),
           p('The Funding Agency is the source(s) of funds for production of the 
             work. If different funding agencies sponsored different stages of the 
             production process, use the "role" attribute to distinguish them.'))
}

funders_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$funders <- renderRHandsontable({
      req(dat())
      funders <- tibble(
        name = character(),
        abbr = character(),
        role = character()
      )
      if(length(dat()$stdyDscr$citation$prodStmt$fundAg) == 0) {
        funders <- add_row(funders, name = NA_character_)
      }
      
      for (i in dat()$stdyDscr$citation$prodStmt$fundAg) {
        if(is.null(i$abbr)) i$abbr <- NA_character_
        if(is.null(i$role)) i$role <- NA_character_
        funders <- add_row(funders, 
                           name = i$name, 
                           abbr = i$abbr,
                           role = i$role)
      }
      rht <- rhandsontable(funders, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 20, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_funders, {
        isolate({
          req(dat())
          updatedData <- dat()
          
          updated_funders <- hot_to_r(input$funders)
          updatedData$stdyDscr$citation$prodStmt$fundAg <- NULL
          new_funders <- list()
          for(i in 1:length(updated_funders$name)) {
            if(!is.na(updated_funders$name[i])) {
              if(updated_funders$name[i] != "") {
                new <- list(name = updated_funders$name[i],
                            abbr = updated_funders$abbr[i],
                            role = updated_funders$role[i]
                )
                new_funders <- c(new_funders, list(new))
              }
            }
          }
          updatedData$stdyDscr$citation$prodStmt$fundAg <- new_funders
          updatedData$stdyDscr$citation$prodStmt$fundAg <- recurse_write(updatedData$stdyDscr$citation$prodStmt$fundAg)
          updatedData$stdyDscr$citation$prodStmt$fundAg <- lapply(updatedData$stdyDscr$citation$prodStmt$fundAg,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

#####################################

bibliography_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Bibliography",
           rHandsontableOutput(ns("bib")),
           tags$br(),
           actionButton(ns("save_bib"), "Save Bibliography"),
           tags$hr(),
           p('DDI allows for four types of documents in the codebook: Related Publications,
             Related Materials, Related Studies, and other references. It also allows for a description
             and/or a bibliographic citation in each element. If using the a bibliographic citation 
             you may include the citation format under format.'),
           tags$hr(),
           p('Related Publications are bibliographic and access information about articles and 
                  reports based on the data in this dataset.'),
           p("Related Materials describes materials related to the study description, such 
                  as appendices, additional information on sampling found in 
                  other documents, etc."),
           p('Related Studies are information on the relationship of the current data 
                  collection to others (e.g., predecessors, successors, other 
                  waves or rounds) or to other editions of the same file. This 
                  would include the names of additional data collections 
                  generated from the same data collection vehicle plus other 
                  collections directed at the same general topic.'),
           p('Other References indicates other pertinent references that do not fit 
             in the above categories.')
           )
}

bibliography_server <-  function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$bib <- renderRHandsontable({
      req(dat())
      bibliography <- tibble(
        publication_type = character(),
        description = character(),
        biblCit = character(),
        format = character(),
        doi = character(),
        lang = character()
      )
      
      bib_list <- c("Related Publications", "Related Materials", "Related Studies", "Other References")
      
      if(length(dat()$stdyDscr$othrStdyMat$relPubl) == 0 &
         length(dat()$stdyDscr$othrStdyMat$relMat) == 0 &
         length(dat()$stdyDscr$othrStdyMat$relStdy) == 0 &
         length(dat()$stdyDscr$othrStdyMat$othRefs) == 0) {
        bibliography <- add_row(bibliography, publication_type = "Related Publications", description = NA_character_)
      }
    
      for (s in dat()$stdyDscr$othrStdyMat$relPubl) {
        if(is.null(s$biblCit)) s$biblCit <- NA_character_
        if(is.null(s$format)) s$format <- NA_character_
        if(is.null(s$doi)) s$doi <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        bibliography <- add_row(bibliography, 
                       publication_type = "Related Publications",
                       description = s$description, 
                       biblCit = s$biblCit,
                       format = s$format,
                       doi = s$doi,
                       lang = s$lang)
      }
      
      for (s in dat()$stdyDscr$othrStdyMat$relMat) {
        if(is.null(s$biblCit)) s$biblCit <- NA_character_
        if(is.null(s$format)) s$format <- NA_character_
        if(is.null(s$doi)) s$doi <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        bibliography <- add_row(bibliography, 
                       publication_type = "Related Materials",
                       description = s$description, 
                       biblCit = s$biblCit,
                       format = s$format,
                       doi = s$doi,
                       lang = s$lang)
      }

      for (s in dat()$stdyDscr$othrStdyMat$relStdy) {
        if(is.null(s$biblCit)) s$biblCit <- NA_character_
        if(is.null(s$format)) s$format <- NA_character_
        if(is.null(s$doi)) s$doi <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        bibliography <- add_row(bibliography, 
                       publication_type = "Related Studies",
                       description = s$description, 
                       biblCit = s$biblCit,
                       format = s$format,
                       doi = s$doi,
                       lang = s$lang)
      }
      
      for (s in dat()$stdyDscr$othrStdyMat$othRefs) {
        if(is.null(s$biblCit)) s$biblCit <- NA_character_
        if(is.null(s$format)) s$format <- NA_character_
        if(is.null(s$doi)) s$doi <- NA_character_
        if(is.null(s$lang)) s$lang <- NA_character_
        bibliography <- add_row(bibliography, 
                       publication_type = "Other References",
                       description = s$description, 
                       biblCit = s$biblCit,
                       format = s$format,
                       doi = s$doi,
                       lang = s$lang)
      }
      
      
      rht <- rhandsontable(bibliography, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_col("publication_type", allowInvalid = FALSE, type = "dropdown", source = bib_list) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    
    observeEvent(
      input$save_bib, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedBib <- hot_to_r(input$bib)
          
          updatedRelPubl <- updatedBib %>% filter(publication_type == "Related Publications")
          updatedRelMat <- updatedBib %>% filter(publication_type == "Related Materials")
          updatedRelStdy <- updatedBib %>% filter(publication_type == "Related Studies")
          updatedOthRefs <- updatedBib %>% filter(publication_type == "Other References")

          updatedData$stdyDscr$othrStdyMat$relPubl <- NULL
          updatedData$stdyDscr$othrStdyMat$relMat <- NULL
          updatedData$stdyDscr$othrStdyMat$relStdy <- NULL
          updatedData$stdyDscr$othrStdyMat$othRefs <- NULL
          
          new_relPubl <- list()
          if(length(updatedRelPubl$description) > 0) {
            for(i in 1:length(updatedRelPubl$description)) {
              if(!is.na(updatedRelPubl$description[i]) & updatedRelPubl$description[i] == "") updatedRelPubl$description[i] <- NA_character_
              if(!is.na(updatedRelPubl$biblCit[i]) & updatedRelPubl$biblCit[i] == "") updatedRelPubl$biblCit[i] <- NA_character_
              
              new <- list(id = paste0("relPubl_", i),
                          description = updatedRelPubl$description[i],
                          biblCit = updatedRelPubl$biblCit[i],
                          format = updatedRelPubl$format[i],
                          doi = updatedRelPubl$doi[i],
                          lang  = stringr::str_extract(updatedRelPubl$lang[i], "^[a-z]{2}")
              )
              new_relPubl <- c(new_relPubl, list(new))
            }
          }
          
          new_relMat <- list()
          if(length(updatedRelMat$description) > 0) {
            for(i in 1:length(updatedRelMat$description)) {
              if(!is.na(updatedRelMat$description[i]) & updatedRelMat$description[i] == "") updatedRelMat$description[i] <- NA_character_
              if(!is.na(updatedRelMat$biblCit[i]) & updatedRelMat$biblCit[i] == "") updatedRelMat$biblCit[i] <- NA_character_
              
              new <- list(id = paste0("relMat_", i),
                          description = updatedRelMat$description[i],
                          biblCit = updatedRelMat$biblCit[i],
                          format = updatedRelMat$format[i],
                          doi = updatedRelMat$doi[i],
                          lang  = stringr::str_extract(updatedRelMat$lang[i], "^[a-z]{2}")
              )
              new_relMat <- c(new_relMat, list(new))
            }
          }

          new_relStdy <- list()
          if(length(updatedRelStdy$description) > 0) {
            for(i in 1:length(updatedRelStdy$description)) {
              if(!is.na(updatedRelStdy$description[i]) & updatedRelStdy$description[i] == "") updatedRelStdy$description[i] <- NA_character_
              if(!is.na(updatedRelStdy$biblCit[i]) & updatedRelStdy$biblCit[i] == "") updatedRelStdy$biblCit[i] <- NA_character_
              
              new <- list(id = paste0("relStdy_", i),
                          description = updatedRelStdy$description[i],
                          biblCit = updatedRelStdy$biblCit[i],
                          format = updatedRelStdy$format[i],
                          doi = updatedRelStdy$doi[i],
                          lang  = stringr::str_extract(updatedRelStdy$lang[i], "^[a-z]{2}")
              )
              new_relStdy <- c(new_relStdy, list(new))
            }
          }
          
          new_othRefs <- list()
          if(length(updatedOthRefs$description) > 0) {
            for(i in 1:length(updatedOthRefs$description)) {
              if(!is.na(updatedOthRefs$description[i]) & updatedOthRefs$description[i] == "") updatedOthRefs$description[i] <- NA_character_
              if(!is.na(updatedOthRefs$biblCit[i]) & updatedOthRefs$biblCit[i] == "") updatedOthRefs$biblCit[i] <- NA_character_
              
              new <- list(id = paste0("othRefs_", i),
                          description = updatedOthRefs$description[i],
                          biblCit = updatedOthRefs$biblCit[i],
                          format = updatedOthRefs$format[i],
                          doi = updatedOthRefs$doi[i],
                          lang  = stringr::str_extract(updatedOthRefs$lang[i], "^[a-z]{2}")
              )
              new_othRefs <- c(new_othRefs, list(new))
            }
          }
          
          updatedData$stdyDscr$othrStdyMat$relPubl <- new_relPubl
          updatedData$stdyDscr$othrStdyMat$relMat <- new_relMat
          updatedData$stdyDscr$othrStdyMat$relStdy <- new_relStdy
          updatedData$stdyDscr$othrStdyMat$othRefs <- new_othRefs
          
          updatedData$stdyDscr$othrStdyMat$relPubl <- recurse_write(updatedData$stdyDscr$othrStdyMat$relPubl)
          updatedData$stdyDscr$othrStdyMat$relMat <- recurse_write(updatedData$stdyDscr$othrStdyMat$relMat)
          updatedData$stdyDscr$othrStdyMat$relStdy <- recurse_write(updatedData$stdyDscr$othrStdyMat$relStdy)
          updatedData$stdyDscr$othrStdyMat$othRefs <- recurse_write(updatedData$stdyDscr$othrStdyMat$othRefs)
          
          updatedData$stdyDscr$othrStdyMat$relPubl <- lapply(updatedData$stdyDscr$othrStdyMat$relPubl,function(x) x[!is.na(x)])
          updatedData$stdyDscr$othrStdyMat$relMat <- lapply(updatedData$stdyDscr$othrStdyMat$relMat,function(x) x[!is.na(x)])
          updatedData$stdyDscr$othrStdyMat$relStdy <- lapply(updatedData$stdyDscr$othrStdyMat$relStdy,function(x) x[!is.na(x)])
          updatedData$stdyDscr$othrStdyMat$othRefs <- lapply(updatedData$stdyDscr$othrStdyMat$othRefs,function(x) x[!is.na(x)])
                  
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

