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

abstract_server <- function(id, dat, filepth, lang) {
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
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
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
                            lang  = stringr::str_extract(updatedAbstracts$lang[i], "^[a-z]{2}")
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

#####################

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

##########################

universe_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Universe",
           rHandsontableOutput(ns("universe")),
           tags$br(),
           actionButton(ns("saveUniverse"), "Save universe"),
           tags$hr(),
           tags$p('The group of persons or other elements that are the object 
                    of research and to which any analytic results refer. Age, 
                    nationality, and residence commonly help to delineate a 
                    given universe, but any of a number of factors may be 
                    involved, such as sex, race, income, veteran status, 
                    criminal convictions, etc. The universe may consist of 
                    elements other than persons, such as housing units, court 
                    cases, deaths, countries, etc. In general, it should be 
                    possible to tell from the description of the universe 
                    whether a given individual or element (hypothetical or real) 
                    is a member of the population under study.'),
           tags$p('The "clusion" attribute provides for specification of groups 
                    included (I) in or excluded (E) from the universe. This 
                    element may be repeated only to support multiple language 
                    expressions of the content.')
  )
}

universe_server <-  function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$universe <- renderRHandsontable({
      req(dat())
      clusionOptions <- c(NA_character_, "I", "E")
      universe <- tibble(
        group = character(),
        clusion = factor(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$universe) == 0) {
        universe <- add_row(universe, group = NA_character_)
      }
      for (u in dat()$stdyDscr$stdyInfo$sumDscr$universe) {
        if(is.null(u$lang)) u$lang <- NA_character_
        universe <- add_row(universe, 
                            group = u$group, 
                            clusion = u$clusion,
                            lang = u$lang)
      }
      rht <- rhandsontable(universe, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(30, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("clusion", allowInvalid = FALSE, type = "dropdown", source = clusionOptions) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$saveUniverse, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedUniverse<- hot_to_r(input$universe)
          updatedData$stdyDscr$stdyInfo$sumDscr$universe <- NULL
          newUniverse <- list()
          for(i in 1:length(updatedUniverse$group)) {
            if(!is.na(updatedUniverse$group[i])) {
              if(updatedUniverse$group[i] != "") {
                new <- list(group = updatedUniverse$group[i],
                            level = "project",
                            clusion = updatedUniverse$clusion[i],
                            lang  = stringr::str_extract(updatedUniverse$lang[i], "^[a-z]{2}")
                )
                newUniverse <- c(newUniverse, list(new))
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$universe <- newUniverse
          updatedData$stdyDscr$stdyInfo$sumDscr$universe <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$universe)
          updatedData$stdyDscr$stdyInfo$sumDscr$universe <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$universe,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

#######################

anlyUnit_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Unit of Analysis",
           rHandsontableOutput(ns("anlyUnit")),
           tags$br(),
           actionButton(ns("save_anlyUnit"), "Save units of analysis"),
           tags$hr(),
           p("Definition: Basic unit of analysis or observation that the file 
               describes: individuals, families/households, groups, 
               institutions/organizations, administrative units, etc.")
  )
}

anlyUnit_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$anlyUnit <- renderRHandsontable({
      req(dat())
      anlyUnit <- tibble(
        group = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) == 0) {
        anlyUnit <- add_row(anlyUnit, group = NA_character_, lang = "")
      }
      
      for (a in dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) {
        if(is.null(a$lang)) a$lang <- NA_character_
        anlyUnit <- add_row(anlyUnit, 
                            group = a$group, 
                            lang = a$lang)
      }
      rht <- rhandsontable(anlyUnit, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_anlyUnit, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_anlyUnit <- hot_to_r(input$anlyUnit)
          updatedData$stdyDscr$sumDscr$anlyUnit <- NULL
          new_anlyUnit <- list()
          for(i in 1:length(updated_anlyUnit$group)) {
            if(!is.na(updated_anlyUnit$group[i])) {
              if(updated_anlyUnit$group[i] != "") {
                new <- list(group = updated_anlyUnit$group[i],
                            lang  = stringr::str_extract(updated_anlyUnit$lang[i], "^[a-z]{2}")
                )
                new_anlyUnit <- c(new_anlyUnit, list(new))
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit <- new_anlyUnit
          updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit)
          updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

#########################

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

####################

geography_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Geography",
           rHandsontableOutput(ns("geog")),
           tags$br(),
           actionButton(ns("saveGeog"), "Save geographic elements"),
           tags$hr(),
           p('nation definition: Indicates the country or countries covered in 
               the file. Attribute "abbr" may be used to list common 
               abbreviations; use of ISO country codes is recommended. Maps to 
               Dublin Core Coverage element. Inclusion of this element is recommended.'),
           p('geogCover definition: Information on the geographic coverage of 
               the data. Includes the total geographic scope of the data, and 
               any additional levels of geographic coding provided in the 
               variables. Maps to Dublin Core Coverage element. Inclusion of 
               this element in the codebook is recommended.'),
           p('geogUnit definition: Lowest level of geographic aggregation 
               covered by the data.')
  )
}

geography_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$geog <- renderRHandsontable({
      req(dat())
      fieldOptions <- c("nation", "geogCover", "geogUnit")
      geog <- tibble(
        field = factor(),
        value = character(),
        abbr = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$nation) == 0 &
         length(dat()$stdyDscr$stdyInfo$sumDscr$geogCover) == 0 &
         length(dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) == 0) {
        geog <- add_row(geog, field = "nation", value = NA_character_, abbr = "", lang = "")
      }
      
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$nation) {
        if(is.null(v$abbr)) v$abbr <- NA_character_
        if(is.null(v$lang)) v$lang <- NA_character_
        geog <- add_row(geog,
                        field = "nation",
                        value = v$value,
                        abbr = v$abbr,
                        lang = v$lang
        )
      }
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$geogCover) {
        if(is.null(v$lang)) v$lang <- NA_character_
        geog <- add_row(geog,
                        field = "geogCover",
                        value = v$value, 
                        abbr = NA,
                        lang = v$lang
        )
      }
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) {
        if(is.null(v$lang)) v$lang <- NA_character_
        geog <- add_row(geog,
                        field = "geogUnit",
                        value = v$value, 
                        abbr = NA,
                        lang = v$lang
        )
      }
      rht <- rhandsontable(geog, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("field", allowInvalid = FALSE, type = "dropdown", source = fieldOptions) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$saveGeog, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_geog <- hot_to_r(input$geog)
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- NULL
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- NULL
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- NULL
          new_nation <- list()
          new_geogCover <- list()
          new_geogUnit <- list()
          for(i in 1:length(updated_geog$field)) {
            if(!is.na(updated_geog$value[i])) {
              if(updated_geog$value[i] != "") {
                if(updated_geog$field[i] == "nation") {
                  new_n <- list(value = updated_geog$value[i],
                                abbr = updated_geog$abbr[i],
                                lang = stringr::str_extract(updated_geog$lang[i], "^[a-z]{2}")
                  )
                  new_nation <- c(new_nation, list(new_n))
                } else if(updated_geog$field[i] == "geogCover"){
                  new_gc <- list(value = updated_geog$value[i],
                                 lang = stringr::str_extract(updated_geog$lang[i], "^[a-z]{2}")
                  )
                  new_geogCover <- c(new_geogCover, list(new_gc))
                } else {
                  new_gu <- list(value = updated_geog$value[i],
                                 lang = stringr::str_extract(updated_geog$lang[i], "^[a-z]{2}")
                  )
                  new_geogUnit <- c(new_geogUnit, list(new_gu))
                }
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- new_nation
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$nation)
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$nation,function(x) x[!is.na(x)])
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- new_geogCover
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$geogCover)
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$geogCover,function(x) x[!is.na(x)])
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- new_geogUnit
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit)
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

######################

timePeriods_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Time periods",
           rHandsontableOutput(ns("timePrd")),
           tags$br(),
           actionButton(ns("save_timePrd"), "Save time periods"),
           tags$hr(),
           p('timePrd Definition: The time period to which the data refer. 
               This item reflects the time period covered by the data, not the 
               dates of coding or making documents machine-readable or the dates 
               the data were collected. Also known as span. Use the event 
               attribute to specify "start", "end", or "single" for each date 
               entered. The "cycle" attribute permits specification of the 
               relevant cycle, wave, or round of data. Maps to Dublin Core 
               Coverage element. Inclusion of this element is recommended.'),
  )
}

timePeriods_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$timePrd <- renderRHandsontable({
      req(dat())
      eventOptions <- c(NA_character_, "start", "end", "single")
      prds <- tibble(
        value = character(),
        date = as.Date(""),
        event = character(),
        cycle = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$timePrd) == 0) {
        prds <- add_row(prds, value = NA_character_)
      }
      
      for (tp in dat()$stdyDscr$stdyInfo$sumDscr$timePrd) {
        if(is.null(tp$date)) tp$date <- NA_character_
        if(is.null(tp$event)) tp$event <- NA_character_
        if(is.null(tp$cycle)) tp$cycle <- NA_character_
        if(is.null(tp$lang)) tp$lang <- NA_character_
        prds <- add_row(prds,
                        value = tp$value,
                        date = as.Date(tp$date), 
                        event = tp$event,
                        cycle = tp$cycle,
                        lang = tp$lang
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
      input$save_timePrd, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_prds <- hot_to_r(input$timePrd)
          updated_prds$event[is.na(updated_prds$event) | updated_prds$event == "" ] <- "single"
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- NULL
          new_timePrd <- list()
          for(i in 1:length(updated_prds$value)) {
            if(!is.na(updated_prds$value[i])) {
              if(updated_prds$value[i] != "") {
                new_tp <- list(value = updated_prds$value[i],
                               date = as.character(updated_prds$date[i]),
                               event = updated_prds$event[i],
                               cycle = updated_prds$cycle[i],
                               lang = stringr::str_extract(updated_prds$lang[i], "^[a-z]{2}")
                )
                new_timePrd <- c(new_timePrd, list(new_tp))
              }
            }
          } 
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- new_timePrd
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$timePrd)
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$timePrd,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}