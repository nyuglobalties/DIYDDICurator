varGrp_label_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Labels",
           p("This section is list the variable groups in the project."),
           tags$hr(),
           h3("Labels"),
           rHandsontableOutput(ns("varGrp_label")),
           tags$br(),
           p("A labl is a short description of the parent element."),
           actionButton(ns("save_varGrp"), "Save Variable Group Labels")
          )
}

varGrp_defntn_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Definitions",
           rHandsontableOutput(ns("varGrp_defntn")),
           tags$br(),
           p("A labl is the rationale for why the group was constituted in 
             this way."),
           actionButton(ns("save_varGrp"), "Save Variable Group Definitions")
  )
}

varGrp_universe_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Universes",
           rHandsontableOutput(ns("varGrp_universe")),
           tags$br(),
           p('The group of persons or other elements that are the object of 
             research and to which any analytic results refer. Age,nationality, 
             and residence commonly help to delineate a given universe, but any 
             of a number of factors may be involved, such as sex, race, income, 
             veteran status, criminal convictions, etc. The universe may consist 
             of elements other than persons, such as housing units, court cases, 
             deaths, countries, etc. In general, it should be possible to tell 
             from the description of the universe whether a given individual or 
             element (hypothetical or real) is a member of the population under 
             study. The "clusion" attribute provides for specification of groups 
             included (I) in or excluded (E) from the universe.'),
           actionButton(ns("save_varGrp"), "Save Variable Group Universes")
  )
}

varGrp_concept_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Concepts",
           rHandsontableOutput(ns("varGrp_concept")),
           tags$br(),
           p('The general subject to which the parent element may be seen as 
             pertaining. This element serves the same purpose as the keywords 
             and topic classification elements, but at the data description 
             level. The "vocab" attribute is provided to indicate the controlled 
             \vocabulary, if any, used in the element, e.g., LCSH (Library of 
             Congress Subject Headings), MeSH (Medical Subject Headings), etc. 
             The "vocabURI" attribute specifies the location for the full 
             controlled vocabulary.'),
           actionButton(ns("save_varGrp"), "Save Variable Group Concepts")
  )
}

varGrp_hierarchy_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Hierarchy",
           rHandsontableOutput(ns("varGrp_hierarchy")),
           tags$br(),
           p('The "varGrp" attribute is used to reference all the subsidiary 
             variable groups which nest underneath the current varGrp. This allows 
             for encoding of a hierarchical structure of variable groups.'),
           actionButton(ns("save_varGrp"), "Save Variable Group Hierarchy"),
           tags$hr(),
           p('Hierarchy visualization'),
           verbatimTextOutput(ns("hierarch_viz"))
  )
}

varGrp_type_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Type",
           rHandsontableOutput(ns("varGrp_type")),
           p(),
           actionButton(ns("save_varGrp"), "Save Variable Group Types"),
           tags$hr(),
           p('The "type" attribute refers to the general type of grouping of the 
           variables, e.g., subject, multiple response. Use the value of "other" 
           if the value is to come from an external controlled vocabulary, and 
           place the term into the otherType attribute.'),
           p('The "otherType" attribute is used when the "type" attribute has a 
           value of "other". This option should only be used when applying a 
           controlled vocabulary to this attribute. Use the complex element 
           controlledVocabUsed to identify the controlled vocabulary to which 
           the selected term belongs.'),
           p('Specific variable groups, included within the "type" attribute, are:'),
           tags$ul(
             tags$li('Section: Questions which derive from the same section of the 
           questionnaire, e.g., all variables located in Section C.'),
             tags$li('Multiple response: Questions where the respondent has the opportunity 
           to select more than one answer from a variety of choices, e.g., what 
           newspapers have you read in the past month (with the respondent able 
           to select up to five choices).'),
             tags$li('Grid: Sub-questions of an introductory or main question but which do 
           not constitute a multiple response group, e.g., I am going to read 
           you some events in the news lately and you tell me for each one 
           whether you are very interested in the event, fairly interested in 
           the fact, or not interested in the event.'),
             tags$li('Display: Questions which appear on the same interview screen (CAI) 
           together or are presented to the interviewer or respondent as a group.'),
             tags$li('Repetition: The same variable (or group of variables) which are 
           repeated for different groups of respondents or for the same 
           respondent at a different time.'),
             tags$li('Subject: Questions which address a common topic or subject, e.g., 
           income, poverty, children.'),
             tags$li('Version: Variables, often appearing in pairs, which represent 
           different aspects of the same question, e.g., pairs of variables 
           (or groups) which are adjusted/unadjusted for inflation or season or 
           whatever, pairs of variables with/without missing data imputed, and 
           versions of the same basic question.'),
             tags$li('Iteration: Questions that appear in different sections of the data 
           file measuring a common subject in different ways, e.g., a set of 
           variables which report the progression of respondent income over the 
           life course.'),
             tags$li('Analysis: Variables combined into the same index, e.g., the 
             components of a calculation, such as the numerator and the 
             denominator of an economic statistic.'),
             tags$li('Pragmatic: A variable group without shared properties.'),
             tags$li('Record: Variable from a single record in a hierarchical file.'),
             tags$li('File: Variable from a single file in a multifile study.'),
             tags$li('Randomized: Variables generated by CAI surveys produced by one or 
             more random number variables together with a response variable, e.g, 
             random variable X which could equal 1 or 2 (at random) which in turn 
             would control whether Q.23 is worded "men" or "women", e.g., would 
             you favor helping [men/women] laid off from a factory obtain training 
             for a new job?'),
             tags$li('Other: Variables which do not fit easily into any of the categories 
           listed above, e.g., a group of variables whose documentation is in 
           another language.')
           )
  )
}

varGrp_label_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$varGrp_label <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        label = character(),
        lang = character()
      )
      
      if(length(dat()$dataDscr$varGrp) == 0) {
        varGrp <- add_row(varGrp, name = "varGrp1")
      }
      
      for (vg in dat()$dataDscr$varGrp) {
        if(is.null(vg$name)) vg$name <- NA_character_
        for (l in vg$labl) {
          if(is.null(l$lang)) l$lang <- NA_character_
          varGrp <- add_row(varGrp,
                            name = vg$name,
                            label = l$value,
                            lang = l$lang)
        }
      }
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    observeEvent(
      input$save_varGrp, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_varGrp <- hot_to_r(input$varGrp_label)
          varGrpList <- unique(updated_varGrp$name)
          existing_varGrps <- list()
          
          for(vg in dat()$dataDscr$varGrp) {
            existing_varGrps <- append(existing_varGrps, vg$name)
          }
          
          # remove previously existing vars that have been removed from app
          for(i in 1:length(existing_varGrps)) {
            if(existing_varGrps[[i]] %in% varGrpList) {
            } else {
              updatedData$dataDscr$varGrp <- lapply(1:length(updatedData$dataDscr$varGrp), 
                                                      function(x) updatedData$dataDscr$varGrp[[x]][updatedData$dataDscr$varGrp[[x]]$name != existing_varGrps[[i]]])
              updatedData$dataDscr$varGrp <- updatedData$dataDscr$varGrp[lengths(updatedData$dataDscr$varGrp) > 0] 
            }
          }
          
          for(vg in varGrpList) {
            new_df <- updated_varGrp %>% filter(name == vg)
            new_l <- list()
            name <- vg 
            if(length(new_df$label) > 0) {
              for(l in 1:length(new_df$label)) {
                if(!is.na(new_df$label[l])) {
                  if(new_df$label[l] != "") {
                    labl <- list(value = new_df$label[l], 
                                 lang = stringr::str_extract(new_df$lang[l], "^[a-z]{2}"), 
                                 level = "varGrp")
                    new_l <- c(new_l, list(labl))
                  }
                }
              }
              new_l <- recurse_write(new_l)
              new_l <- lapply(new_l,function(x) x[!is.na(x)])
            }
            
            # need something for new varGrps
            if(vg %in% existing_varGrps) {
              #only write over labels
              for(i in 1:length(updatedData$dataDscr$varGrp)) {
                if(vg == updatedData$dataDscr$varGrp[[i]]$name) {
                  updatedData$dataDscr$varGrp[[i]]$labl <- new_l
                }
              }
            } else {
              updatedData$dataDscr$varGrp <- append(updatedData$dataDscr$varGrp, 
                                                    list(list(name = vg,
                                                      labl = new_l)))
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

varGrp_type_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$varGrp_type <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        type = character(),
        otherType = character()
      )
      
      type_list <- c("", "section", "multipleResp", "grid", "display", "repetition",
                     "subject", "version", "iteration", "analysis", "pragmatic",
                     "record", "file", "randomized", "other")
      # get a list of all varGrps...
      name_list <- list()
      for(i in 1:length(dat()$dataDscr$varGrp)) {
        name_list <- append(name_list, dat()$dataDscr$varGrp[[i]]$name)
      }
      
      for (vg in dat()$dataDscr$varGrp) {
        if(is.null(vg$type)) vg$type <- NA_character_
        if(is.null(vg$otherType)) vg$otherType <- NA_character_
        varGrp <- add_row(varGrp,
                          name = vg$name,
                          type = vg$type,
                          otherType = vg$otherType)
      }
      
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 60),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("name", allowInvalid = FALSE, readOnly = TRUE) %>% 
        hot_col("type", allowInvalid = FALSE, type = "dropdown", source = type_list) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    observeEvent(
      input$save_varGrp, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_varGrp <- hot_to_r(input$varGrp_type)
          varGrpList <- unique(updated_varGrp$name)
          
          for(vg in varGrpList) {
            new_df <- updated_varGrp %>% filter(name == vg)
            name <- vg 
            if(!is.na(new_df$type) & new_df$type == "") new_df$type <- NA_character_
            if(!is.na(new_df$type)) {
              type <- new_df$type 
              if(type == "other") {
                otherType = new_df$otherType
              } else {
                otherType <- NA_character_
              }
            } else {
              type <- NA_character_
              otherType <- NA_character_
            }
            #only write over type
            for(i in 1:length(updatedData$dataDscr$varGrp)) {
              if(vg == updatedData$dataDscr$varGrp[[i]]$name) {
                if(!is.na(type)) {
                  updatedData$dataDscr$varGrp[[i]]$type <- type
                } else {
                  updatedData$dataDscr$varGrp[[i]]$type <- NULL
                }
                if(!is.na(type) & type == "other") {
                  if(otherType == '') otherType <- NA_character_
                  if(!is.na(otherType)) {
                    updatedData$dataDscr$varGrp[[i]]$otherType = otherType
                  } else {
                    updatedData$dataDscr$varGrp[[i]]$otherType = NULL
                  }
                } else {
                  updatedData$dataDscr$varGrp[[i]]$otherType = NULL
                }
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

varGrp_hierarchy_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$varGrp_hierarchy <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        parent = character(),
        child = character()
      )
      
      # get a list of all varGrps...
      name_list <- list()
      for(i in 1:length(dat()$dataDscr$varGrp)) {
        name_list <- append(name_list, dat()$dataDscr$varGrp[[i]]$name)
      }
      
      # make child list
      child_list <- c("")
      child_list <- append(child_list, name_list)
      
      # check that there is a hierarchy
      hierarchy_exist <- FALSE
      for(vg in dat()$dataDscr$varGrp) {
        if(length(vg$varGrp) > 0) hierarchy_exist = TRUE
      }
      
      if(!hierarchy_exist) {
        varGrp <- add_row(varGrp, parent = name_list[[1]])
      }
      
      for (vg in dat()$dataDscr$varGrp) {
        if(is.null(vg$varGrp)) vg$varGrp <- NA_character_
        
        if(!is.na(vg$varGrp)) {
          split_vg <- strsplit(vg$varGrp, " ")
          for(x in split_vg) {
            varGrp <- add_row(varGrp,
                              parent = vg$name,
                              child = x)
          }
        }
      }
      
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("parent", allowInvalid = FALSE, type = "dropdown", source = name_list) %>% 
        hot_col("child", allowInvalid = FALSE, type = "dropdown", source = child_list) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })  
    
    output$hierarch_viz <- renderPrint({
      req(dat())
      varGrp <- tibble(
        parent = character(),
        child = character()
      )
      first_level <- tibble(
        parent = character(),
        child = character()
      )
      
      # put all varGrps under study - two dataframes?
      
      for(vg in dat()$dataDscr$varGrp) {
        first_level <- add_row(first_level,
                               parent = "study",
                               child = vg$name)
      }

      # as a varGrp becomes a child filter out the ones under the study
      for (vg in dat()$dataDscr$varGrp) {
        if(is.null(vg$varGrp)) vg$varGrp <- NA_character_
        
        if(!is.na(vg$varGrp)) {
          split_vg <- strsplit(vg$varGrp, " ")
          for(i in 1:lengths(split_vg)) {
            varGrp <- add_row(varGrp,
                              parent = vg$name,
                              child = split_vg[[1]][i])
            # remove x from first_level
            first_level <- first_level %>% filter(child != split_vg[[1]][i])
          }
        }
      }
      varGrp <- bind_rows(first_level, varGrp)
      print(FromDataFrameNetwork(varGrp))
    })
    
    
    observeEvent(
      input$save_varGrp, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_varGrp <- hot_to_r(input$varGrp_hierarchy)
          varGrpList <- unique(updated_varGrp$parent)

          for(i in 1:length(updatedData$dataDscr$varGrp)) {
            updatedData$dataDscr$varGrp[[i]]$varGrp <- NULL
          }
          
          for(vg in varGrpList) {
            new_df <- updated_varGrp %>% filter(parent == vg)

            # check to make sure a child isn't = to the parent
            child_list <- list()
 
            for(child in new_df$child) {
              if(child != vg) {
                child_list <- append(child_list, child)
              }
            }
            # take the list or vector and convert it into a string
            varGrp <- paste(child_list, collapse = " ")
            
            #only write over hierarchy
            for(i in 1:length(updatedData$dataDscr$varGrp)) {
              if(vg == updatedData$dataDscr$varGrp[[i]]$name) {
                if(!is.na(varGrp)) {
                  updatedData$dataDscr$varGrp[[i]]$varGrp <- varGrp
                } else {
                  updatedData$dataDscr$varGrp[[i]]$varGrp <- NULL
                }
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}


varGrp_defntn_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$varGrp_defntn <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        defntn = character(),
        lang = character()
      )
      
      # get a list of all varGrps...
      name_list <- list()
      for(i in 1:length(dat()$dataDscr$varGrp)) {
        name_list <- append(name_list, dat()$dataDscr$varGrp[[i]]$name)
      }
      
      defntn_exist <- FALSE
      for(vg in dat()$dataDscr$varGrp) {
        if(length(vg$defntn) > 0) defntn_exist = TRUE
      }
      
      if(!defntn_exist) {
        varGrp <- add_row(varGrp, name = name_list[[1]])
      }
      
      for (vg in dat()$dataDscr$varGrp) {
        for (d in vg$defntn) {
          if(is.null(d$lang)) d$lang <- NA_character_
          varGrp <- add_row(varGrp,
                            name = vg$name,
                            defntn = d$value,
                            lang = d$lang)
        }
      }
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("name", allowInvalid = FALSE, type = "dropdown", source = name_list) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    observeEvent(
      input$save_varGrp, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_varGrp <- hot_to_r(input$varGrp_defntn)
          new_varGrp <- list()
          varGrpList <- unique(updated_varGrp$name)
          
          for(vg in varGrpList) {
            new_df <- updated_varGrp %>% filter(name == vg)
            new_d <- list()
            name <- vg 
            if(length(new_df$defntn) > 0) {
              for(l in 1:length(new_df$defntn)) {
                if(!is.na(new_df$defntn[l])) {
                  if(new_df$defntn[l] != "") {
                    defntn <- list(value = new_df$defntn[l], 
                                 lang = stringr::str_extract(new_df$lang[l], "^[a-z]{2}"), 
                                 level = "varGrp")
                    new_d <- c(new_d, list(defntn))
                  }
                }
              }
              new_d <- recurse_write(new_d)
              new_d <- lapply(new_d,function(x) x[!is.na(x)])
            }
            
            # write only over definitions
            for(i in 1:length(updatedData$dataDscr$varGrp)) {
              if(updatedData$dataDscr$varGrp[[i]]$name %in% varGrpList) {
                if(vg == updatedData$dataDscr$varGrp[[i]]$name) {
                  if(length(new_d) > 0) {
                    updatedData$dataDscr$varGrp[[i]]$defntn <- new_d
                  } else {
                    updatedData$dataDscr$varGrp[[i]]$defntn <- NULL
                  }
                }
              } else {
                updatedData$dataDscr$varGrp[[i]]$defntn <- NULL
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

varGrp_concept_server <-function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    output$varGrp_concept <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        term = character(),
        vocabu = character(),
        vocab_URI = character(),
        lang = character()
      )

      # gotta get a list of all varGrps...
      name_list <- list()
      for(i in 1:length(dat()$dataDscr$varGrp)) {
        name_list <- append(name_list, dat()$dataDscr$varGrp[[i]]$name)
      }
      
      concept_exist <- FALSE
      for(vg in dat()$dataDscr$varGrp) {
        if(length(vg$concept) > 0) concept_exist = TRUE
      }
      
      if(!concept_exist) {
        varGrp <- add_row(varGrp, name = name_list[[1]])
      }
      
      for (vg in dat()$dataDscr$varGrp) {
        name <- vg$name
        for (con in vg$concept) {
          if(is.null(con$vocabu)) con$vocabu <- NA_character_
          if(is.null(con$vocab_URI)) con$vocab_URI <- NA_character_
          if(is.null(con$lang)) con$lang <- NA_character_
          
          varGrp <- add_row(varGrp,
                            name = name,
                            term = con$value,
                            vocabu = con$vocabu,
                            vocab_URI = con$vocab_URI,
                            lang = con$lang)
        }
      }
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20, 20, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("name", allowInvalid = FALSE, type = "dropdown", source = name_list) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_varGrp, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_varGrp <- hot_to_r(input$varGrp_concept)
          new_varGrp <- list()
          varGrpList <- unique(updated_varGrp$name)
          
          for(vg in varGrpList) {
            new_df <- updated_varGrp %>% filter(name == vg)
            new_c <- list()
            name <- vg 
            for(l in 1:length(new_df$term)) {
              if(!is.na(new_df$term[l])) {
                if(new_df$term[l] != "") {
                  concept <- list(value = new_df$term[l],
                                  vocabu = new_df$vocabu[l],
                                  vocab_URI = new_df$vocab_URI[l],
                                  lang = stringr::str_extract(new_df$lang[l], "^[a-z]{2}") 
                                  )
                  new_c <- c(new_c, list(concept))
                }
              }
            }
            new_c <- recurse_write(new_c)
            new_c <- lapply(new_c,function(x) x[!is.na(x)])
            
            
            # write only over concepts
            for(i in 1:length(updatedData$dataDscr$varGrp)) {
              if(updatedData$dataDscr$varGrp[[i]]$name %in% varGrpList) {
                if(vg == updatedData$dataDscr$varGrp[[i]]$name) {
                  if(length(new_c) > 0) {
                    updatedData$dataDscr$varGrp[[i]]$concept <- new_c
                  } else {
                    updatedData$dataDscr$varGrp[[i]]$concept <- NULL
                  }
                }
              } else {
                updatedData$dataDscr$varGrp[[i]]$concept <- NULL
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

varGrp_universe_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$varGrp_universe <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        universe = character(),
        clusion = character(),
        lang = character()
      )
      clusion_list <- c("Included", "Excluded")
      
      # gotta get a list of all varGrps...
      name_list <- list()
      for(i in 1:length(dat()$dataDscr$varGrp)) {
        name_list <- append(name_list, dat()$dataDscr$varGrp[[i]]$name)
      }
      
      universe_exist <- FALSE
      for(vg in dat()$dataDscr$varGrp) {
        if(length(vg$defntn) > 0) universe_exist = TRUE
      }
      
      if(!universe_exist) {
        varGrp <- add_row(varGrp, name = name_list[[1]])
      }
      
      for (vg in dat()$dataDscr$varGrp) {
        for (u in vg$universe) {
          if(is.null(u$lang)) u$lang <- NA_character_
          varGrp <- add_row(varGrp,
                            name = vg$name,
                            universe = u$group,
                            clusion = u$clusion,
                            lang = u$lang)
        }
      }
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("name", allowInvalid = FALSE, type = "dropdown", source = name_list) %>% 
        hot_col("clusion", allowInvalid = FALSE, type = "dropdown", source = clusion_list) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    observeEvent(
      input$save_varGrp, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_varGrp <- hot_to_r(input$varGrp_universe)
          new_varGrp <- list()
          varGrpList <- unique(updated_varGrp$name)
          
          for(vg in varGrpList) {
            new_df <- updated_varGrp %>% filter(name == vg)
            new_u <- list()
            name <- vg 
            if(length(new_df$universe) > 0) {
              for(l in 1:length(new_df$universe)) {
                if(!is.na(new_df$universe[l])) {
                  if(new_df$universe[l] != "") {
                    if(is.na(new_df$clusion[l])) new_df$clusion[l] <- "I"
                    universe <- list(group = new_df$universe[l], 
                                   lang = stringr::str_extract(new_df$lang[l], "^[a-z]{2}"), 
                                   clusion = stringr::str_extract(new_df$clusion[l], "^[A-Z]{1}"),
                                   level = "varGrp")
                    new_u <- c(new_u, list(universe))
                  }
                }
              }
              new_u <- recurse_write(new_u)
              new_u <- lapply(new_u,function(x) x[!is.na(x)])
            }
            
            # write only over universes
            for(i in 1:length(updatedData$dataDscr$varGrp)) {
              if(updatedData$dataDscr$varGrp[[i]]$name %in% varGrpList) {
                if(vg == updatedData$dataDscr$varGrp[[i]]$name) {
                  if(length(new_u) > 0) {
                    updatedData$dataDscr$varGrp[[i]]$universe <- new_u
                  } else {
                    updatedData$dataDscr$varGrp[[i]]$universe <- NULL
                  }
                }
              } else {
                updatedData$dataDscr$varGrp[[i]]$universe <- NULL
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}