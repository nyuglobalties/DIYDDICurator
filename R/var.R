var_label_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Labels", 
           rHandsontableOutput(ns("var")),
           tags$br(),
           actionButton(ns("saveVars"), "Save variables"),
           tags$hr(),
           tags$p('This element describes all of the features of a single 
                  variable in a social science data file. The following elements 
                  are repeatable to support multi-language content: anlysUnit, 
                  embargo, imputation, respUnit, security, TotlResp. The attribute 
                  "name" usually contains the so-called "short label" for the 
                  variable, limited to eight characters in many statistical 
                  analysis systems such as SAS or SPSS. ') 
  )
}

var_characteristics_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Characteristics", 
           rHandsontableOutput(ns("varCharacteristics")),
           tags$br(),
           actionButton(ns("saveVars"), "Save variable characteristics"),
           tags$hr(),
           tags$p('The "nature" attribute records the nature of the variable, 
                  whether it is "nominal", "ordinal", "interval", "ratio", or 
                  "percent".'),
           tags$p('The attribute "temporal" indicates whether the variable relays 
                  time-related information.'),
           tags$p('The "geog" attribute indicates whether the variable relays 
                  geographic information.')
  )
}

var_varGrpAssign_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Variable Group Assignment", 
           rHandsontableOutput(ns("var_varGrps")),
           tags$br(),
           actionButton(ns("saveVars"), "Save variable group assignment"),
           tags$hr(),
           tags$p('This is to assign a reference for the constituent variable 
                  IDs to thet right variable group.')
  )
}


var_respondent_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Respondent information", 
           rHandsontableOutput(ns("varRespondents")),
           tags$br(),
           actionButton(ns("saveVars"), "Save variable respondents"),
           tags$hr(),
           tags$p('The response unit provides information regarding who provided 
                  the information contained within the variable, e.g., respondent, 
                  proxy, interviewer. This element may be repeated only to support 
                  multiple language expressions of the content.'),
           tags$p('The analysis unit provides information regarding whom or what 
                  the variable describes. The element may be repeated only to 
                  support multiple language expressions of the content.')
  )
}

var_security_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Security and Embargo", 
           tags$h4("Security"),
           rHandsontableOutput(ns("varSecurity")),
           tags$br(),
           tags$h4("Embargo"),
           rHandsontableOutput(ns("varEmbargo")),
           tags$br(),
           actionButton(ns("saveVars"), "Save security and embargo information"),
           tags$hr(),
           tags$p('The security and embargo elements are used to temporarily or
                  permanently restrict levels of access, e.g., public, subscriber, 
                  need to know. to variables. An embargo provides information on 
                  variables which are not currently available because of policies 
                  established by the principal investigators and/or data producers. 
                  These elements may be repeated to support multiple language 
                  expressions of the content.')
  )
}

var_catgry_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Variable Categories", 
          tags$h4("Category Sets"),
           uiOutput(ns("selector")),
           textInput(ns("new_catgry_set_name"), "Enter a new category set name and check below box to create a new set"),
           checkboxInput(ns("createNew"), "Create new catgry set"),
           rHandsontableOutput(ns("category_sets")),
           tags$br(),
           actionButton(ns("saveCatgryChanges"), "Save category set changes"),
           tags$hr(),
           tags$h4("Assign catgry set to categorical variables (nominal & ordinal)"),
           rHandsontableOutput(ns("categories")),
           tags$br(),
           actionButton(ns("saveVars"), "Save categories"),
           tags$hr(),
           tags$p('A description of a particular response to a categorical variable.'),
           tags$p('To assign categories either use an existing category set or create a new one and
                  then assign it to the variable.  Existing sets can be edited as needed.')
  )
}

var_label_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$var <- renderRHandsontable({
      req(dat())

      var <- tibble(
        name = character(),
        label = character(),
        lang = character()
      )

      if(length(dat()$dataDscr[["var"]]) == 0 ) {
        var <- add_row(var, name = "varName1")
      }
      
      for (v in dat()$dataDscr[["var"]]) {
        if(is.null(v$name)) v$name <- NA_character_
        if(length(v$labl) == 0) var <- add_row(var, name = v$name)
        for (l in v$labl) {
          if(is.null(l$lang)) l$lang <- NA_character_
          var <- add_row(var,
                            name = v$name,
                            label = l$value,
                            lang = l$lang)
        }
      }
      rht <- rhandsontable(var, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 60, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    observeEvent(
      input$saveVars, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_var <- hot_to_r(input$var)
          varList <- unique(updated_var$name)
          existing_vars <- list()
          
          for(v in dat()$dataDscr$var) {
            existing_vars <- append(existing_vars, v$name)
          }
          
          # remove previously existing vars that have been removed from app
          for(i in 1:length(existing_vars)) {
            if(existing_vars[[i]] %in% varList) {
            } else {
              updatedData$dataDscr[["var"]] <- lapply(1:length(updatedData$dataDscr[["var"]]), 
                                                      function(x) updatedData$dataDscr[["var"]][[x]][updatedData$dataDscr[["var"]][[x]]$name != existing_vars[[i]]])
              updatedData$dataDscr[["var"]] <- updatedData$dataDscr[["var"]][lengths(updatedData$dataDscr[["var"]]) > 0] 
            }
          }
          
          for(v in varList) {
            new_df <- updated_var %>% filter(name == v)
            new_l <- list()
            if(length(new_df$label) > 0) {
              for(l in 1:length(new_df$label)) {
                if(!is.na(new_df$label[l])) {
                  if(new_df$label[l] != "") {
                    labl <- list(value = new_df$label[l], 
                                 lang = stringr::str_extract(new_df$lang[l], "^[a-z]{2}"), 
                                 level = "var")
                    new_l <- c(new_l, list(labl))
                  }
                }
              }
              new_l <- recurse_write(new_l)
              new_l <- lapply(new_l,function(x) x[!is.na(x)])
            }
            
            if(v %in% existing_vars) {
              #only write over labels
              for(i in 1:length(updatedData$dataDscr[["var"]])) {
                # this way can't go over the max of the previously existing table...can't add a new one
                if(v == updatedData$dataDscr[["var"]][[i]]$name) {
                  updatedData$dataDscr[["var"]][[i]]$labl <- new_l
                }
              }
            } else {
              updatedData$dataDscr[["var"]] <- append(updatedData$dataDscr[["var"]], 
                                                    list(list(name = v,
                                                              labl = new_l)))
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}


var_characteristics_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$varCharacteristics <- renderRHandsontable({
      req(dat())
      
      var <- tibble(
        name = character(),
        nature = character(),
        temporal = character(),
        geog = character()
      )
      
      # get a list of all varGrps...
      name_list <- list()
      for(i in 1:length(dat()$dataDscr[["var"]])) {
        name_list <- append(name_list, dat()$dataDscr[["var"]][[i]]$name)
      }
      
      nature_list <- c("", "nominal", "ordinal", "interval", "ratio", "percent")
      yes_no_list <- c("", "Y", "N")
      
      for (v in dat()$dataDscr[["var"]]) {
        if(is.null(v$nature)) v$nature <- NA_character_
        if(is.null(v$temporal)) v$temporal <- NA_character_
        if(is.null(v$geog)) v$geog <- NA_character_
        var <- add_row(var,
                       name = v$name,
                       nature = v$nature,
                       temporal = v$temporal,
                       geog = v$geog)
      }

      rht <- rhandsontable(var, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("nature", allowInvalid = FALSE, type = "dropdown", source = nature_list) %>%
        hot_col("temporal", allowInvalid = FALSE, type = "dropdown", source = yes_no_list) %>%
        hot_col("geog", allowInvalid = FALSE, type = "dropdown", source = yes_no_list) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    observeEvent(
      input$saveVars, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_var <- hot_to_r(input$varCharacteristics)
          varList <- unique(updated_var$name)
          
          for(v in varList) {
            new_df <- updated_var %>% filter(name == v)
            if(!is.na(new_df$nature) & new_df$nature == "") new_df$nature <- NA_character_
            if(!is.na(new_df$temporal) & new_df$temporal == "") new_df$temporal <- NA_character_
            if(!is.na(new_df$geog) & new_df$geog == "") new_df$geog <- NA_character_
            if(!is.na(new_df$nature)) {
              nature <- new_df$nature 
            } else {
              nature <- NA_character_
            }
            if(!is.na(new_df$temporal)) {
              temporal <- new_df$temporal 
            } else {
              temporal <- NA_character_
            }
            if(!is.na(new_df$geog)) {
              geog <- new_df$geog 
            } else {
              geog <- NA_character_
            }
            #only write over characteristics
            for(i in 1:length(updatedData$dataDscr[["var"]])) {
              if(v == updatedData$dataDscr[["var"]][[i]]$name) {
                if(!is.na(nature)) {
                  updatedData$dataDscr[["var"]][[i]]$nature <- nature
                } else {
                  updatedData$dataDscr[["var"]][[i]]$nature <- NULL
                }
                if(!is.na(temporal)) {
                  updatedData$dataDscr[["var"]][[i]]$temporal <- temporal
                } else {
                  updatedData$dataDscr[["var"]][[i]]$temporal <- NULL
                }
                if(!is.na(geog)) {
                  updatedData$dataDscr[["var"]][[i]]$geog <- geog
                } else {
                  updatedData$dataDscr[["var"]][[i]]$geog <- NULL
                }
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

var_respondent_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$varRespondents <- renderRHandsontable({
      req(dat())
      respondents <- tibble(
        name = character(),
        respondent_type = character(),
        value = character(),
        lang = character()
      )
      
      # get a list of all varGrps...
      name_list <- list()
      respondent_type = c("Analysis Unit", "Response Unit")
      
      if(length(dat()$dataDscr[["var"]]) > 0) {
        for(i in 1:length(dat()$dataDscr[["var"]])) {
          name_list <- append(name_list, dat()$dataDscr[["var"]][[i]]$name)
        }
      
        respondent_exist <- FALSE
        for(v in dat()$dataDscr[["var"]]) {
          if(length(v$anlysUnit) > 0) respondent_exist = TRUE
          if(length(v$respUnit) > 0) respondent_exist = TRUE
        }
        
        if(!respondent_exist) {
          respondents <- add_row(respondents, name = name_list[[1]])
        }
        
        for (v in dat()$dataDscr[["var"]]) {
          for (resp in v$respUnit) {
            if(is.null(resp$lang)) resp$lang <- NA_character_
            respondents <- add_row(respondents,
                                   name = v$name,
                                   respondent_type = "Response Unit",
                                   value = resp$value,
                                   lang = resp$lang)
          }
          for(anly in v$anlysUnit) {
            if(is.null(resp$lang)) resp$lang <- NA_character_
            respondents <- add_row(respondents,
                                   name = v$name,
                                   respondent_type = "Analysis Unit",
                                   value = anly$value,
                                   lang = anly$lang)
          }
        }
      }
      rht <- rhandsontable(respondents, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("name", allowInvalid = FALSE, type = "dropdown", source = name_list) %>% 
        hot_col("respondent_type", allowInvalid = FALSE, type = "dropdown", source = respondent_type) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    observeEvent(
      input$saveVars, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_vars <- hot_to_r(input$varRespondents)
          varList <- unique(updated_vars$name)
          for(v in varList) {
            new_df <- updated_vars %>% filter(name == v)
            anlys_df <- new_df %>% filter(respondent_type == "Analysis Unit")
            resp_df <- new_df %>% filter(respondent_type == "Response Unit")
            
            new_resp <- list()
            new_anlys <- list()
            if(length(anlys_df$value) > 0) {
              for(i in 1:length(anlys_df$name)) {
                if(!is.na(anlys_df$value[i])) {
                  if(anlys_df$value[i] != "") {
                    anlys <- list(value = anlys_df$value[i], 
                                  lang = stringr::str_extract(anlys_df$lang[i], "^[a-z]{2}"))
                    new_anlys <- c(new_anlys, list(anlys))
                  }
                }
              }
            }
            if(length(resp_df$value) > 0) {
              for(i in 1:length(resp_df$name)) {
                if(!is.na(anlys_df$value[i])) {
                  if(anlys_df$value[i] != "") {
                    resp <- list(value = resp_df$value[i], 
                                 lang = stringr::str_extract(resp_df$lang[i], "^[a-z]{2}"))
                    new_resp <- c(new_resp, list(resp))
                  }
                }
              }
            }

            new_anlys <- recurse_write(new_anlys)
            new_anlys <- lapply(new_anlys,function(x) x[!is.na(x)])
            new_resp <- recurse_write(new_resp)
            new_resp <- lapply(new_resp,function(x) x[!is.na(x)])
          
            
            # write only over anlysUnit and respUnit
            for(i in 1:length(updatedData$dataDscr[["var"]])) {
              if(updatedData$dataDscr[["var"]][[i]]$name %in% varList) {
                if(v == updatedData$dataDscr[["var"]][[i]]$name) {
                  if(length(new_anlys) > 0) {
                    updatedData$dataDscr[["var"]][[i]]$anlysUnit <- new_anlys
                  } else {
                    updatedData$dataDscr[["var"]][[i]]$anlysUnit <- NULL
                  }
                  if(length(new_resp) > 0) {
                    updatedData$dataDscr[["var"]][[i]]$respUnit <- new_resp
                  } else {
                    updatedData$dataDscr[["var"]][[i]]$respUnit <- NULL
                  }
                }
              } 
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

var_varGrpAssign_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$var_varGrps <- renderRHandsontable({
      req(dat())
      varGrps <- tibble(
        variable = character(),
        variable_group = character()
      )
      
      varGrp2 <- tibble(
        variable = character(),
        variable_group = character()
      )

      varGrp_list <- c("")
      if(length(dat()$dataDscr$varGrp) > 0) {
        for(i in 1:length(dat()$dataDscr$varGrp)) {
          varGrp_list <- append(varGrp_list, dat()$dataDscr$varGrp[[i]]$name)
          if(!is.null(dat()$dataDscr$varGrp[[i]][["var"]])) {
            vars_temp <- strsplit(dat()$dataDscr$varGrp[[i]][["var"]], " ")
            for(v in vars_temp) {
              varGrp2<- add_row(varGrp2, 
                                variable = v,
                                variable_group = dat()$dataDscr$varGrp[[i]]$name)
            }
          }
        }
      }
      
      var <- tibble(variable = character())
      if(length(dat()$dataDscr[["var"]]) > 0) {
        for (v in dat()$dataDscr[["var"]]) {
          # pull the var from varGrp to put in here
          var <- add_row(var,
                         variable = v$name)
        }
      }
      varGrp <- left_join(var, varGrp2, by = "variable") %>% 
        mutate(variable_group = ifelse(is.na(variable_group), "", variable_group))
      
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("variable", readOnly = TRUE) %>%
        hot_col("variable_group", allowInvalid = FALSE, type = "dropdown", source = varGrp_list) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    observeEvent(
      input$saveVars, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_vars <- hot_to_r(input$var_varGrps)
          varGrpList <- unique(updated_vars$variable_group)
          
          #remove all var in varGrps
          for(i in 1:length(updatedData$dataDscr$varGrp)) {
            updatedData$dataDscr$varGrp[[i]]$var <- NULL
          }
    
          for(v in varGrpList) {
            new_df <- updated_vars %>% filter(variable_group == v)
            var_list <- list()
            if(length(new_df$variable) > 0) {
              for(i in 1:length(new_df$variable)) {
                if(!is.na(new_df$variable[i])) {
                  if(new_df$variable[i] != "") {
                    var_list <- append(var_list, new_df$variable[i])
                  }
                }
              }
            }
            if(length(var_list) > 0 & v != "") {
              vars <- paste(var_list, collapse = " ")
            } else {
              vars <- NA_character_
            }

            # write only over var in the varGrps
            for(i in 1:length(updatedData$dataDscr$varGrp)) {
              if(updatedData$dataDscr$varGrp[[i]]$name == v) {
                if(!is.na(vars)) {
                  updatedData$dataDscr$varGrp[[i]]$var <- vars
                } 
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}

var_security_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$varSecurity <- renderRHandsontable({
      req(dat())
      
      security <- tibble(
        variable = character(),
        value = character(),
        date = as.Date(""),
        lang = character()
      )
      
      # get a list of all varGrps...
      name_list <- list()
      if(length(dat()$dataDscr[["var"]]) > 0) {
        for(i in 1:length(dat()$dataDscr[["var"]])) {
          name_list <- append(name_list, dat()$dataDscr[["var"]][[i]]$name)
        }
        
        security_exist <- FALSE
        for(v in dat()$dataDscr[["var"]]) {
          if(length(v$security) > 0) security_exist = TRUE
        }
        if(!security_exist) {
          security <- add_row(security, variable = name_list[[1]])
        }
        
        for (v in dat()$dataDscr[["var"]]) {
          for(s in v$security) {
            if(is.null(s$date)) v$date <- NA_character_
            if(is.null(s$lang)) v$lang <- NA_character_
            security <- add_row(security,
                                variable = v$name,
                                value = s$value,
                                date = as.Date(s$date),
                                lang = s$lang)
          }
        }
      }
      
      rht <- rhandsontable(security, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("variable", allowInvalid = FALSE, type = "dropdown", source = name_list) %>%
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    output$varEmbargo <- renderRHandsontable({
      req(dat())
      
      embargo <- tibble(
        variable = character(),
        value = character(),
        date = as.Date(""),
        event = character(),
        lang = character()
      )
      
      # get a list of all varGrps...
      name_list <- list()
      event_list <- c("notBefore", "notAfter")
      if(length(dat()$dataDscr[["var"]]) > 0) {
        for(i in 1:length(dat()$dataDscr[["var"]])) {
          name_list <- append(name_list, dat()$dataDscr[["var"]][[i]]$name)
        }
        
        embargo_exist <- FALSE
        for(v in dat()$dataDscr[["var"]]) {
          if(length(v$embargo) > 0) embargo_exist = TRUE
        }
        if(!embargo_exist) {
          embargo <- add_row(embargo, variable = name_list[[1]])
        }
        
        for (v in dat()$dataDscr[["var"]]) {
          for(e in v$embargo) {
            if(is.null(e$date)) v$date <- NA_character_
            if(is.null(e$lang)) v$lang <- NA_character_
            embargo <- add_row(embargo,
                               variable = v$name,
                               value = e$value,
                               date = as.Date(e$date),
                               event = e$event,
                               lang = e$lang)
          }
        }
      }
      
      rht <- rhandsontable(embargo, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("variable", allowInvalid = FALSE, type = "dropdown", source = name_list) %>%
        hot_col("event", allowInvalid = FALSE, type = "dropdown", source = event_list) %>%
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$saveVars, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_security <- hot_to_r(input$varSecurity)
          updated_embargo <- hot_to_r(input$varEmbargo)
          sec_varList <- unique(updated_security$variable)
          emb_varList <- unique(updated_embargo$variable)
          
          #delete all current security and embargo information
          for(i in length(updatedData$dataDscr[["var"]])) {
            updatedData$dataDscr[["var"]][[i]]$security <- NULL
            updatedData$dataDscr[["var"]][[i]]$embargo <- NULL
          }

          for(v in sec_varList) {
            new_df <- updated_security %>% filter(variable == v)
            new_l <- list()
            if(length(new_df$value) > 0) {
              for(i in 1:length(new_df$value)) {
                if(!is.na(new_df$value[i])) {
                  if(new_df$value[i] != "") {
                    sec <- list(value = new_df$value[i], 
                                 lang = stringr::str_extract(new_df$lang[i], "^[a-z]{2}"), 
                                 date = as.character(new_df$date[i]))
                    new_l <- c(new_l, list(sec))
                  }
                }
              }
              new_l <- recurse_write(new_l)
              new_l <- lapply(new_l,function(x) x[!is.na(x)])
            }
            
            for(i in 1:length(updatedData$dataDscr[["var"]])) {
              if(v == updatedData$dataDscr[["var"]][[i]]$name) {
                if(length(new_l) > 0) {
                  updatedData$dataDscr[["var"]][[i]]$security <- new_l
                }
              }
            }
          }
          
          for(v in emb_varList) {
            new_df <- updated_embargo %>% filter(variable == v)
            new_l <- list()
            if(length(new_df$value) > 0) {
              for(i in 1:length(new_df$value)) {
                if(!is.na(new_df$value[i])) {
                  if(new_df$value[i] != "") {
                    if(is.na(new_df$event[i]) | new_df$event[i] == "") new_df$event[i] <- "notBefore"
                    labl <- list(value = new_df$value[i], 
                                 lang = stringr::str_extract(new_df$lang[i], "^[a-z]{2}"), 
                                 event = new_df$event[i],
                                 date = as.character(new_df$date[i]))
                    new_l <- c(new_l, list(labl))
                  }
                }
              }
              new_l <- recurse_write(new_l)
              new_l <- lapply(new_l,function(x) x[!is.na(x)])
            }
            
            for(i in 1:length(updatedData$dataDscr[["var"]])) {
              if(v == updatedData$dataDscr[["var"]][[i]]$name) {
                if(length(new_l) > 0) {
                  updatedData$dataDscr[["var"]][[i]]$embargo <- new_l
                }
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}

var_catgry_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    catgry_sets <- reactive({
      sets <- dat()$catgrySets
      names <- list()
      for(s in sets) {
        names <- append(names, s$name)
      }
      return(names)
    })
    
    output$selector <- renderUI({
      selectInput(session$ns("choice"), 
                  label = "Edit an existing catgry set", 
                  choices = catgry_sets(),
                  selected = "binary")
    })
    
    output$category_sets <- renderRHandsontable({
      req(dat())
      catSet <- tibble(
        catValu = character(),
        label = character(),
        lang = character()
      )

      if(input$createNew) {
        catSet <- add_row(catSet, catValu = "")
      } else {
        for(s in dat()$catgrySets) {
          if(s$name == input$choice) {
            for(option in s$options) {
              catValu <- option$catValu
              for(labl in option$labl) {
                if(is.null(labl$lang)) labl$lang <- ""
                catSet <- add_row(catSet,
                                  catValu = catValu,
                                  label = labl$value,
                                  lang = labl$lang)
              }
            }
          }
        }
      }

      rht <- rhandsontable(catSet, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })

    output$categories <- renderRHandsontable({
      req(dat())
      
      vars <- tibble(
        variable = character(),
        nature = character(),
        catSet = character()
      )
      # need a list of all nominal and ordinal variables
      for(v in dat()$dataDscr[["var"]]) {
        if(is.null(v$catSet)) v$catSet <- NA_character_
        if(is.null(v$nature)) v$nature <- NA_character_
        vars <- add_row(vars,
                        variable = v$name,
                        nature = v$nature,
                        catSet = v$catSet)
      }
      vars <- vars %>% 
        filter(nature == "nominal" | nature == "ordinal") %>% 
        select('variable', 'catSet')
      
      cat_list <- list("")
      cat_list <- append(cat_list, catgry_sets())

      rht <- rhandsontable(vars, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("variable", readOnly = TRUE) %>%
        hot_col("catSet", allowInvalid = FALSE, type = "dropdown", source = cat_list) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })

    observeEvent(
      input$saveCatgryChanges, {
        isolate({
          req(dat())
          updatedData <- dat()

          catgry_df <- hot_to_r(input$category_sets)
          catgry_list <- list()
          
          # create category list
          catValues <- unique(catgry_df$catValu)
          for(coding in catValues) {
            subset_df <- catgry_df %>% filter(catValu == coding)
            labl_list <- list()
            for(i in 1:length(subset_df$catValu)) {
              new_l <- list(value = subset_df$label[i],
                            lang = stringr::str_extract(subset_df$lang[i], "^[a-z]{2}"),
                            level = "catgry")
              labl_list <- c(labl_list, list(new_l))
            }
            labl_list <- recurse_write(labl_list)
            labl_list <- lapply(labl_list, function(x) x[!is.na(x)])
            cat_list <- list(catValu = coding, 
                             labl = labl_list)
            catgry_list <- append(catgry_list, list(cat_list))
          }
          catgry_list <- recurse_write(catgry_list)
          catgry_list <- lapply(catgry_list, function(x) x[!is.na(x)])
          
          if(input$createNew) {
            name = input$new_catgry_set_name
            new_l <- list(name = name, options = catgry_list)
            updatedData$catgrySets <- append(updatedData$catgrySets,
                                             list(new_l))
          } else {
            name = input$choice
            new_l <- list(name = name, options = catgry_list)
            # find  the right catgry to overwrite
            correct_entry = 0
            for(i in 1:length(updatedData$catgrySets)) {
              if(updatedData$catgrySets[[i]]$name == name) correct_entry = i
            }
            updatedData$catgrySets[[correct_entry]] <- new_l
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      }
    )
    
    observeEvent(
      input$saveVars, {
        isolate({
          req(dat())
          updatedData <- dat()
          
          updated_var <- hot_to_r(input$categories)

          for(i in 1:length(updated_var$variable)) {
            if(!is.na(updated_var$catSet[i]) & updated_var$catSet[i] == "") updated_var$catSet[i] <- NA_character_
            if(!is.na(updated_var$catSet[i])) {
              catSet <- updated_var$catSet[i] 
              catgrys <- list()
              for(cs in updatedData$catgrySets) {
                if(updated_var$catSet[i] == cs$name) {
                  catgrys <- cs$options
                }
              }
            } else {
              catSet <- NA_character_
              catgrys <- NA_character_
            }

            #only write over characteristics
            for(i2 in 1:length(updatedData$dataDscr[["var"]])) {
              if(updated_var$variable[i] == updatedData$dataDscr[["var"]][[i2]]$name) {
                if(!is.na(catSet)) {
                  updatedData$dataDscr[["var"]][[i2]]$catSet <- catSet
                  updatedData$dataDscr[["var"]][[i2]]$catgry <- catgrys
                } else {
                  updatedData$dataDscr[["var"]][[i2]]$catSet <- NULL
                  updatedData$dataDscr[["var"]][[i2]]$catgry <- NULL
                }
              }
            }
          }
          
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}