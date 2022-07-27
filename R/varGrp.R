varGrp_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Measures/Variable Groups",
           p("This section is separated into two tables to handle the labels, definitions,
               and universes of varGrps and the concepts behind varGrps. It may be
               expanded to include links to publications/methods/study descriptions 
               in the metadata, access restrictions, and etc."),
           tags$hr(),
           h3("Labels, Definitions, and Universes"),
           rHandsontableOutput(ns("varGrp")),
           tags$br(),
           p("A labl is a short description of the parent element. A defntn is 
               a rational for why the group was constituted in this specific way. A 
               universe is the group of persons or other elements that are the object of research."),
           tags$hr(),
           h3("Concepts"),
           rHandsontableOutput(ns("varGrp_concept")),
           tags$br(),
           p("The general concept(s) of the varGrps. Each concept should belong 
               to a controlled vocabulary that pertains to the designated community 
               of the dataset. Currently we use two controlled vocabularies, the UNESCO 
               Thesaurus for SDGs and the exploreSEL Thesaurus for academic researchers."),
           tags$hr(),
           actionButton(ns("save_varGrp"), "Save Variable Groups")
          )
}

varGrp_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$varGrp <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        element = character(),
        value = character(),
        lang = character()
      )
      
      if(length(dat()$dataDscr$varGrp) == 0) {
        varGrp <- add_row(varGrp, name = "varGrp1", element = "labl")
      }
      
      elementOptions <- c("labl", "defntn", "universe_I", "universe_E")
      for (vg in dat()$dataDscr$varGrp) {
        name <- vg$name
        if(length(dat()$dataDscr$varGrp) == 1 & is.null(vg$labl) & is.null(vg$defntn) & is.null(vg$universe)) {
          varGrp <- add_row(varGrp, name = name, element = "labl")
        }
        for (l in vg$labl) {
          if(is.null(l$lang)) l$lang <- NA_character_
          varGrp <- add_row(varGrp,
                            name = name,
                            element = "labl",
                            value = l$value,
                            lang = l$lang)
        }
        for (d in vg$defntn) {
          if(is.null(d$lang)) d$lang <- NA_character_
          varGrp <- add_row(varGrp,
                            name = name,
                            element = "defntn",
                            value = d$value,
                            lang = d$lang)
        }
        for (u in vg$universe) {
          if(is.null(u$lang)) u$lang <- NA_character_
          varGrp <- add_row(varGrp,
                            name = name,
                            element = paste0("universe_", u$clusion),
                            value = u$group,
                            lang = u$lang
          )
        }
      }
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("element", allowInvalid = FALSE, type = "dropdown", source = elementOptions) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    output$varGrp_concept <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        value = character(),
        vocabu = character(),
        vocab_URI = character(),
        lang = character()
      )
      if(length(dat()$dataDscr$varGrp) == 0) {
        varGrp <- add_row(varGrp, name = "varGrp1")
      } else if(length(sapply(dat()$dataDscr$varGrp, "[[", "concept")) > 0 & 
                is.null(sapply(dat()$dataDscr$varGrp, "[[", "concept")[[1]])) {
        varGrp <- add_row(varGrp, name = dat()$dataDscr$varGrp[[1]]$name)
      }
      
      for (vg in dat()$dataDscr$varGrp) {
        name <- vg$name
        for (con in vg$concept) {
          if(is.null(con$vocabu)) con$vocabu <- NA_character_
          if(is.null(con$vocab_URI)) con$vocab_URI <- NA_character_
          if(is.null(con$lang)) con$lang <- NA_character_
          
          varGrp <- add_row(varGrp,
                            name = name,
                            value = con$value,
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
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_varGrp, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_varGrp <- hot_to_r(input$varGrp)
          updated_concept <- hot_to_r(input$varGrp_concept)
          new_varGrp <- list()
          varGrpList <- unique(append(updated_varGrp$name, updated_concept$name))
          for(vg in varGrpList) {
            new_df <- updated_varGrp %>% filter(name == vg)
            new_labl <- new_df %>% filter(element == "labl")
            new_l <- list()
            new_defntn <- new_df %>% filter(element == "defntn")
            new_d <- list()
            new_universe <- new_df %>% filter(element == "universe_I" | element == "universe_E")
            new_u <- list()
            name <- vg 
            type <- new_df$type[[1]]
            if(length(new_labl$value) > 0) {
              for(l in 1:length(new_labl$value)) {
                if(!is.na(new_labl$value[l])) {
                  if(new_labl$value[l] != "") {
                    labl <- list(value = new_labl$value[l], 
                                 lang = stringr::str_extract(new_labl$lang[l], "^[a-z]{2}"), 
                                 level = "varGrp")
                    new_l <- c(new_l, list(labl))
                  }
                }
              }
              new_l <- recurse_write(new_l)
              new_l <- lapply(new_l,function(x) x[!is.na(x)])
            }
            if(length(new_defntn$value) > 0) {
              for(d in 1:length(new_defntn$value)) {
                if(!is.na(new_defntn$value[d])) {
                  if(new_defntn$value[d] != "") {
                    if(!is.na(new_defntn$value[d])) {
                      defntn <- list(value = new_defntn$value[d], 
                                     lang = stringr::str_extract(new_defntn$lang[d], "^[a-z]{2}"))
                      new_d <- c(new_d, list(defntn))
                    }
                  }
                }
              }
              new_d <- recurse_write(new_d)
              new_d <- lapply(new_d,function(x) x[!is.na(x)])
            }
            if(length(new_universe$value) > 0) {
              for(u in 1:length(new_universe$value)) {
                if(!is.na(new_universe$value[u])) {
                  if(new_universe$value[u] != "") {
                    if(str_detect(new_universe$element[u], "_I$")) {
                      universe <- list(group = new_universe$value[u],
                                       level = "varGrp",
                                       clusion = "I",
                                       lang = stringr::str_extract(new_universe$lang[u], "^[a-z]{2}"))
                    } else {
                      universe <- list(group = new_universe$value[u],
                                       level = "varGrp",
                                       clusion = "E",
                                       lang = stringr::str_extract(new_universe$lang[u], "^[a-z]{2}"))
                    }
                    new_u <- c(new_u, list(universe))
                  }
                }
              }
              new_u <- recurse_write(new_u)
              new_u <- lapply(new_u,function(x) x[!is.na(x)])
            }
            new_conc <- updated_concept %>% filter(name == vg)
            new_co <- list()
            if(length(new_conc$value) > 0) {
              for(c in 1:length(new_conc$value)) {
                if(!is.na(new_conc$value[c])) {
                  if(new_conc$value[c] != "") {
                    concept <- list(value = new_conc$value[c], 
                                    vocabu = new_conc$vocabu[c],
                                    vocab_URI = new_conc$vocab_URI[c],
                                    lang = stringr::str_extract(new_conc$lang[c], "^[a-z]{2}"))
                    new_co <- c(new_co, list(concept))
                  }
                }
              }
              new_co <- recurse_write(new_co)
              new_co <- lapply(new_co,function(x) x[!is.na(x)])
            }
            new <- list()
            new$name <- name
            new$type <- type
            if(length(new_l) > 0) new$labl <- new_l
            if(length(new_d) > 0) new$defntn <- new_d
            if(length(new_u) > 0) new$universe <- new_u
            if(length(new_co) > 0) new$concept <- new_co
            if (!is.null(new$lab) | !is.null(new$defntn) | !is.null(new$universe) | !is.null(new$concept)) {
              new_varGrp <- c(new_varGrp, list(new))  
            }
          }
          updatedData$dataDscr$varGrp <- new_varGrp
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}