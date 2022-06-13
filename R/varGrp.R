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
           p("A labl is a short description of the parent element. A defntn is 
               a rational for why the group was constituted in this specific way. A 
               universe is the group of persons or other elements that are the object of research."),
           tags$hr(),
           h3("Concepts"),
           rHandsontableOutput(ns("varGrp_concept")),
           p("The general concept(s) of the varGrps. Each concept should belong 
               to a controlled vocabulary that pertains to the designated community 
               of the dataset. Currently we use two controlled vocabularies, the UNESCO 
               Thesaurus for SDGs and the exploreSEL Thesaurus for academic researchers."),
           tags$hr(),
           actionButton(ns("save_varGrp"), "Save Variable Groups")
          )
}

varGrp_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$varGrp <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        element = character(),
        value = character(),
        lang = character()
      )
      elementOptions <- c("labl", "defntn", "universe_I", "universe_E")
      for (vg in dat()$dataDscr$varGrp) {
        name <- vg$name
        for (l in vg$labl) {
          varGrp <- add_row(varGrp,
                            name = name,
                            element = "labl",
                            value = l$value,
                            lang = l$lang)
        }
        for (d in vg$defntn) {
          varGrp <- add_row(varGrp,
                            name = name,
                            element = "defntn",
                            value = d$value,
                            lang = d$lang)
        }
        for (u in vg$universe) {
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
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
      
    })  
    
    output$varGrp_concept <- renderRHandsontable({
      req(dat())
      varGrp <- tibble(
        name = character(),
        value = character(),
        vocab = character(),
        lang = character()
      )
      
      vocabOptions <- c("UNESCO", "exporeSEL")
      for (vg in dat()$dataDscr$varGrp) {
        name <- vg$name
        for (c in vg$concept) {
          varGrp <- add_row(varGrp,
                            name = name,
                            value = c$value,
                            vocab = c$vocab,
                            lang = c$lang)
        }
      }
      
      rht <- rhandsontable(varGrp, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("vocab", allowInvalid = FALSE, type = "dropdown", source = vocabOptions) %>% 
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
                labl <- list(value = new_labl$value[l], lang = new_labl$lang[l], level = "varGrp")
                new_l <- c(new_l, list(labl))
              }
            }
            if(length(new_defntn$value) > 0) {
              for(d in 1:length(new_defntn$value)) {
                defntn <- list(value = new_defntn$value[d], lang = new_defntn$lang[d])
                new_d <- c(new_d, list(defntn))
              }
            }
            if(length(new_universe$value) > 0) {
              for(u in 1:length(new_universe$value)) {
                if(str_detect(new_universe$element[u], "_I$")) {
                  universe <- list(group = new_universe$value[u],
                                   level = "varGrp",
                                   clusion = "I",
                                   lang = new_universe$lang[u])
                } else {
                  universe <- list(group = new_universe$value[u],
                                   level = "varGrp",
                                   clusion = "E",
                                   lang = new_universe$lang[u])
                }
                new_u <- c(new_u, list(universe))
              }
            }
            new_conc <- updated_concept %>% filter(name == vg)
            new_c <- list()
            if(length(new_conc$value) > 0) {
              for(c in 1:length(new_conc$value)) {
                if(new_conc$vocab[c] == "UNESCO") vocabURI <- "http://vocabularies.unesco.org/thesaurus"
                if(new_conc$vocab[c] == "exporeSEL") vocabURI <- "http://exploresel.gse.harvard.edu/terms/"
                concept <- list(value = new_conc$value[c], 
                                vocab = new_conc$vocab[c],
                                vocabURI = vocabURI,
                                lang = new_conc$lang[c])
                new_c<- c(new_c, list(concept))
              }
            }
            
            new <- list()
            new$name <- name
            new$type <- type
            if(length(new_l) > 0) new$labl <- new_l
            if(length(new_d) > 0) new$defntn <- new_d
            if(length(new_u) > 0) new$universe <- new_u
            if(length(new_c) > 0) new$concept <- new_c
            new_varGrp <- c(new_varGrp, list(new))
          }
          updatedData$dataDscr$varGrp <- new_varGrp
          yaml::write_yaml(updatedData, filepth())
        })
      })
  })
}