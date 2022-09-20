#!/usr/bin/env Rscript --vanilla

library(shiny)
library(rhandsontable)
library(tidyverse)
library(data.tree)

loadSupport()

ui <- fluidPage(
  navbarPage(strong("DIY DDI Curator"),
    tabPanel("Introduction", 
             p('The DIY DDI Curator is designed for you, the researcher and/or information professional, 
               to edit, add, and delete  metadata for your projects using the DDI-Codebook 2.5 schema.'), 
             p('To begin, please pick the project you would like to edit 
                      below, create a new file, or upload your own (just remember 
                      to save before you shut down the app).'),
             tags$hr(), 
             uiOutput("project"),
             tags$hr(),
             fileInput("inputed_dat", 
                       "Upload an existing yaml file",
                       accept = ".yml"),
             tags$hr(),
             textInput("newFileName", label = "New local file", placeholder = "Type new file name here"),
             tags$em('When creating a new file please use snake_case (dashes instead 
               of blanks) or camelCase (no blanks but uppercase the first letter 
               of the non-first word.'),
             tags$br(),
             tags$br(),
             actionButton("createNewFile", "Create new file")
             ),
    navbarMenu(
      "Project Information",
      title_ui("titles"),
      authors_ui("authors"),
      series_ui("series"),
      producers_ui("producers"),
      funders_ui("funders")
    ),
    navbarMenu(
      "Study Information",
      abstract_ui("abstract"),
      subject_ui("subject"),
      universe_ui("universe"),
      anlyUnit_ui("anlyUnit"),
      dataKind_ui("dataKind"),
      geography_ui("geog"),
      timePeriods_ui("timePrd")
    ),
    navbarMenu(
      "Data Collection",
      collDate_ui("collDate"),
      timeMeth_ui("timeMeth"),
      frequenc_ui("frequenc"),
      dataCollector_ui("dataCollector"),
      collMode_ui("collMode"),
      collSitu_ui("collSitu"),
      collectorTraining_ui("collectorTraining"),
      resInstru_ui("resInstru"),
      instrumentDevelopment_ui("instrumentDevelopment"),
      ConOps_ui("ConOps"),
      actMin_ui("actMin"),
      sampProc_ui("sampProc"),
      deviat_ui("deviat")
    ),
    navbarMenu(
      "Bibliography",
      relMat_ui("relMat"),
      relStdy_ui("relStdy"),
      relPubl_ui("relPubl"),
      othRefs_ui("othRefs")
    ),
    navbarMenu(
      "Variable Groups",
      varGrp_label_ui("varGrp_l"),
      varGrp_type_ui("varGrp_t"),
      varGrp_defntn_ui("varGrp_d"),
      varGrp_universe_ui("varGrp_u"),
      varGrp_concept_ui("varGrp_c"),
      varGrp_hierarchy_ui("varGrp_h")
    ),
    navbarMenu(
      "Variables",
      var_label_ui("var"), 
      var_characteristics_ui("var_char"),
      var_respondent_ui("var_resp"),
      var_varGrpAssign_ui("varGrp_assign"),
      var_security_ui("var_sec"),
      var_catgry_ui("catgry")
    ),
    navbarMenu(
      "Evaluation/Export",
      readme_generation_ui("readme"),
      ddi_generation_ui("ddi"),
      download_data_ui("data_download")
    )
  )
)

server <- function(input, output, session) {
  
  filepth <- reactive({
    if(is.null(input$inputed_dat)) {
      r <- paste0("data/", input$project)
    } else {
      r <- input$inputed_dat$datapath
    }
    return(r)
  })
  
  init_dat <- reactiveFileReader(intervalMillis = 1000, 
                            session, 
                            filePath = filepth,
                            readFunc = yaml::read_yaml
                          )
  
  output$project <- renderUI(
    radioButtons("project", 
                 label = "Select Project",
                 choices = list.files("data/")
                 )
  )
  
  dat <- reactive(recurse_read(init_dat()))
  
  lang <- isolate(c("", "en - English", "fr - Français", "es - Español", 
                    "ar - عربى", "zh - 中国人", "ru - Русский"))
  
  
  
  observeEvent(
    input$createNewFile, {
      isolate({
        req(input$newFileName) 
        name <- str_replace_all(input$newFileName, " ", "_")
        if(!stringr::str_detect(input$newFileName, "[.]yml$")) {
          name <- paste0(name, ".yml")
        }
        file.copy("template.yml", "data/template.yml")
        file.rename("data/template.yml", paste0("data/", name))
        
        updateRadioButtons(session = session,
                           inputId = "project",
                           label = "Select Project",
                           choices = list.files("data/"))
      })
    })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Project Information servers 
  title_server("titles", dat, filepth, lang)
  authors_server("authors", dat, filepth)
  series_server("series", dat, filepth, lang)
  producers_server("producers", dat, filepth, lang)
  funders_server("funders", dat, filepth)
  
  # Study Information servers
  abstract_server("abstract", dat, filepth, lang)
  subject_server("subject", dat, filepth, lang)
  universe_server("universe", dat, filepth, lang)
  anlyUnit_server("anlyUnit", dat, filepth, lang)
  dataKind_server("dataKind", dat, filepth, lang)
  geography_server("geog", dat, filepth, lang)
  timePeriods_server("timePrd", dat, filepth, lang)
  
  # Data Collection servers
  collDate_server("collDate", dat, filepth, lang)
  timeMeth_server("timeMeth", dat, filepth, lang)  
  frequenc_server("frequenc", dat, filepth, lang)
  dataCollector_server("dataCollector", dat, filepth, lang)  
  collectorTraining_server("collectorTraining", dat, filepth, lang)
  collMode_server("collMode", dat, filepth, lang)
  collSitu_server("collSitu", dat, filepth, lang)
  resInstru_server("resInstru", dat, filepth, lang)
  instrumentDevelopment_server("instrumentDevelopment", dat, filepth, lang)
  ConOps_server("ConOps", dat, filepth, lang)
  actMin_server("actMin", dat, filepth, lang)
  sampProc_server("sampProc", dat, filepth, lang)
  deviat_server("deviat", dat, filepth, lang)
  
  relMat_server("relMat", dat, filepth, lang)
  relStdy_server("relStdy", dat, filepth, lang)
  relPubl_server("relPubl", dat, filepth, lang)
  othRefs_server("othRefs", dat, filepth, lang)
  
  # Vargrp servers
  varGrp_label_server("varGrp_l", dat, filepth, lang)
  varGrp_type_server("varGrp_t", dat, filepth)
  varGrp_defntn_server("varGrp_d", dat, filepth, lang)
  varGrp_universe_server("varGrp_u", dat, filepth, lang)
  varGrp_concept_server("varGrp_c", dat, filepth, lang)
  varGrp_hierarchy_server("varGrp_h", dat, filepth)
  
  # var servers
  var_label_server("var", dat, filepth, lang)
  var_characteristics_server("var_char", dat, filepth)
  var_respondent_server("var_resp", dat, filepth, lang)
  var_varGrpAssign_server("varGrp_assign", dat, filepth)
  var_security_server("var_sec", dat, filepth, lang)
  var_catgry_server("catgry", dat, filepth, lang)
  
  # Export servers
  ddi_generation_server("ddi", dat, filepth)
  readme_generation_server("readme", dat)
  download_data_server("data_download", dat)
}

shinyApp(server = server, ui = ui)