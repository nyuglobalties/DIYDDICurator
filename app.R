library(shiny)
library(rhandsontable)
library(tidyverse)

source("R/ui_utils.R")

ui <- fluidPage(
  navbarPage(strong("TIES metadata curation tool"),
    tabPanel("Introduction", 
             p('This is the TIES metadata curation tool. It is designed for 
               you, the researcher, to edit, add, and delete descriptive metadata 
               for your projects using tables that allows you edit existing 
               metadata, add or delete metadata by right clicking on the table, 
               and choose appropriate attributes from drop down menus. Each 
               table has a "lang" (language) attribute for translations.'),
             p('In addition, each table has the element definitions below it. In 
               some cases a table may represent more than one element in which 
               case a field column is added to the table.'),
             p(strong('To begin, please pick the project you would like to edit below.')),
             tags$hr(), 
             radioButtons("proj",
                          "Select Project",
                          choices = c("Sample - Project 1" = "samp1", 
                                      "Sample - Project 2" = "samp2"),
                          selected = "samp1")
             ),
    tabPanel("Project Information", 
             h1(textOutput("files")),
             p("This section holds all administrative data/Data Team data."),
             p("An important section in here will be quality control status and 
               data level (whether from a curation standpoint the data is suitable 
               for internal use, private sharing, replication/verification, or reuse.")),
    navbarMenu(
      "Study Information",
      tabPanel("Abstract", 
               rHandsontableOutput("abstract"),
               tags$br(),
               actionButton("saveAbstract", "Save abstracts"),
               tags$hr(),
               tags$p('Definition: An unformatted summary describing the purpose, nature, 
                      and scope of the data collection, special characteristics 
                      of its contents, major subject areas covered, and what 
                      questions the PIs attempted to answer when they conducted 
                      the study.'), 
               tags$p('Only three elements are allowed in content type: 
                      "abstract"; "purpose"; and "mixed".')
      ),
      tabPanel("Subjects",
               rHandsontableOutput("subject"),
               tags$br(),
               actionButton("saveSubject", "Save subjects"),
               tags$hr(),
               tags$p("Defintion: Words or phrases that describe salient aspects of a data 
             collection's content. Can be used for building keyword indexes and 
             for classification and retrieval purposes. A controlled vocabulary 
             can be employed. Maps to Dublin Core Subject element. The 'vocab' 
             attribute is provided for specification of the controlled 
             vocabulary in use, e.g., LCSH, MeSH, etc. The 'vocabURI attribute 
             specifies the location for the full controlled vocabulary.")
      ),
      tabPanel("Universe",
               rHandsontableOutput("universe"),
               tags$br(),
               actionButton("saveUniverse", "Save universe"),
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
                    expressions of the content.')),
      tabPanel("Unit of Analysis",
               rHandsontableOutput("anlyUnit"),
               tags$br(),
               actionButton("save_anlyUnit", "Save units of analysis"),
               tags$hr(),
               p("Definition: Basic unit of analysis or observation that the file 
               describes: individuals, families/households, groups, 
               institutions/organizations, administrative units, etc.")),
      tabPanel("Geography",
               rHandsontableOutput("geog"),
               tags$br(),
               actionButton("saveGeog", "Save geographic elements"),
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
               covered by the data.')),
      tabPanel("Time periods",
               rHandsontableOutput("timePrd"),
               tags$br(),
               actionButton("saveTimePrd_collDate", "Save time periods"),
               tags$hr(),
               p('timePrd Definition: The time period to which the data refer. 
               This item reflects the time period covered by the data, not the 
               dates of coding or making documents machine-readable or the dates 
               the data were collected. Also known as span. Use the event 
               attribute to specify "start", "end", or "single" for each date 
               entered. The "cycle" attribute permits specification of the 
               relevant cycle, wave, or round of data. Maps to Dublin Core 
               Coverage element. Inclusion of this element is recommended.'),
               p('collDate Definition: Contains the date(s) when the data were 
               collected. Use the event attribute to specify "start", "end", or 
               "single" for each date entered. The "cycle" attribute permits 
               specification of the relevant cycle, wave, or round of data. 
               Maps to Dublin Core Coverage element. Inclusion of this element 
               in the codebook is recommended.')
      )
    ),
    navbarMenu(
      "Data Collection",
      tabPanel("Time Method",
               rHandsontableOutput("timeMeth"),
               tags$br(),
               actionButton("save_timeMeth", "Save time method"),
               tags$hr(),
               p('The time method or time dimension of the data collection.'),
               p('The txt element is there to provide a lengthier description 
                 of the parent element that is more suitable for documentation.')),
      tabPanel("Frequency of Data Collection",
               rHandsontableOutput("frequenc"),
               tags$br(),
               actionButton("save_frequenc", "Save frequency"),
               tags$hr(),
               p('For data collected at more than one point in time, the 
                 frequency with which the data were collected.')),
      tabPanel("Data Collectors",
               rHandsontableOutput("dataCollector"),
               tags$br(),
               actionButton("save_dataCollector", "Save data collector"),
               tags$hr(),
               p('The entity (individual, agency, or institution) responsible 
               for administering the questionnaire or interview or compiling 
               the data. This refers to the entity collecting the data, not to 
               the entity producing the documentation. Attribute "abbr" may be 
               used to list common abbreviations given to agencies, etc.'), 
               p('Attribute "affiliation" may be used to record affiliation of the 
               data collector. The role attribute specifies the role of person 
               in the data collection process.')),
      tabPanel("Mode of Data Collection",
               rHandsontableOutput("collMode"),
               tags$br(),
               actionButton("save_collMode", "Save mode of data collection"),
               tags$hr(),
               p('The method used to collect the data; instrumentation 
                 characteristics.')),
      tabPanel("Characteristics of Data Collection Situation",
               rHandsontableOutput("collSitu"),
               tags$br(),
               actionButton("save_collSitu", "Save characteristics of data collection situation"),
               tags$hr(),
               p('Description of noteworthy aspects of the data collection 
                 situation. Includes information on factors such as c
                 ooperativeness of respondents, duration of interviews, number 
                 of call-backs, etc.')),
      tabPanel("Type of Research Instrument",
               rHandsontableOutput("resInstru"),
               tags$br(),
               actionButton('save_resInstru', "Save type of research instrument"),
               tags$hr(),
               p('The type of data collection instrument used. "Structured" 
                 indicates an instrument in which all respondents are asked the 
                 same questions/tests, possibly with precoded answers. If a 
                 small portion of such a questionnaire includes open-ended 
                 questions, provide appropriate comments. "Semi-structured" 
                 indicates that the research instrument contains mainly 
                 open-ended questions. "Unstructured" indicates that in-depth 
                 interviews were conducted.')),
      tabPanel("Instrument Development",
               rHandsontableOutput("instrumentDevelopment"),
               tags$br(),
               actionButton("save_instrumentDevelopment", "Save instrument development"),
               tags$hr(),
               p('Describe any development work on the data collection 
                 instrument. Type attribute allows for the optional use of a 
                 defined development type with or without use of a controlled 
                 vocabulary.')),
      tabPanel("Control Operations",
               rHandsontableOutput("ConOps"),
               tags$br(),
               actionButton("save_ConOps", "Save control operations"),
               tags$hr(),
               p('Methods to facilitate data control performed by the primary 
                 investigator or by the data archive. Specify any special 
                 programs used for such operations. The "agency" attribute maybe 
                 used to refer to the agency that performed the control 
                 operation.')),
      tabPanel("Actions to Minimize Losses",
               rHandsontableOutput("actMin"),
               tags$br(),
               actionButton("save_actMin", "Save actions to minimize losses"),
               tags$hr(),
               p('Summary of actions taken to minimize data loss. Includes 
                 information on actions such as follow-up visits, supervisory 
                 checks, historical matching, estimation, etc.')),
      tabPanel("Sampling Procedure",
               rHandsontableOutput("sampProc"),
               tags$br(),
               actionButton("save_sampProc", "Save sampling procedure"),
               tags$hr(),
               p('The type of sample and sample design used to select the 
                 survey respondents to represent the population. May include 
                 reference to the target sample size and the sampling fraction.')),
      tabPanel("Major Deviations from Sample Design",
               rHandsontableOutput("deviat"),
               tags$br(),
               actionButton("save_deviat", "Save major deviations from Sample Design"),
               tags$hr(),
               p('Information indicating correspondence as well as discrepancies 
                 between the sampled units (obtained) and available statistics 
                 for the population (age, sex-ratio, marital status, etc.) as a 
                 whole.'))
    ),
    tabPanel("Measures/Variable Groups",
             rHandsontableOutput("varGrp"),
             tags$br(),
             actionButton("save_varGrp", "Save Variable Groups"),
             tags$hr(),
             p("This will consist of a table of the varGrps and its elements, it 
               may be structured differently from the other elements because 
               ideally I want the researchers to assign what variable belongs to 
               what group.")),
    tabPanel("Evaluation/Export",
             p("This section will allow the researcher/s to export the existing 
               metadata into a file using a .Rmd template. I might want them to 
               be able to pick elements to include OR include elements from 
               multiple projects. This way the metadata can serve as a resource 
               for other writing."),
             p("In addition, this section will evaluate the metadata 
               programmatically according to a rubric so that we know the curation
               quality of the metadata.")
             )
  )
)

server <- function(input, output, session) {
  
  filepth <- reactive({
    r <- switch(input$proj,
                samp1 = "data/metadata_sample1.yml",
                samp2 = "data/metadata_sample2.yml")
    return(r)
  })
  
  dat <- reactiveFileReader(intervalMillis = 1000, 
                            session, 
                            filePath = filepth,
                            readFunc = yaml::read_yaml
                          )
  
  output$abstract <- renderRHandsontable({
    req(dat())
    contentTypeOptions <- c(NA_character_, "abstract", "purpose", "mixed")
    abstract <- tibble(
      value = character(),
      contentType = factor(),
      lang = character()
      )
    for (a in dat()$stdyDscr$stdyInfo$abstract) {
      abstract <- add_row(abstract, value = a$value, contentType = a$contentType, lang = a$lang)
    }
    rht <- rhandsontable(abstract, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_col("contentType", allowInvalid = FALSE, type = "dropdown", source = contentTypeOptions) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
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

  output$universe <- renderRHandsontable({
    req(dat())
    clusionOptions <- c("I", "E")
    universe <- tibble(
      group = character(),
      clusion = factor(),
      lang = character(),
      textForDocumentation = character()
    )
    for (u in dat()$stdyDscr$stdyInfo$sumDscr$universe) {
      universe <- add_row(universe, 
                         group = u$group, 
                         clusion = u$clusion,
                         lang = u$lang,
                         textForDocumentation = u$textForDocumentation)
    }
    rht <- rhandsontable(universe, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40, 40, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_col("clusion", allowInvalid = FALSE, type = "dropdown", source = clusionOptions) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$anlyUnit <- renderRHandsontable({
    req(dat())
    anlyUnit <- tibble(
      group = character(),
      lang = character(),
      textForDocumentation = character()
    )
    for (a in dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) {
      anlyUnit <- add_row(anlyUnit, 
                          group = a$group, 
                          lang = a$lang,
                          textForDocumentation = a$textForDocumentation)
    }
    rht <- rhandsontable(anlyUnit, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$timePrd <- renderRHandsontable({
    req(dat())
    fieldOptions <- c("timePrd", "collDate")
    eventOptions <- c("start", "end", "single")
    cycleOptions <- c("Wave1", "Wave2", "Wave3", "Wave4", "Wave5", "Wave6", "Wave7")
    prds <- tibble(
      field = factor(),
      date = as.Date(""),
      event = factor(),
      cycle = factor()
    )
    for (tp in dat()$stdyDscr$stdyInfo$sumDscr$timePrd) {
      prds <- add_row(prds,
                      field = "timePrd",
                      date = as.Date(tp$date), 
                      event = tp$event,
                      cycle = tp$cycle
                      )
    }
    for (cd in dat()$stdyDscr$stdyInfo$sumDscr$collDate) {
      prds <- add_row(prds,
                      field = "collDate",
                      date = as.Date(cd$date), 
                      event = cd$event,
                      cycle = cd$cycle
                      )
    }
    rht <- rhandsontable(prds, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40, 40, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_col("field", allowInvalid = FALSE, type = "dropdown", source = fieldOptions) %>% 
      hot_col("event", allowInvalid = FALSE, type = "dropdown", source = eventOptions) %>%
      hot_col("cycle", allowInvalid = FALSE, type = "dropdown", source = cycleOptions) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })

  output$geog <- renderRHandsontable({
    req(dat())
    fieldOptions <- c("nation", "geogCover", "geogUnit")
    geog <- tibble(
      field = factor(),
      value = character(),
      abbr = character(),
      lang = character()
    )
    for (v in dat()$stdyDscr$stdyInfo$sumDscr$nation) {
      geog <- add_row(geog,
                      field = "nation",
                      value = v$value,
                      abbr = v$abbr,
                      lang = v$lang
      )
    }
    for (v in dat()$stdyDscr$stdyInfo$sumDscr$geogCover) {
      geog <- add_row(geog,
                      field = "geogCover",
                      value = v$value, 
                      abbr = NA,
                      lang = v$lang
      )
    }
    for (v in dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) {
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
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })

  output$timeMeth <- renderRHandsontable({
    req(dat())
    timeMeth <- tibble(
      value = character(),
      lang = character(),
      txt = character()
    )
    for (t in dat()$stdyDscr$method$dataColl$timeMeth) {
      timeMeth <- add_row(timeMeth, 
                          value = t$value, 
                          lang = t$lang,
                          txt = t$txt)
    }
    rht <- rhandsontable(timeMeth, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$frequenc <- renderRHandsontable({
    req(dat())
    frequenc <- tibble(
      value = character(),
      lang = character()
    )
    for (f in dat()$stdyDscr$method$dataColl$frequenc) {
      frequenc <- add_row(frequenc, 
                          value = f$value, 
                          lang = f$lang)
    }
    rht <- rhandsontable(frequenc, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$dataCollector <- renderRHandsontable({
    req(dat())
    dataCollector <- tibble(
      value = character(),
      lang = character()
    )
    for (d in dat()$stdyDscr$method$dataColl$dataCollector) {
      dataCollector <- add_row(dataCollector, 
                          value = d$value, 
                          lang = d$lang)
    }
    rht <- rhandsontable(dataCollector, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$collMode <- renderRHandsontable({
    req(dat())
    collMode <- tibble(
      value = character(),
      lang = character()
    )
    for (c in dat()$stdyDscr$method$dataColl$collMode) {
      collMode <- add_row(collMode, 
                               value = c$value, 
                               lang = c$lang)
    }
    rht <- rhandsontable(collMode, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$collSitu <- renderRHandsontable({
    req(dat())
    collSitu <- tibble(
      value = character(),
      lang = character()
    )
    for (c in dat()$stdyDscr$method$dataColl$collSitu) {
      collSitu <- add_row(collSitu, 
                               value = c$value, 
                               lang = c$lang)
    }
    rht <- rhandsontable(collSitu, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$resInstru <- renderRHandsontable({
    req(dat())
    resInstru <- tibble(
      value = character(),
      type = character(),
      lang = character()
    )
    for (r in dat()$stdyDscr$method$dataColl$resInstru) {
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
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$instrumentDevelopment <- renderRHandsontable({
    req(dat())
    instrumentDevelopment <- tibble(
      value = character(),
      type = character(),
      lang = character()
    )
    for (i in dat()$stdyDscr$method$dataColl$instrumentDevelopment) {
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
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$ConOps <- renderRHandsontable({
    req(dat())
    ConOps <- tibble(
      value = character(),
      agency = character(),
      lang = character()
    )
    for (c in dat()$stdyDscr$method$dataColl$ConOps) {
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
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$actMin <- renderRHandsontable({
    req(dat())
    actMin <- tibble(
      value = character(),
      lang = character()
    )
    for (a in dat()$stdyDscr$method$dataColl$actMin) {
      actMin <- add_row(actMin, 
                        value = a$value, 
                        lang = a$lang)
    }
    rht <- rhandsontable(actMin, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$sampProc <- renderRHandsontable({
    req(dat())
    sampProc <- tibble(
      value = character(),
      lang = character(),
      txt = character()
    )
    for (s in dat()$stdyDscr$method$dataColl$sampProc) {
      sampProc <- add_row(sampProc, 
                          value = s$value, 
                          lang = s$lang,
                          txt = s$txt)
    }
    rht <- rhandsontable(sampProc, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40, 100),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$deviat <- renderRHandsontable({
    req(dat())
    deviat <- tibble(
      value = character(),
      lang = character()
    )
    for (d in dat()$stdyDscr$method$dataColl$deviat) {
      deviat <- add_row(deviat, 
                        value = d$value, 
                        lang = d$lang)
    }
    rht <- rhandsontable(deviat, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
      hot_cols(colWidths = c(100, 40),
               manualColumnMove = FALSE,
               manualColumnResize = FALSE) %>% 
      hot_rows(rowHeights = NULL) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
  })
  
  output$varGrp <- renderRHandsontable({
    req(dat())
    varGrp <- tibble(
      name = character(),
      type = character(),
      element = character(),
      value = character(),
      lang = character()
    )
    elementOptions <- c("labl", "defntn", "universe_I", "universe_E")
    typeOptions <- c("section", "multipleResp", "grid", "display", "repetition",
                     "subject", "version", "iteration", "analysis", "pragmatic",
                     "record", "file", "randomized", "other")
    for (vg in dat()$dataDscr$varGrp) {
      name <- vg$name
      type <- vg$type
      for (l in vg$labl) {
        varGrp <- add_row(varGrp,
                          name = name,
                          type = type,
                          element = "labl",
                          value = l$value,
                          lang = l$lang)
      }
      for (d in vg$defntn) {
        varGrp <- add_row(varGrp,
                          name = name,
                          type = type,
                          element = "defntn",
                          value = d$value,
                          lang = d$lang)
      }
      for (u in vg$universe) {
        varGrp <- add_row(varGrp,
                          name = name,
                          type = type,
                          element = paste0("universe_", u$clusion),
                          value = u$value,
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
      hot_col("type", allowInvalid = FALSE, type = "dropdown", source = typeOptions) %>% 
      hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
    htmlwidgets::onRender(rht, change_hook)
    
  })
# ---------------
  
  output$files <- renderText( {
    req(dat())
    title <- dat()$stdyDscr$citation$titlStmt$titl
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
          new <- list(value = updatedAbstracts$value[i],
                      contentType = updatedAbstracts$contentType[i],
                      lang  = updatedAbstracts$lang[i]
                  )
          newAbstract <- c(newAbstract, list(new))
        }
        updatedData$stdyDscr$stdyInfo$abstract <- newAbstract
        yaml::write_yaml(updatedData, filepth())
      })
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
  
  observeEvent(
    input$saveUniverse, {
      isolate({
        req(dat())
        updatedData <- dat()
        updatedUniverse<- hot_to_r(input$universe)
        updatedData$stdyDscr$sumDscr$universe <- NULL
        newUniverse <- list()
        for(i in 1:length(updatedUniverse$group)) {
          new <- list(group = updatedUniverse$group[i],
                      level = "project",
                      clusion = updatedUniverse$clusion[i],
                      lang  = updatedUniverse$lang[i],
                      textForDocumentation = updatedUniverse$textForDocumentation[i]
          )
          newUniverse <- c(newUniverse, list(new))
        }
        updatedData$stdyDscr$stdyInfo$sumDscr$universe <- newUniverse
        yaml::write_yaml(updatedData, filepth())
      })
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
          new <- list(group = updated_anlyUnit$group[i],
                      lang  = updated_anlyUnit$lang[i],
                      textForDocumentation = updated_anlyUnit$textForDocumentation[i]
          )
          new_anlyUnit <- c(new_anlyUnit, list(new))
        }
        updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit <- new_anlyUnit
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$saveTimePrd_collDate, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_prds <- hot_to_r(input$timePrd)
        updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- NULL
        updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- NULL
        new_timePrd <- list()
        new_collDate <- list()
        # not writing properly
        for(i in 1:length(updated_prds$field)) {
          if(updated_prds$field[i] == "timePrd") {
            new_tp <- list(date = as.character(updated_prds$date[i]),
                           event = updated_prds$event[i],
                           cycle = updated_prds$cycle[i]
            )
            new_timePrd <- c(new_timePrd, list(new_tp))
          } else {
            new_cd <- list(date = as.character(updated_prds$date[i]),
                           event = updated_prds$event[i],
                           cycle = updated_prds$cycle[i]
            )
            new_collDate <- c(new_collDate, list(new_cd))
          }
        }
        updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- new_timePrd
        updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- new_collDate
        yaml::write_yaml(updatedData, filepth())
      })
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
          if(updated_geog$field[i] == "nation") {
            new_n <- list(value = updated_geog$value[i],
                               abbr = updated_geog$abbr[i],
                               lang = updated_geog$lang[i]
            )
            new_nation <- c(new_nation, list(new_n))
          } else if(updated_geog$field[i] == "geogCover"){
            new_gc <- list(value = updated_geog$value[i],
                                  lang = updated_geog$lang[i]
            )
            new_geogCover <- c(new_geogCover, list(new_gc))
          } else {
            new_gu <- list(value = updated_geog$value[i],
                           lang = updated_geog$lang[i]
            )
            new_geogUnit <- c(new_geogUnit, list(new_gu))
          }
        }
        updatedData$stdyDscr$stdyInfo$sumDscr$nation <- new_nation
        updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- new_geogCover
        updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- new_geogUnit
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_timeMeth, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_timeMeth <- hot_to_r(input$timeMeth)
        updatedData$stdyDscr$method$dataColl$timeMeth <- NULL
        new_timeMeth <- list()
        for(i in 1:length(updated_timeMeth$value)) {
          new <- list(value = updated_timeMeth$value[i],
                      lang  = updated_timeMeth$lang[i],
                      txt = updated_timeMeth$txt[i]
          )
          new_timeMeth <- c(new_timeMeth, list(new))
        }
        updatedData$stdyDscr$method$dataColl$timeMeth <- new_timeMeth
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_frequenc, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_frequenc <- hot_to_r(input$frequenc)
        updatedData$stdyDscr$method$dataColl$frequenc <- NULL
        new_frequenc <- list()
        for(i in 1:length(updated_frequenc$value)) {
          new <- list(value = updated_frequenc$value[i],
                      lang  = updated_frequenc$lang[i]
          )
          new_frequenc <- c(new_frequenc, list(new))
        }
        updatedData$stdyDscr$method$dataColl$frequenc <- new_frequenc
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_dataCollector, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_dataCollector <- hot_to_r(input$dataCollector)
        updatedData$stdyDscr$method$dataColl$dataCollector <- NULL
        new_dataCollector <- list()
        for(i in 1:length(updated_dataCollector$value)) {
          new <- list(value = updated_dataCollector$value[i],
                      lang  = updated_dataCollector$lang[i]
          )
          new_dataCollector <- c(new_dataCollector, list(new))
        }
        updatedData$stdyDscr$method$dataColl$dataCollector <- new_dataCollector
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_dataCollector, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_dataCollector <- hot_to_r(input$dataCollector)
        updatedData$stdyDscr$method$dataColl$dataCollector <- NULL
        new_dataCollector <- list()
        for(i in 1:length(updated_dataCollector$value)) {
          new <- list(value = updated_dataCollector$value[i],
                      lang  = updated_dataCollector$lang[i]
          )
          new_dataCollector <- c(new_dataCollector, list(new))
        }
        updatedData$stdyDscr$method$dataColl$dataCollector <- new_dataCollector
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_collMode, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_collMode <- hot_to_r(input$collMode)
        updatedData$stdyDscr$method$dataColl$collMode <- NULL
        new_collMode <- list()
        for(i in 1:length(updated_collMode$value)) {
          new <- list(value = updated_collMode$value[i],
                      lang  = updated_collMode$lang[i]
          )
          new_collMode <- c(new_collMode, list(new))
        }
        updatedData$stdyDscr$method$dataColl$collMode <- new_collMode
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_collSitu, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_collSitu <- hot_to_r(input$collSitu)
        updatedData$stdyDscr$method$dataColl$collSitu <- NULL
        new_collSitu <- list()
        for(i in 1:length(updated_collSitu$value)) {
          new <- list(value = updated_collSitu$value[i],
                      lang  = updated_collSitu$lang[i]
          )
          new_collSitu <- c(new_collSitu, list(new))
        }
        updatedData$stdyDscr$method$dataColl$collSitu <- new_collSitu
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_resInstru, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_resInstru <- hot_to_r(input$resInstru)
        updatedData$stdyDscr$method$dataColl$resInstru <- NULL
        new_resInstru <- list()
        for(i in 1:length(updated_resInstru$value)) {
          new <- list(value = updated_resInstru$value[i],
                      type = updated_resInstru$type[i],
                      lang  = updated_resInstru$lang[i]
          )
          new_resInstru <- c(new_resInstru, list(new))
        }
        updatedData$stdyDscr$method$dataColl$resInstru <- new_resInstru
        yaml::write_yaml(updatedData, filepth())
      })
    })

  observeEvent(
    input$save_instrumentDevelopment, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_instrumentDevelopment <- hot_to_r(input$instrumentDevelopment)
        updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- NULL
        new_instrumentDevelopment <- list()
        for(i in 1:length(updated_instrumentDevelopment$value)) {
          new <- list(value = updated_instrumentDevelopment$value[i],
                      type = updated_instrumentDevelopment$type[i],
                      lang  = updated_instrumentDevelopment$lang[i]
          )
          new_instrumentDevelopment <- c(new_instrumentDevelopment, list(new))
        }
        updatedData$stdyDscr$method$dataColl$instrumentDevelopment <- new_instrumentDevelopment
        yaml::write_yaml(updatedData, filepth())
      })
    })

  observeEvent(
    input$save_ConOps, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_ConOps <- hot_to_r(input$ConOps)
        updatedData$stdyDscr$method$dataColl$ConOps <- NULL
        new_ConOps <- list()
        for(i in 1:length(updated_ConOps$value)) {
          new <- list(value = updated_ConOps$value[i],
                      agency = updated_ConOps$agency[i],
                      lang  = updated_ConOps$lang[i]
          )
          new_ConOps <- c(new_ConOps, list(new))
        }
        updatedData$stdyDscr$method$dataColl$ConOps <- new_ConOps
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_actMin, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_actMin <- hot_to_r(input$actMin)
        updatedData$stdyDscr$method$dataColl$actMin <- NULL
        new_actMin <- list()
        for(i in 1:length(updated_actMin$value)) {
          new <- list(value = updated_actMin$value[i],
                      lang  = updated_actMin$lang[i]
          )
          new_actMin <- c(new_actMin, list(new))
        }
        updatedData$stdyDscr$method$dataColl$actMin <- new_actMin
        yaml::write_yaml(updatedData, filepth())
      })
    })

  observeEvent(
    input$save_sampProc, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_sampProc <- hot_to_r(input$sampProc)
        updatedData$stdyDscr$method$dataColl$sampProc <- NULL
        new_sampProc <- list()
        for(i in 1:length(updated_sampProc$value)) {
          new <- list(value = updated_sampProc$value[i],
                      lang  = updated_sampProc$lang[i],
                      txt  = updated_sampProc$txt[i]
          )
          new_sampProc <- c(new_sampProc, list(new))
        }
        updatedData$stdyDscr$method$dataColl$sampProc <- new_sampProc
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_deviat, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_deviat <- hot_to_r(input$deviat)
        updatedData$stdyDscr$method$dataColl$deviat <- NULL
        new_deviat <- list()
        for(i in 1:length(updated_deviat$value)) {
          new <- list(value = updated_deviat$value[i],
                      lang  = updated_deviat$lang[i]
          )
          new_deviat <- c(new_deviat, list(new))
        }
        updatedData$stdyDscr$method$dataColl$deviat <- new_deviat
        yaml::write_yaml(updatedData, filepth())
      })
    })
  
  observeEvent(
    input$save_varGrp, {
      isolate({
        req(dat())
        updatedData <- dat()
        updated_varGrp <- hot_to_r(input$varGrp)
        new_varGrp <- list()
        varGrpList <- unique(updated_varGrp$name)
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
                universe <- list(value = new_universe$value[u],
                                 level = "varGrp",
                                 clusion = "I",
                                 lang = new_universe$lang[u])
              } else {
                universe <- list(value = new_universe$value[u],
                                 level = "varGrp",
                                 clusion = "E",
                                 lang = new_universe$lang[u])
              }
              new_u <- c(new_u, list(universe))
            }
          }
          # the below is creating empty elements when there isn't a labl, defntn or universe
          new <- list()
          new$name <- name
          new$type <- type
          if(length(new_l) > 0) new$labl <- new_l
          if(length(new_d) > 0) new$defntn <- new_d
          if(length(new_u) > 0) new$universe <- new_u
          new_varGrp <- c(new_varGrp, list(new))
        }
        updatedData$dataDscr$varGrp <- new_varGrp
        yaml::write_yaml(updatedData, filepth())
      })
    }
  )
  
}

shinyApp(server = server, ui = ui)