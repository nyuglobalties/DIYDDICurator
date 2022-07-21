ddi_generation_ui <- function(id) {
  ns <- NS(id)
  tabPanel("DDI Codebook",
           downloadButton(ns("download_DDI"), "Download codebook"),
           tags$br(),
           tags$br(),
           htmltidy::xmlviewOutput(ns("XML"))
  )
}

ddi_generation_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    output$XML <- htmltidy::renderXmlview({
      req(dat())
      htmltidy::xml_view(generate_ddi_codebook(dat()))
    })
    
    output$download_DDI <- downloadHandler(
      filename = "codebook.xml",
      content = function(file) {
        xml2::write_xml(generate_ddi_codebook(dat()), file)
      },
      contentType = "text/xml"
    )
  })
}