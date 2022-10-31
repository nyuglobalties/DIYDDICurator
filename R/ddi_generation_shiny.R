ddi_generation_ui <- function(id) {
  ns <- NS(id)
  tabPanel("DDI Codebook",
           downloadButton(ns("download_DDI"), "Download codebook"),
           tags$br(),
           tags$br(),
           verbatimTextOutput(ns("XML"))
  )
}

ddi_generation_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    output$XML <- renderText({
      req(dat())
      ddi <- generate_ddi_codebook(dat())
      ddi <- rddi::as_xml(ddi)
      ddi <- gsub("(<relPubl[^>]*>)((?!&lt;).+)(&lt;)", "\\1\\2\n        <", ddi, perl = TRUE)
      ddi <- gsub("(<relMat)([^>]*>)((?!&lt;).+)(&lt;)", "\\1\\2\\3\n        <", ddi, perl = TRUE)
      ddi <- gsub("(<relStdy)([^>]*>)((?!&lt;).+)(&lt;)", "\\1\\2\\3\n        <", ddi, perl = TRUE)
      ddi <- gsub("(<othRefs)([^>]*>)((?!&lt;).+)(&lt;)", "\\1\\2\\3\n        <", ddi, perl = TRUE)
      
      ddi <- gsub("(&gt;)(</relPubl>)", ">\n      \\2", ddi)
      ddi <- gsub("(&gt;)(</relMat>)", ">\n      \\2", ddi)
      ddi <- gsub("(&gt;)(</relStdy>)", ">\n      \\2", ddi)
      ddi <- gsub("(&gt;)(</othRefs>)", ">\n      \\2", ddi)
      
      # match &lt; where the &lt; is on the same line as the &gt;
      ddi <- gsub("([^ \n])(&lt;)", "\\1<", ddi)
      
      # double check what happens when it's non-mixed content
      ddi <- gsub("&lt;", "        <", ddi)
      ddi <- gsub("&gt;", ">", ddi)
      #ddi <- xml2::read_xml(ddi)
      ddi <- as.character(ddi)
    })
    
    output$download_DDI <- downloadHandler(
      filename = "codebook.xml",
      content = function(file) {
        ddi <- rddi::as_xml(generate_ddi_codebook(dat()))
        ddi <- gsub("&lt;", "<", ddi)
        ddi <- gsub("&gt;", ">", ddi)
        ddi <- xml2::read_xml(ddi)
        xml2::write_xml(ddi, file)
      },
      contentType = "text/xml"
    )
  })
}