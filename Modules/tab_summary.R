tab_summaryUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(
        title = "Project Overview", status = "primary", solidHeader = TRUE, width = 12,
        h3("Embryonic Mammary RNA-seq Analysis"),
        p("This interactive application analyzes embryonic mammary gland RNA sequencing data from the Satta_et_al. study."),
        br(),
        h4("Dataset Information:"),
        tags$ul(
          tags$li("Species: Mouse"),
          tags$li("Tissue: Embryonic mammary gland"),
          tags$li("Time points: E13.5 and E16.5"),
          tags$li("Conditions: Wild type (WT) and Stabilized β-catenin (Stab_bcat)")
        ),
        br(),
        h4("Available Analysis Tabs:"),
        tags$ul(
          tags$li(strong("Epithelium:"), "Gene expression analysis with statistical comparisons"),
          tags$li(strong("Mesenchyme:"), "Mesenchymal tissue analysis"),
          tags$li(strong("ATAC-seq:"), "Chromatin accessibility data")
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Sample Information", status = "info", solidHeader = TRUE, width = 6,
        DT::dataTableOutput(ns("sample_table"))
      ),
      
      box(
        title = "Dataset Statistics", status = "info", solidHeader = TRUE, width = 6,
        tableOutput(ns("dataset_stats"))
      )
    )
  )
}

tab_summaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    sample_data <- reactive({
      tryCatch({
        read.csv("rawData/Satta_et_al/sampleTable.csv", stringsAsFactors = TRUE)
      }, error = function(e) {
        data.frame(
          sampleName = c("Sample_1", "Sample_2", "Sample_3"),
          group = c("E13.5_WT", "E16.5_WT", "E13.5_Stab_bcat"),
          condition = c("WT", "WT", "Stab_bcat")
        )
      })
    })
    
    output$sample_table <- DT::renderDataTable({
      DT::datatable(
        sample_data(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    output$dataset_stats <- renderTable({
      data <- sample_data()
      stats <- data.frame(
        Metric = c("Total Samples", "Unique Groups", "Time Points", "Conditions"),
        Value = c(
          nrow(data),
          length(unique(data$group)),
          "E13.5, E16.5",
          "WT, Stab_bcat"
        )
      )
      stats
    }, striped = TRUE, hover = TRUE)
    
  })
}