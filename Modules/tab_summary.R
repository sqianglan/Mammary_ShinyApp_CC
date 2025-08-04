library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)

tab_summaryUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$script(HTML("
      function openTab(tabName) {
        // Update the Shiny input
        Shiny.setInputValue('tabs', tabName, {priority: 'event'});
        
        // Also trigger shinydashboard tab switching directly
        setTimeout(function() {
          var tabLink = $('a[data-value=\"' + tabName + '\"]');
          if (tabLink.length > 0) {
            tabLink.click();
          }
        }, 100);
      }
    ")),
    
    # Top section with logo
    fluidRow(
      column(9,
        h2("Embryonic Mammary Gland RNA-seq Analysis", style = "margin-top: 20px;")
      ),
      column(3,
        div(style = "text-align: right; margin-top: 10px;",
          img(src = "HY__LD01_LogoFP_EN_B3____BW.png", height = "120px", alt = "University of Helsinki Logo")
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Project Overview", status = "primary", solidHeader = TRUE, width = 12,
        div(style = "text-align: center; margin-bottom: 20px;",
          img(src = "Embryonic mamamry gland development-v3_E10-E19.png", width = "80%", alt = "Embryonic Mammary Gland Development E10-E19"),
          br(),
          p(style = "font-size: 13px; color: #666; margin-top: 10px; font-style: italic;",
            "Schematic diagram adapted from ",
            tags$a("Dr. Ewelina Trela's doctoral dissertation", href = "http://hdl.handle.net/10138/333953", target = "_blank", style = "color: #007bff;"), "."
          )
        ),
        p("The mammary gland is a unique organ that undergoes dynamic alterations throughout a female's reproductive life, making it an ideal model for developmental, stem cell and cancer biology research. Mammary gland development begins in utero and proceeds via a quiescent bud stage before the initial outgrowth and subsequent branching morphogenesis. How mammary epithelial cells transit from quiescence to an actively proliferating and branching tissue during embryogenesis and, importantly, how the branch pattern is determined remain largely unknown.", style = "font-size: 16px; margin-bottom: 15px; text-align: justify;"),
        p("This interactive application summarized embryonic mammary gland RNA sequencing data from two key studies conducted in the ", 
          tags$a("Marja Mikkola Group", href = "https://www.helsinki.fi/en/researchgroups/epithelial-morphogenesis", target = "_blank", style = "font-weight: bold;"), 
          "at Institute of Biotechnology, HiLIFE, University of Helsinki, on the epithelium and mesenchyme compartments, respectively.", style = "font-size: 16px;"),
        br(),
        h4("Datasets and Citations:", style = "font-size: 18px;"),
        tags$ul(
          tags$li(strong("Epithelium Data:"), tags$em("\"Stabilization of Epithelial β-Catenin Compromises Mammary Cell Fate Acquisition and Branching Morphogenesis\""), " Satta JP, Lan Q, Taketo MM, Mikkola ML. ", tags$a("J Invest Dermatol. 2024;144(6):1223-1237.e10", href = "https://doi.org/10.1016/j.jid.2023.11.018", target = "_blank"), " | GEO: ", tags$a("GSE236630", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE236630", target = "_blank"), style = "font-size: 15px;"),
          tags$li(strong("Mesenchyme Data:"), tags$em("\"Mesenchyme instructs growth while epithelium directs branching in the mouse mammary gland\""), " Lan Q, Trela E, Lindström R, Satta JP, Kaczyńska B, Christensen MM, Holzenberger M, Jernvall J, Mikkola ML. ", tags$a("eLife. 2024;13:e93326", href = "https://doi.org/10.7554/eLife.93326", target = "_blank"), " | GEO: ", tags$a("GSE225821", href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE225821", target = "_blank"), style = "font-size: 15px;")
        ),
        br(),
        h4("Available Analysis Tabs:", style = "font-size: 18px;"),
        tags$ul(
          tags$li(tags$a("Epithelium", href = "#", onclick = "openTab('epithelium')", style = "font-weight: bold; color: #007bff; text-decoration: none;"), ": Analysis of gene expression in the mammary epithelium (Satta et al.).", style = "font-size: 15px;"),
          tags$li(tags$a("Mesenchyme", href = "#", onclick = "openTab('mesenchyme')", style = "font-weight: bold; color: #007bff; text-decoration: none;"), ": Analysis of gene expression in the mammary mesenchyme (Lan et al.).", style = "font-size: 15px;")
        ),
        br(),
        div(style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #007bff; margin-top: 20px;",
          h5("Contact Information", style = "color: #007bff; margin-bottom: 10px;"),
          p(style = "font-size: 14px; margin-bottom: 5px;",
            strong("Project-related issues:"), " Contact ", 
            tags$a("Marja Mikkola", href = "mailto:marja.mikkola@helsinki.fi", style = "color: #007bff;")
          ),
          p(style = "font-size: 14px; margin-bottom: 0px;",
            strong("Technical/website issues:"), " Contact ", 
            tags$a("Qiang Lan", href = "mailto:qiang.lan@bristol.ac.uk", style = "color: #007bff;")
          )
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Epithelium Dataset Summary (Satta et al.)", status = "info", solidHeader = TRUE, width = 6,
        tableOutput(ns("epithelium_summary"))
      ),
      
      box(
        title = "Mesenchyme Dataset Summary (Lan et al.)", status = "info", solidHeader = TRUE, width = 6,
        tableOutput(ns("mesenchyme_summary"))
      )
    ),
    
    # Funding section
    fluidRow(
      box(
        title = "FUNDING", status = "success", solidHeader = TRUE, width = 12,
        div(style = "text-align: center;",
          fluidRow(
            column(12,
                div(style = "display: flex; flex-wrap: wrap; justify-content: center; align-items: center; gap: 15px;",
                div(style = "flex: 1 1 auto; margin: 7px; display: flex; align-items: center; justify-content: center;",
                  img(src = "sigrid_juselius_logo.png", height = "100px", alt = "Sigrid Jusélius Foundation")
                ),
                div(style = "flex: 1 1 auto; margin: 7px; display: flex; align-items: center; justify-content: center;",
                  img(src = "cancer_society_of_finland_2.logo.png", height = "100px", alt = "Cancer Society of Finland")
                ),
                div(style = "flex: 1 1 auto; margin: 7px; display: flex; align-items: center; justify-content: center;",
                  img(src = "OÖ-logo-1944-4.png", height = "100px", alt = "Oskar Öflunds Stiftelse")
                ),
                div(style = "flex: 1 1 auto; margin: 7px; display: flex; align-items: center; justify-content: center;",
                  img(src = "suomen-kulttuurirahasto-logo-nelio-harmaa.png", height = "100px", alt = "Finnish Cultural Foundation")
                ),
                div(style = "flex: 1 1 auto; margin: 7px; display: flex; align-items: center; justify-content: center;",
                  img(src = "ejag.logo.png", height = "100px", alt = "Ella ja Georg Ehrnroothin Säätiö")
                ),
                div(style = "flex: 1 1 auto; margin: 7px; display: flex; align-items: center; justify-content: center;",
                  img(src = "ils-logo.jpg.png", height = "100px", alt = "ILS")
                ),
                div(style = "flex: 1 1 auto; margin: 7px; display: flex; align-items: center; justify-content: center;",
                  img(src = "AKA_FI_SE_ENG_sininen_RGB_logo.png", height = "100px", alt = "Academy of Finland")
                ),
                div(style = "flex: 1 1 auto; margin: 7px; display: flex; align-items: center; justify-content: center;",
                  img(src = "hilife-logo-square-black-text.png", height = "100px", alt = "HiLIFE")
                )
                )
            )
          )
        )
      )
    ),
    
    # Copyright section
    fluidRow(
      column(12,
        div(style = "text-align: center; padding: 20px 0; border-top: 1px solid #dee2e6; margin-top: 20px;",
          p(style = "font-size: 11px; color: #6c757d; margin-bottom: 0px;",
            "© 2025 Qiang Lan & Marja Mikkola Group. Licensed under BSD 3-Clause License. All rights reserved."
          )
        )
      )
    )
    
  )
}

tab_summaryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Load Epithelium Sample Data
    sample_data_epithelium <- reactive({
      tryCatch({
        read.csv("rawData/Satta_et_al/sampleTable.csv", stringsAsFactors = TRUE)
      }, error = function(e) {
        data.frame(Error = "Failed to load Satta et al. sample data.")
      })
    })
    
    # Load Mesenchyme Sample Data
    sample_data_mesenchyme <- reactive({
      tryCatch({
        read.csv("rawData/Lan_et_al/All_data/sampleTable.csv", stringsAsFactors = TRUE)
      }, error = function(e) {
        data.frame(Error = "Failed to load Lan et al. sample data.")
      })
    })
    
    output$epithelium_summary <- renderTable({
      data <- sample_data_epithelium()
      
      if ("Error" %in% names(data)) {
        data.frame(
          Metric = "Error",
          Value = "Failed to load data"
        )
      } else {
        data.frame(
          Metric = c("Total Samples", "Unique Groups", "Time Points", "Conditions"),
          Value = c(
            nrow(data),
            length(unique(data$group)),
            "E13.5, E16.5",
            "WT, Stab β-catenin"
          )
        )
      }
    }, striped = TRUE, hover = TRUE)
    
    output$mesenchyme_summary <- renderTable({
      data <- sample_data_mesenchyme()
      
      if ("Error" %in% names(data)) {
        data.frame(
          Metric = "Error",
          Value = "Failed to load data"
        )
      } else {
        unique_groups <- unique(data$groups)
        data.frame(
          Metric = c("Total Samples", "Unique Groups", "Time Points", "Tissue Types"),
          Value = c(
            nrow(data),
            length(unique_groups),
            "E13.5 or E16.5",
            "Skin Mesenchyme,Mammary Mesenchyme, Fatpad, Submandibular gland (SMG) Mesenchyme"
          )
        )
      }
    }, striped = TRUE, hover = TRUE)
    
  })
}