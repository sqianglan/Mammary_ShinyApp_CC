library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(DESeq2)
library(ggpubr)
library(ggbeeswarm)
library(patchwork)
library(shinycssloaders)

# Source UI and server modules
source("Modules/tab_summary.R", encoding = "UTF-8")
source("Modules/tab_mesenchyme.R", encoding = "UTF-8")
source("Modules/tab_mfuzzy_cluster.R", encoding = "UTF-8")
source("Modules/tab_tissue_markers.R", encoding = "UTF-8")
source("Modules/tab_epithelium.R", encoding = "UTF-8")
source("Modules/analytics_tracker.R", encoding = "UTF-8")

ui <- dashboardPage(
  dashboardHeader(title = "Embryonic Mammary RNA-seq Analysis"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Home", tabName = "summary", icon = icon("home")),
      menuItem("Epithelium", tabName = "epithelium", icon = icon("dna")),
      menuItem("Mesenchyme", tabName = "mesenchyme", icon = icon("microscope"))
      #menuItem("ATAC-seq", tabName = "atacseq", icon = icon("search"))
    ),
    br(),
    div(style = "text-align: center; padding: 10px;",
      actionButton("show_analytics_modal", "Admin Access", 
                   class = "btn-warning btn-sm", 
                   icon = icon("key"),
                   style = "width: 90%;")
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    # Analytics access modal
    bsModal("analytics_modal", "Analytics Dashboard", "show_analytics_modal", size = "large",
      conditionalPanel(
        condition = "!output.analytics_authenticated",
        div(style = "text-align: center; padding: 20px;",
          h4("Administrator Access Required"),
          p("Enter the administrator password to access the analytics dashboard:"),
          div(style = "display: flex; justify-content: center; margin: 20px 0;",
            passwordInput("admin_password", "Password:", placeholder = "Enter admin password", width = "300px")
          ),
          actionButton("admin_login", "Access Analytics", class = "btn-primary", icon = icon("unlock")),
          br(), br(),
          textOutput("admin_message")
        )
      ),
      conditionalPanel(
        condition = "output.analytics_authenticated",
        analytics_ui("modal_analytics")
      )
    ),
    
    tabItems(
      tabItem(tabName = "summary",
        tab_summaryUI("summary")
      ),
      
      tabItem(tabName = "epithelium",
        tab_epitheliumUI("epithelium")
      ),
      
      tabItem(tabName = "mesenchyme",
        tab_mesenchymeUI("mesenchyme")
      )
      
      #tabItem(tabName = "atacseq",
      #  tab_atacseqUI("atacseq")
      #)
    )
  )
)

server <- function(input, output, session) {
  # Log visitor on app start
  log_visitor(session)
  
  # Analytics authentication state
  analytics_authenticated <- reactiveVal(FALSE)
  
  # Handle admin login from modal
  observeEvent(input$admin_login, {
    if (input$admin_password == "mikkola2024") {
      analytics_authenticated(TRUE)
      output$admin_message <- renderText("")
      updateTextInput(session, "admin_password", value = "")
    } else {
      output$admin_message <- renderText("Incorrect password. Please try again.")
    }
  })
  
  # Send authentication state to UI
  output$analytics_authenticated <- reactive({
    analytics_authenticated()
  })
  outputOptions(output, "analytics_authenticated", suspendWhenHidden = FALSE)
  
  # Initialize all modules
  tab_summaryServer("summary")
  tab_mesenchymeServer("mesenchyme", parent_session = session)
  tab_epitheliumServer("epithelium", parent_session = session)
  analytics_server("modal_analytics")
  #tab_atacseqServer("atacseq")
}

shinyApp(ui = ui, server = server)