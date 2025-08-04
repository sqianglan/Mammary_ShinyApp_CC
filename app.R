library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
#library(DESeq2)
library(ggpubr)
library(ggbeeswarm)
#library(patchwork)
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
      menuItem("Epithelium", tabName = "epithelium", icon = icon("grip-horizontal")),
      menuItem("Mesenchyme", tabName = "mesenchyme", icon = icon("dna"))
      #menuItem("ATAC-seq", tabName = "atacseq", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .sidebar-menu > li > a {
          font-size: 16px !important;
        }
        .sidebar-menu .treeview-menu li a {
          font-size: 14px !important;
        }
      "))
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
  
  # Initialize all modules
  tab_summaryServer("summary")
  tab_mesenchymeServer("mesenchyme", parent_session = session)
  tab_epitheliumServer("epithelium", parent_session = session)
  #tab_atacseqServer("atacseq")
}

shinyApp(ui = ui, server = server)