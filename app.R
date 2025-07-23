library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(DESeq2)
library(ggpubr)
library(ggbeeswarm)
library(patchwork)

# Source UI and server modules
source("Modules/tab_summary.R", encoding = "UTF-8")
source("Modules/tab_mesenchyme.R", encoding = "UTF-8")
source("Modules/tab_atacseq.R", encoding = "UTF-8")
source("Modules/tab_epithelium.R", encoding = "UTF-8")

ui <- dashboardPage(
  dashboardHeader(title = "Embryonic Mammary RNA-seq Analysis"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Summary", tabName = "summary", icon = icon("chart-line")),
      menuItem("Epithelium", tabName = "epithelium", icon = icon("dna")),
      menuItem("Mesenchyme", tabName = "mesenchyme", icon = icon("microscope")),
      menuItem("ATAC-seq", tabName = "atacseq", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
        tab_summaryUI("summary")
      ),
      
      tabItem(tabName = "epithelium",
        tab_epitheliumUI("epithelium")
      ),
      
      tabItem(tabName = "mesenchyme",
        tab_mesenchymeUI("mesenchyme")
      ),
      
      tabItem(tabName = "atacseq",
        tab_atacseqUI("atacseq")
      )
    )
  )
)

server <- function(input, output, session) {
  # Initialize all modules
  tab_summaryServer("summary")
  tab_mesenchymeServer("mesenchyme")
  tab_atacseqServer("atacseq")
  tab_epitheliumServer("epithelium", parent_session = session)
}

shinyApp(ui = ui, server = server)