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

source("Modules/tab_summary.R")
source("Modules/tab_epithelium.R")
source("Modules/tab_mesenchyme.R")
source("Modules/tab_atacseq.R")

ui <- dashboardPage(
  dashboardHeader(title = "Embryonic Mammary RNA-seq Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
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
  tab_summaryServer("summary")
  tab_epitheliumServer("epithelium")
  tab_mesenchymeServer("mesenchyme")
  tab_atacseqServer("atacseq")
}

shinyApp(ui = ui, server = server)