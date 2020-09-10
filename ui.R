setwd('C:\\Users\\NGS\\Documents\\Simon\\Sahara')

library(shiny)
library(dplyr)
library(magrittr)
library(DT)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-11.0.2')
options(java.parameters = "- Xmx10240m")
library(xlsx)

ui <- fluidPage(
  titlePanel("Sahara - the IHC raw data converter (v2.0)"),

  sidebarPanel(
    fileInput(inputId = "input", label = "Select a file" ),
    textInput(inputId = "parameter", label = HTML(paste("Pixel to mm", tags$sup(2), sep = "")), value = 2.5e-7 ),
    checkboxGroupInput(inputId = 'TissueCategory', label = 'Tissue Category', selected = "all",
                 c("All" = "all", "Strama" = "Strama", "Tumor" = "Tumor")),
    
    ##20191030
    textOutput('tissuecat'),
    checkboxGroupInput(inputId = 'TissueCategory', label = 'Tissue Category', 
                       choices = textOutput('tissuecat')),
    
    actionButton(inputId = 'convert', label = 'Convert'),
    downloadButton("downloadData", "Download")
  ),

  mainPanel(
    #tableOutput("area_file"), #name to give to the output object
    tableOutput("input"), #name to give to the output object
    DT::dataTableOutput('converted')

  )

)
