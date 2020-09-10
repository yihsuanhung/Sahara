setwd('C:\\Users\\NGS\\Documents\\Simon\\Sahara')

library(shiny)
library(dplyr)
library(magrittr)
library(DT)
#library(ggpubr)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-11.0.2')
options(java.parameters = "- Xmx10240m")
library(xlsx)
options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output) {

  data <- reactive({
    inFile <- input$input ##
    if (is.null(inFile))
      return(NULL)
    input <- read.table(inFile$datapath, sep = "\t", header = TRUE,
                     stringsAsFactors = FALSE, check.names = FALSE,
                     fill = TRUE)
    
    input <- subset(input, select = c("Sample Name", "Tissue Category", "Phenotype", 
                                      "Total Cells", "Tissue Category Area (pixels)"))
    names(input) <- c("SampleName", "Tissue", "Phenotype", "Count", "Pixels")
    input <- input %>% filter(Pixels != 0)
    #cd <- cd[cd$`Tissue Category Area (pixels)` != 0,]
    return(input)
  })


  conversion <- eventReactive(input$convert,{
    parameter <- as.numeric(input$parameter)
    
    # Read the file
    inFile <- input$input
    input <- read.table(inFile$datapath, sep = "\t", header = TRUE,
                       stringsAsFactors = FALSE, check.names = FALSE,
                       fill = TRUE)
    input <- subset(input, select = c("Sample Name", "Tissue Category", "Phenotype", 
                                      "Total Cells", "Tissue Category Area (pixels)"))
    names(input) <- c("SampleName", "Tissue", "Phenotype", "Count", "Pixels")
    input <- input %>% filter(Pixels != 0)
    
    # Add new columes: Area & SlideID
    
    input$Area <- input$Pixels * parameter
    input$SlideID <- lapply(strsplit(input$SampleName, "_"), "[", 1) %>% unlist()
    
    phenotypes <- unique(input$Phenotype)
    tissues <- unique(input$Tissue)
    slideIDs <- unique(input$SlideID)
    fields <- unique(input$SampleName)
    


    ##### Table 1 ##### bySlide #####
    table1 <- matrix(0L, nrow = length(slideIDs), ncol = length(phenotypes))
    colnames(table1) <- phenotypes
    rownames(table1) <- slideIDs
    for (ID in slideIDs){
      for (phenotype in phenotypes){
        area <- input$Area[input$SlideID == ID & input$Phenotype == phenotype] %>% sum()
        count <- input$Count[input$SlideID == ID & input$Phenotype == phenotype] %>% sum()
        if (area > 0){
          density <- count / area
          table1[ID, phenotype] <- density
        }
      }
    }
    
    #table1 <- tibble::rownames_to_column(data.frame(table1), "SlideID") 
    

    ##### Table 2 ##### bySlide - Tissue #####
    pairs <- expand.grid(tissues, slideIDs)
    names(pairs) <- c("Tissue", "SlideID")
    pairs <- pairs[, c(2, 1)]
    table2 <- matrix(0L, nrow = nrow(pairs), ncol = length(phenotypes))
    colnames(table2) <- phenotypes
    
    for (i in seq(nrow(pairs))){
      ID <- as.character(pairs[i, 1])
      tissue <- as.character(pairs[i, 2])
      for (phenotype in phenotypes){
        query <- input %>%
          filter(SlideID == ID, Phenotype == phenotype, Tissue == tissue)
        area <- query %>% pull(Area) %>% sum()
        count <- query %>% pull(Count) %>% sum()
        if (area > 0){
          density <- count / area
          table2[i, phenotype] <- density 
        }
      }
    }
    
    table2 <- cbind(pairs, table2)

    
    ##### Table 3 ##### byField #####
    table3 <- matrix(0L, nrow = length(fields), ncol = length(phenotypes))
    colnames(table3) <- phenotypes
    rownames(table3) <- fields
    for (field in fields){
      for (phenotype in phenotypes){
        area <- input$Area[input$SampleName == field & input$Phenotype == phenotype] %>% sum()
        count <- input$Count[input$SampleName == field & input$Phenotype == phenotype] %>% sum()
        if (area > 0){
          density <- count / area
          table3[field, phenotype] <- density
        }
      }
    }
    
    #table3 <- tibble::rownames_to_column(data.frame(table3), "SampleName") 
    
    
    ##### Table 4 ##### byField - Tissue #####
    pairs <- expand.grid(tissues, fields)
    names(pairs) <- c("Tissue", "Field")
    pairs$SlideID <- lapply(strsplit(as.character(pairs$Field), "_"), "[", 1) %>% unlist()
    
    pairs <- pairs[, c(3, 2, 1)]
    table4 <- matrix(0L, nrow = nrow(pairs), ncol = length(phenotypes))
    colnames(table4) <- phenotypes
    
    for (i in seq(nrow(pairs))){
      field <- as.character(pairs[i, 2])
      tissue <- as.character(pairs[i, 3])
      for (phenotype in phenotypes){
        query <- input %>%
          filter(SampleName == field, Phenotype == phenotype, Tissue == tissue)
        area <- query %>% pull(Area) %>% sum()
        count <- query %>% pull(Count) %>% sum()
        if (area > 0){
          density <- count / area
          table4[i, phenotype] <- density
        }
      }
    }
    
    table4 <- cbind(pairs, table4)

    
    ##### Return #####
    return_list <- list('table1' = table1, 'table2' = table2,
                        'table3' = table3, 'table4' = table4)

    return(return_list)

  })






  output$input <- renderTable({
    head(data(), 7)

  })
  
  output$tissuecat <- renderText({unique(data()$Tissue)}) ##20191030

  output$converted <- DT::renderDataTable({

    # Create a Progress object

    progress <- shiny::Progress$new(style = 'old')
    progress$set(message = "Converting...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())

    #conversion()
    conversion <- conversion()
    round(conversion$table1, 2)

  })


  output$downloadData <- downloadHandler(

    filename = function() { 'output.xlsx' },

    content = function(file) {

      conversion <- conversion()

      tempFile <- tempfile(fileext = ".xlsx")

      write.xlsx(conversion$table1, tempFile, sheetName = 'bySlide', row.names=TRUE, append = TRUE)
      write.xlsx(conversion$table2, tempFile, sheetName = 'bySlide - Tissue', row.names = FALSE, append = TRUE)
      write.xlsx(conversion$table3, tempFile, sheetName = 'byField', row.names=TRUE, append = TRUE)
      write.xlsx(conversion$table4, tempFile, sheetName = 'byField - Tissue', row.names = FALSE, append = TRUE)

      file.rename(tempFile, file)

    },

    contentType = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'

  )
}
