#install.packages('ggpubr')
#library(ggpubr)



filename <- "20190115 2ed UC_cell_seg_data_summary.txt"
parameter <- 2.5e-7

input <- read.table(filename, sep = "\t", header = TRUE,
                    stringsAsFactors = FALSE, check.names = FALSE,
                    fill = TRUE)

input <- subset(input, select = c("Sample Name", "Tissue Category", "Phenotype", 
                           "Total Cells", "Tissue Category Area (pixels)"))
names(input) <- c("SampleName", "Tissue", "Phenotype", "Count", "Pixels")
input <- input %>% filter(Pixels != 0)
input$Area <- input$Pixels * parameter
input$SlideID <- lapply(strsplit(input$SampleName, "_"), "[", 1) %>% unlist()

phenotypes <- unique(input$Phenotype)
tissues <- unique(input$Tissue)
slideIDs <- unique(input$SlideID)
fields <- unique(input$SampleName)

## by SlideID
table1 <- matrix(0L, nrow = length(slideIDs), ncol = length(phenotypes))
colnames(table1) <- phenotypes
rownames(table1) <- slideIDs
for (ID in slideIDs){
    for (phenotype in phenotypes){
      area <- input$Area[input$SlideID == ID & input$Phenotype == phenotype] %>% sum()
      count <- input$Count[input$SlideID == ID & input$Phenotype == phenotype] %>% sum()
      density <- count / area
      table1[ID, phenotype] <- density
    }
}

table1 <- tibble::rownames_to_column(data.frame(table1), "SlideID") 


## by SlideID and Tissue

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
    density <- count / area
    table2[i, phenotype] <- density
  }
}

table2 <- cbind(pairs, table2)

## by Field
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



## by SlideID and Tissue

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

##### TEST #####
View(table1)
View(table2)
View(table3)
View(table4)


