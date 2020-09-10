setwd("/Users/simon/GoogleDrive/NTUH/Projects/Sahara/v2.0")
setwd('C:/Users/NGS/GoogleDrive/NTUH/Projects/Sahara/v2.0')

library(dplyr)
library(stringr)
library(magrittr)

file <- read.table("20190115 2ed UC_cell_seg_data_summary.txt",
                   sep = "\t", header = TRUE,
                   stringsAsFactors = FALSE, fill = TRUE)

row_sub = file$Tissue.Category.Area..pixels. != 0
file <- file[row_sub,]


phenotypes <- unique(file$Phenotype)
categories <- unique(file$Tissue.Category)

samples_add <- file$Sample.Name
slides_add <- str_extract(samples_add, "(\\d+-\\d+)")
file$SlideID <- slides_add

samples <- unique(file$Sample.Name)
slides <- unique(file$SlideID)

parameter <- 2.5e-7

### Conver pixel to area > Fixed_Area
file$Fixed_Area <- file$Tissue.Category.Area..pixels. * parameter

area_by_slide_tissue <- list()
for (slide in slides){
  for (tissue in categories){
    area_by_slide_tissue[[slide]][[tissue]] <- file %>%
      filter(SlideID == slide, Tissue.Category == tissue) %>%
      pull(Tissue.Category.Area..pixels.) %>%
      sum() * parameter
  }
}

area_by_sample_tissue <- list()
for (smaple in samples){
  for (tissue in categories){
    area_by_sample_tissue[[smaple]][[tissue]] <- file %>%
      filter(Sample.Name == smaple, Tissue.Category == tissue) %>%
      pull(Tissue.Category.Area..pixels.) %>%
      sum() * parameter
  }
}

### Table1 bySlide
table1 <- matrix(0L, nrow = length(slides), ncol = length(phenotypes))
colnames(table1) <- phenotypes
rownames(table1) <- slides

for (slide in slides){
  slide.des.sum <- filter(file, SlideID == slide)
  slide.des.sum <- sum(slide.des.sum$Fixed_Area)
  for (phenotype in phenotypes){
    aa <- file %>%
      filter(SlideID == slide, Phenotype == phenotype) %>%
      pull(Total.Cells) %>%
      sum() / slide.des.sum

    table1[slide, phenotype] <- aa
    }
}


### Table2 bySlide - Tissue
# add new colume: uid.tissue
file$uid.tissue <- paste(file$SlideID, file$Tissue.Category, sep = ":")
# extract uniid
uniid <- unique(file$uid.tissue)

table2 <- matrix(0L, nrow = length(uniid), ncol = length(phenotypes))
colnames(table2) <- phenotypes
rownames(table2) <- uniid

for (uid in uniid){
  uidnames <- strsplit(uid, ":")[[1]]
  for (phenotype in phenotypes){
      bb <- file %>%
        filter(SlideID == uidnames[1], Phenotype == phenotype,
               Tissue.Category == uidnames[2]) %>%
        pull(Total.Cells) %>%
        sum() / area_by_slide_tissue[[uidnames[1]]][[uidnames[2]]]
      table2[uid, phenotype] <- bb
  }
}


infoCols <- t(data.frame(sapply(rownames(table2),
                                function(x) strsplit(x, ":")[[1]])))

colnames(infoCols) <- c("SlideID", "Tissue")

table2 <- cbind(infoCols, table2)


### Table3 byField
table3 <- matrix(0L, nrow = length(samples), ncol = length(phenotypes))
colnames(table3) <- phenotypes
rownames(table3) <- samples

for (sample in samples){
  sample.des.sum <- filter(file, Sample.Name == sample)
  sample.des.sum <- sum(sample.des.sum$Fixed_Area)
  for (phenotype in phenotypes){
    cc <- file %>%
      filter(Sample.Name == sample, Phenotype == phenotype) %>%
      pull(Total.Cells) %>%
      sum() / sample.des.sum

    table3[sample, phenotype] <- cc
  }
}

### Table4 byField - Tissue
uniid4 <- unique(paste(file$Sample.Name, file$Tissue.Category, sep = ":"))

table4 <- matrix(0L, nrow = length(uniid4), ncol = length(phenotypes))
colnames(table4) <- phenotypes
rownames(table4) <- uniid4

for (uid in uniid4){
  uidnames <- strsplit(uid, ":")[[1]]
  for (phenotype in phenotypes){
    dd <- file %>%
      filter(Sample.Name == uidnames[1], Phenotype == phenotype,
             Tissue.Category == uidnames[2]) %>%
      pull(Total.Cells) %>%
      sum() / area_by_sample_tissue[[uidnames[1]]][[uidnames[2]]]
    table4[uid, phenotype] <- dd
  }
}

infoCols <- t(data.frame(sapply(rownames(table4),
                                function(x) strsplit(x, ":")[[1]])))

colnames(infoCols) <- c("Sample", "Tissue")

table4 <- cbind(infoCols, table4)
