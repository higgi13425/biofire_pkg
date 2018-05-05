# Function for analysis of GI BioFire text data
# Extracts dataframe from txt file

#Packages
library(tidyverse)
library(stringr)
library(lubridate)
library(readr)
library(pdftools)

# function clean_biofire_pdf, for cleaning .txt files from biofire machine
# test file is here:
# file <- "~/Documents/Rcode/biofire/lab_biofire_results_txt/FilmArray_Run_Date_2017_08_04_Sample_3113_SN_09937650.txt"

# requires packages tidyverse, stringr, readr
# lubridate, pdftools


#function clean_biofire_pdf, for cleaning pdf files from biofire machine
#requires packages pdftools, stringr, tidyr
#test file is here:
# file <- "~/Documents/Rcode/biofire/lab_biofire_results_pdf/FilmArray_Run_Date_2017_06_27_Sample_153_SN_09937928.pdf"
clean_biofire_pdf <- function(file){
  pdtext<- pdf_text(file)
  text2<- str_extract(pdtext, "Bacteria\n[\\s\\S]*Sapovirus")
  text2 <- read.csv(text=text2, stringsAsFactors=FALSE, strip.white = TRUE)
  text2$Bacteria[grepl("N/A", text2$Bacteria)] <- "Not Detected E. coli O157"
  text2 <- as.data.frame(text2[str_detect(text2$Bacteria, "Detect"),])
  names(text2) <- "Bacteria"
  text2 <- text2 %>% separate(Bacteria, c("detect","bug"), sep="ed ")
  text2$detect<- paste0(text2$detect, "ed")
  text2$bug<- str_trim(text2$bug)
  text2  
}
