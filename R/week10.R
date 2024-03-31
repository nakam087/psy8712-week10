# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)

# Data Import and Cleaning
gss_tbl<-read_sav("../data/GSS2016.sav")%>% #it seems like all the IAP and DKs are already marked as NA's?
  filter(!is.na(MOSTHRS))%>% #remove NAs from MOSTHRS
  mutate(work_hours=MOSTHRS)%>% #renamed column
  select(-HRS1,-HRS2)%>% #removed these 2 columns
  select(where(~mean(is.na(.))<0.75)) #removed columns that had less than 75% NA's

# Visualization
ggplot(gss_tbl,aes(work_hours))+ #plotted as a barplot because only 1 continuous variable
  geom_bar()

# Analysis  

  
                