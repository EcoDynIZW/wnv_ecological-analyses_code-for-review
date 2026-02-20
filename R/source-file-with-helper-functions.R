

## ------------------------------- Load libraries ---------------------

#devtools::install_github("EcoDynIZW/d6")
library(d6)
#d6::install_d6_packages(geo = TRUE)
library(remotes)

library(here) ;here()
library(tidyverse)
library(tidyr)

## plot helpers
library(patchwork)
library(ggplot2)
library(gridExtra)
library(viridis)
library(flextable)

## additional libraries:
library(lubridate)
library(vroom)
library(readxl)

## analysis
library(iNEXT)
library(vegan)
library(pheatmap)
library(ggcorrplot)

library(modelr)
library(GGally)
library(ggrepel)


## ------------------ Set themes -------------------------------


today <- Sys.Date()
today

########################### DATA YEAR SOLLTE SCHON GESETZT SEIN DAFÜR!

## create output directory with most recent analysis results
dir_output_path <- here('output', paste0('BUAdata_',data_year,'_analysisDate_',today) )
ifelse(!dir.exists(file.path(dir_output_path)), 
       dir.create(file.path(dir_output_path)), FALSE)

dir_plot_path <- here('plots', paste0('BUAdata_',data_year,'_analysisDate_',today) )
ifelse(!dir.exists(file.path(dir_plot_path)), 
       dir.create(file.path(dir_plot_path)), FALSE)

#theme_set(d6::theme_d6(base_size = 18)) ##TODO does not work, does not plot axes
theme_set(theme_bw(base_size = 18))

site.cols  <- c("#90EE90", "#006400","#FF7F50","#6CA6CD","#CDCD00")
site.names <- c("C",       "N",      "RA",     "RB",     "S")

