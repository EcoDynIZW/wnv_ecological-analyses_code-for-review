
################################################################################
## ------------------------------- Load libraries ----------------------------##
################################################################################

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
library(grid) ## for pheatmap-package

#install.packages("FactoMineR")
library("FactoMineR")
#install.packages("corrr")
library("corrr")
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("factoextra")
library("factoextra")

library(knitr)

################################################################################
## ---------------------------- Settings -------------------------------------##
################################################################################


today <- Sys.Date()
today

## data_year should be set in main files or wrapper

## create output directory with most recent analysis results
dir_output_path <- here('output', paste0('BUAdata_',data_year,'_analysisDate_',today) )
ifelse(!dir.exists(file.path(dir_output_path)), 
       dir.create(file.path(dir_output_path)), FALSE)

dir_plot_path <- here('plots', paste0('BUAdata_',data_year,'_analysisDate_',today) )
ifelse(!dir.exists(file.path(dir_plot_path)), 
       dir.create(file.path(dir_plot_path)), FALSE)

#theme_set(d6::theme_d6(base_size = 18,base_family = ""))) ##TODO does not work, does not plot axes
theme_set(theme_bw(base_size = 18))

site.cols  <- c("#90EE90", "#006400","#FF7F50","#6CA6CD","#CDCD00")
site.names <- c("C",       "N",      "RA",     "RB",     "S")


### --------------------------------------------------------------------###
### --------------------------LOAD DATA---------------------------------###
### --------------------------------------------------------------------###

###----------------- ----      2023      ------------------------------ ###

if (data_year == 2023) {
  
  mymosq <- read.table(here("data", "2023_data_mosquitoes_20240313.csv"), 
                      sep = "\t", quote = "\"'", dec = ".", header = TRUE)
  
  myenvblood <- read.table(here("data",  
                           "BE23_BUA_correlation_data20260218.csv"), 
                      sep = ";", quote = "\"'", dec = ",", header = TRUE)
  
  
  } else
  
  ## ------------------------   2024      ------------------------------- ###
  if (data_year == 2024) {
    
    mymosq <- read.table(here("data", "2024_data_mosquitoes_20260109.csv"), 
                        sep = "\t", quote = "\"'", dec = ".", header = TRUE)
    
    myenvblood <- read.table(here("data",  
                             "BE24_BUA_correlation_data20260218.csv"), 
                        sep = ";", quote = "\"'", dec = ",", header = TRUE)

    } else {
    
    ## ------------------------   2023 and 2024      ----------------------###
    
      mymosq <- read.table(here("data",  
                             "9999_data_mosquitoes_20260219.csv"), 
                        sep = "\t", quote = "\"'", dec = ",", header = TRUE)

      myenvblood <- read.table(here("data",  
                             "BE23-24_BUA_correlation_data20260218.csv"), 
                        sep = ";", quote = "\"'", dec = ",", header = TRUE)
      }

### --------- relative to loaded year take environmental variables ------###
myenv <- myenvblood[, c(1:18)]


##### -----------------        modelled bird abundances      ------------  #####
## Planillo et al. 2021 Div Dist
ab_aves <- read.table(here("data", "bird_abundance_per_site_modelled.csv"), 
                      sep = "\t", dec = ".", header = TRUE)

################################################################################
