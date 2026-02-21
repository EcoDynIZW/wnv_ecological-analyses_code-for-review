################################################################################
## ------------------------------- WRAPPER     ----------------------------##
################################################################################

## run all code and years at once!
library(knitr)

FLAG_run <- 1 ## set to 0 if all code scripts are run by hand
data_year_ct <- c(2023,2024,9999)

knitr::knit(input = here::here('R','Fig5_S1_communityMetrics_20260220.Rmd'), 
            output =here::here('R','Fig5_S1_communityMetrics_20260220.R') ,
            tangle=TRUE)

knitr::knit(here::here('R','Fig6_bloodmealCorrelations_20260218.Rmd'), 
            here::here('R','Fig6_bloodmealCorrelations_20260218.R'),
            tangle=TRUE)

knitr::knit(here::here('R','Fig7_S2_PCA_20260218.Rmd'), 
            here::here('R','Fig7_S2_PCA_20260218.R'),
            tangle=TRUE)


if (FLAG_run == 1)
{
  for (ii in 1:length(data_year_ct)) {
    
    data_year <- data_year_ct[ii]
    print(data_year)
    
    ## needs to be here so that folders are created for respective data_year
    source(here::here('R','source-file-with-helper-functions.R')) 
    
    ## the three scripts
    source(here::here('R','Fig5_S1_communityMetrics_20260220.R'))   ### TODO pdfs are not saved!
    source(here::here('R','Fig6_bloodmealCorrelations_20260218.R'))
    source(here::here('R','Fig7_S2_PCA_20260218.R'))

  }
  print("done")
}
dev.off()
