params <-
list(date = structure(20505, class = "Date"))

## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, 
                      dev = "ragg_png", fig.width = 9, fig.height = 6, dpi = 600, retina = 1)
Sys.setlocale("LC_TIME", "C")


## --------------------------------------------------------------------------------------------------------------------------------------

if (FLAG_run == 0) {
 data_year <- 2023 ## set at the beginning - set to 9999 if data_years are combined
 source(here::here('R','source-file-with-helper-functions.R'))
}
suffix_for_plot <- "_bloodmeal_"


## --------------------------------------------------------------------------------------------------------------------------------------
str(myenvblood)
num_df <- myenvblood[,-1] ## delete the non-numeric site variable

## check if there are variance issues - TODO - geht sicher in einer Zeile!
## only concerns 2024 data
tmp1 <- scale(num_df)
(a <- which(is.na(tmp1))) ## same amount of birds present, variance is 0 -> delete columns

if (length(a) > 0){
  tmp2 <- apply(tmp1, 2, function(x) all(is.na(x)))
  col_pos <- which(tmp2 == TRUE)

  to_del <- c(col_pos[[1]],col_pos[[2]]) ## TODO automatize to length of col_pos
  tmp3 <- tmp1[,-to_del]
  } else {tmp3 <- num_df} 


## run correlation
cor_mat <- cor(tmp3, method = "kendall", use = "pairwise.complete.obs")

cor_p <-   ggcorrplot(cor_mat,
           hc.order = TRUE,         # cluster similar variables
           type = "upper",
           lab = TRUE, 
           lab_size = 4,
           tl.cex = 12,
           legend.title = "Kendall's tau",
           outline.color = "white",
           insig = 'blank',
           digits = 1,
           colors = c("blue", "white", "red"))
cor_p 


### TODO - there are warnings about the aesthetics
pdf(file = paste0(paste0(dir_plot_path,"/Fig_6_corr_envs_kendall_",
                  data_year,suffix_for_plot,today,".pdf")),
                  width = 20, height = 25)
  cor_p
  
dev.off()


## ----sessionInfo-----------------------------------------------------------------------------------------------------------------------
Sys.time()
#git2r::repository() ## uncomment if you are using GitHub
sessionInfo()

