params <-
list(date = structure(20508, class = "Date"))

## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE,
  dev = "ragg_png", fig.width = 9, fig.height = 6, dpi = 600, retina = 1
)
Sys.setlocale("LC_TIME", "C")


## -----------------------------------------------------------------------------------------------------------------------------
if (FLAG_run == 0) {
  data_year <- 2024 ## set at the beginning - set to 9999 if data_years are combined
  source(here::here("R", "source-file-with-helper-functions.R"))
}
suffix_for_plot <- "_bloodmeal_"


## -----------------------------------------------------------------------------------------------------------------------------
str(myenvblood)
num_df <- myenvblood[, -1] ## delete the non-numeric site variable
num_df <- janitor::clean_names(num_df)
num_df <- rename_with(num_df, ~ gsub("_", " ", .x))
# num_df <- rename_with(num_df, ~ gsub(".", " ", .x))

## only concerns 2024 data
tmp1 <- scale(num_df)
tmp2 <- tmp1[, !colSums(is.na(tmp1))] ## remove if there are variance issues

## run correlation
cor_mat <- cor(tmp2, method = "kendall", use = "pairwise.complete.obs")

cor_p <- ggcorrplot(cor_mat,
  hc.order = TRUE, # cluster similar variables
  type = "upper",
  lab = TRUE,
  lab_size = 2,
  tl.cex = 10,
  legend.title = "Kendall's tau",
  outline.color = "white",
  insig = "blank",
  digits = 1,
  colors = c("blue", "white", "red")
)

cor_p_custom <-
  cor_p +
  scale_x_discrete(position = "top") +
  theme(
    axis.text.x.top = element_text(vjust = 1, hjust = 1, angle = -45),
    panel.grid.major = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.8, .2)
  )
# theme(panel.grid.major.y = element_blank()) ## to havae lines to x-axis
cor_p_custom

ggsave(
  paste0(
    dir_plot_path, "/Fig_6_corr_envs_kendall_",
    data_year, suffix_for_plot, today, ".pdf"
  ),
  width = 14, height = 12, plot = cor_p_custom
)


# pdf(
#  file = paste0(paste0(
#    dir_plot_path, "/Fig_6_corr_envs_kendall_",
#    data_year, suffix_for_plot, today, ".pdf"
#  )),
#  width = 15, height = 15
# )
# cor_p_custom
# dev.off()


## ----sessionInfo--------------------------------------------------------------------------------------------------------------
Sys.time()
# git2r::repository() ## uncomment if you are using GitHub
sessionInfo()

