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

suffix_for_plot <- "_PCA_"


## -----------------------------------------------------------------------------------------------------------------------------
str(myenvblood)
myenvblood$site

## resort based on alphabetical order to keep it the same order !!!! super important
df_sorted <- myenvblood[order(myenvblood$site), ]
rownames(df_sorted) <- c(1:5)
rownames(df_sorted)


num_df <- df_sorted[, -1] ## delete the non-numeric site variable
data_normalized <- scale(num_df)


## remove birds with zero variance
data_normalized <- data_normalized[, !colSums(is.na(data_normalized))]


## -----------------------------------------------------------------------------------------------------------------------------
## based on scaled data
data.pca <- prcomp(data_normalized, center = TRUE, scale. = TRUE)

summary(data.pca)

rownames(data.pca$rotation) <- gsub("_", " ", rownames(data.pca$rotation))

# Compute eigenvalues
eigenvalues <- (data.pca$sdev)^2
eigenvalues

## -----scree plot-----
# Data for scree plot
scree_data <- data.frame(
  Component = 1:length(eigenvalues),
  Eigenvalue = eigenvalues
)

# ggplot scree plot
ggplot(scree_data, aes(x = Component, y = Eigenvalue)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Scree Plot", x = "Principal Component",
    y = "Eigenvalue"
  )

# Retain components with eigenvalues > 1
components_to_retain <- sum(eigenvalues > 1)
components_to_retain # 4

variance_explained <- round(eigenvalues / sum(eigenvalues) * 100, digits = 2)
cumulative_variance <- cumsum(variance_explained)

# Combine into a data frame for interpretation
results <- data.frame(
  PC = 1:length(eigenvalues),
  Eigenvalue = eigenvalues,
  Variance_Explained = variance_explained,
  Cumulative_Variance = cumulative_variance
)
print(results)


## -----------------------------------------------------------------------------------------------------------------------------
## Biplot using factoextra
## ind is related to the rows,
## and the numbering is according to rownames
rownames(com) ##  "1" "2" "3" "4" "5"
com$site ## "C"  "N"  "RA" "RB" "S"

p <- fviz_pca_biplot(data.pca,
  repel = TRUE,
  col.var = "cos2",
  gradient.cols = scico::scico(100, palette = "acton", direction = -1, end = .9),
  pointshape = 19, pointsize = 7, alpha.ind = 0.8,
  legend.title = "cos2"
) +
  theme(panel.grid.minor = element_blank())

## Cedric's hack:
## use site labels not numbers
p$layers[[2]]$data$name <- factor(com$site)
## force custom site colors for points and labels
p$layers[[1]]$mapping <- aes(x, y, colour = site.cols)
p$layers[[2]]$mapping <- aes(x, y, label = name, colour = site.cols)
p$layers[[1]]$aes_params$colour <- site.cols
p$layers[[2]]$aes_params$colour <- colorspace::darken(site.cols, .45)
## bold and larger text
p$layers[[2]]$aes_params$fontface <- "bold"
p$layers[[2]]$aes_params$size <- 5

p

ggsave(
  paste0(
    dir_plot_path, "/Fig_7_pca_blood_envcom_wSites_",
    data_year, suffix_for_plot, today, ".pdf"
  ),
  width = 12, height = 8, plot = p
)


## save
# pdf(
#  file = paste0(dir_plot_path, "/Fig_7_pca_envcom_wSites_", data_year, suffix_for_plot, today, ".pdf"),
#  width = 12, height = 8
# )
# p
# dev.off()


## -----------------------------------------------------------------------------------------------------------------------------
cos_p <- fviz_cos2(data.pca, choice = "var", axes = 1:2)
cos_p

ggsave(
  paste0(
    dir_plot_path, "/Fig_S2_pca_blood_envcom_cos2_",
    data_year, suffix_for_plot, today, ".pdf"
  ),
  width = 12, height = 8, plot = cos_p
)

## save plots
# pdf(
#  file = paste0(dir_plot_path, "/Fig_S2_pca_blood_envcom_cos2_", data_year, suffix_for_plot, today, ".pdf"),
#  width = 12, height = 8
# )
# fviz_cos2(data.pca, choice = "var", axes = 1:2)
# dev.off()


## ----sessionInfo--------------------------------------------------------------------------------------------------------------
Sys.time()
# git2r::repository() ## uncomment if you are using GitHub
sessionInfo()

