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
str(myenv)
str(ab_aves)


## -----------------------------------------------------------------------------------------------------------------------------
#### env vars and aves spec across locations

names(ab_aves) ## 66 bird species - focus on the abundant ones - if at least 1 bird present
ab_aves$site ## take care - different sequence now, not sorted alphabetically
# ab_aves_floor <- floor(ab_aves[c(2:dim(ab_aves)[2])]) ## reduces 33
ab_aves_floor <- round(ab_aves[c(2:dim(ab_aves)[2])], digits = 0) ## reduces 24


aves_to_remove <- which(colSums(ab_aves_floor) == 0)
length(aves_to_remove) # remove 33 of 66
names(aves_to_remove)

master_aves_red <- ab_aves_floor[, -which(names(ab_aves_floor) %in% names(aves_to_remove))]
master_aves <- cbind(ab_aves$site, master_aves_red)
head(master_aves)
names(master_aves)[1] <- "site"

ft <- flextable(master_aves)
save_as_docx(ft, path = paste0(
  dir_output_path,
  "/ft_aves_spec_per_location_modelled_reduced", suffix_for_plot, today, ".docx"
))


## -----------------------------------------------------------------------------------------------------------------------------
com <- merge(myenv, master_aves, by = "site") ## merge! by site


## -----------------------------------------------------------------------------------------------------------------------------
com
## in case there are factors/characters in the data
# which(names(com) == 'site')

subset_for_analysis <- com[, -1]
data_normalized <- scale(subset_for_analysis)

# head(data_normalized)

## remove birds with zero variance
data_normalized <- data_normalized[, !colSums(is.na(data_normalized))]

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
    dir_plot_path, "/Fig_7_pca_envcom_wSites_",
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
fviz_cos2(data.pca, choice = "var", axes = 1:2)

## save plots
pdf(
  file = paste0(dir_plot_path, "/Fig_S2_pca_envcom_cos2_", data_year, suffix_for_plot, today, ".pdf"),
  width = 12, height = 8
)
fviz_cos2(data.pca, choice = "var", axes = 1:2)
dev.off()


## ----sessionInfo--------------------------------------------------------------------------------------------------------------
Sys.time()
# git2r::repository() ## uncomment if you are using GitHub
sessionInfo()

