params <-
list(date = structure(20505, class = "Date"))

## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE, 
                      dev = "ragg_png", fig.width = 9, fig.height = 6, dpi = 600, retina = 1)
Sys.setlocale("LC_TIME", "C")


## ----settings--------------------------------------------------------------------------------------------------------------------------
# FLAG_run <- 0
if (FLAG_run == 0) {
 data_year <- 9999 ## set at the beginning - set to 9999 if data_years are combined
 source(here::here('R','source-file-with-helper-functions.R'))
}
suffix_for_plot <- "_community-metrics_"


## ----dataset-prep----------------------------------------------------------------------------------------------------------------------

str(mymosq)
#View(mymosq)


## copy dataset
tmp <- mymosq

## delete 19th of June in 2023 data set
myrow <- which(tmp$Date == '2023-06-19')
myrow
if (length(myrow) > 0) {tmp <- tmp[-myrow,] } 

## delete column of unidentifieds
a1 <- which(names(tmp) == "AB_unidentified")
a2 <- which(names(tmp) == "WNV_unidentified")
tmp1 <- tmp[, -c(a1,a2)]

## select only the columns with mosquitoes
i <- grep("AB_", names(tmp1) )
length(i)                ## this is mosquito diversity across all sites
ab_mosq <- tmp1[,c(1,i)]
#View(ab_mosq)

## select only the columns with infected
j <- grep("WNV_", names(tmp1) )
length(j)             ## this is infected mosquito diversity across all sites
ab_mosq_wnv <- tmp1[,c(1,j)]
#View(ab_mosq_wnv)


## ab_mosq and ab_mosq_wnv used for further analyses



## ----dataset-prep2---------------------------------------------------------------------------------------------------------------------

AB_mosq_site <- tapply(ab_mosq[,-1], ab_mosq$site, colSums, na.rm=T ) ## first column needs to be taken out
AB_mosq_list <- list('RB' = unname(AB_mosq_site$RB),
                     'RA' = unname(AB_mosq_site$RA),
                     'S' = unname(AB_mosq_site$S),
                     'C' = unname(AB_mosq_site$C),
                     'N' = unname(AB_mosq_site$N)
                     )
AB_mosq_list

## 2024 has only 1 infected mosquito species
data_dim <- dim(ab_mosq_wnv)[2]

if (data_dim > 2 ) {
    AB_wnv_site <- tapply(ab_mosq_wnv[,-1], ab_mosq_wnv$site, colSums, na.rm =T )
    AB_wnv_list <- list('RB' = unname(AB_wnv_site$RB),
                     'RA' = unname(AB_wnv_site$RA),
                     'S' = unname(AB_wnv_site$S),
                     'C' = unname(AB_wnv_site$C),
                     'N' = unname(AB_wnv_site$N)
                     )
    AB_wnv_list } else { 
      
    AB_wnv_site <- rowsum(x = ab_mosq_wnv[,-1], 
                             group = ab_mosq_wnv$site, na.rm =T )
    colnames(AB_wnv_site) <- names(ab_mosq_wnv[2]) # first position is site
       
    AB_wnv_list <- list(
         C  = AB_wnv_site[1,1],
         N  = AB_wnv_site[2,1],
         RA = AB_wnv_site[3,1],
         RB = AB_wnv_site[4,1],
         S  = AB_wnv_site[5,1]
    )
     
}
    


## ----mir-per-site----------------------------------------------------------------------------------------------------------------------
MIR_rb <- round(sum(AB_wnv_list$RB) / sum(AB_mosq_list$RB) * 1000 , digits=2); MIR_rb
MIR_ra <- round(sum(AB_wnv_list$RA) / sum(AB_mosq_list$RA) * 1000 , digits=2); MIR_ra
MIR_s <- round(sum(AB_wnv_list$S) / sum(AB_mosq_list$S) * 1000 , digits=2); MIR_s
MIR_c <- round(sum(AB_wnv_list$C) / sum(AB_mosq_list$C) * 1000 , digits=2); MIR_c
MIR_n <- round(sum(AB_wnv_list$N) / sum(AB_mosq_list$N) * 1000 , digits=2); MIR_n

## MIR changes in C if not only Culex is considered, 
## infection in A vexans, but only 1 of 20 - neglected because it does not
## reflect the processes in the field


## ----mir-per-spec-and-site-------------------------------------------------------------------------------------------------------------
if (data_year != 2024) {

  wnv_inf_spec <- gsub(pattern = "WNV_", replacement = "",x= names(ab_mosq_wnv))
  ab_mosq_spec <- gsub(pattern = "AB_" , replacement = "",x= names(ab_mosq))
  
  a <- which(ab_mosq_spec %in% wnv_inf_spec)
  ab_mosq_spec_with_inf <- ab_mosq[,a]
  
  sum_mosq_spec_with_inf <- tapply(ab_mosq_spec_with_inf[,-1],
                                  ab_mosq_spec_with_inf$site, 
                                  colSums, na.rm = T)
  
  mir_site_spec = mapply(FUN = `/`, 
                               AB_wnv_site, sum_mosq_spec_with_inf, 
                               SIMPLIFY = FALSE)
  
  mir_site_spec = mapply(FUN = function(val1) round(val1*1000, digits = 2), 
                         mir_site_spec, 
                         SIMPLIFY = FALSE)
  
  AB_wnv_beta_rel_as_df <- as.data.frame(mir_site_spec)

  ft <- flextable(AB_wnv_beta_rel_as_df)
  save_as_docx(ft, path =  paste0(dir_output_path,
                    "/ft_wnv_mir_per_species_and_site_",data_year,
                    suffix_for_plot,today,".docx"))
}



## ----raref-mosq-q0, warnings = FALSE---------------------------------------------------------------------------------------------------
mosq_div0 <- iNEXT(AB_mosq_list, q=0, datatype="abundance") 

mosq_div0$AsyEst ## save this output
write.csv(x = mosq_div0$AsyEst, 
   file = paste0(dir_output_path,"/Rarefaction_mosq_",data_year,
            suffix_for_plot,today,".csv"))


#### Hill q = 0 ; richness/ diversity
## TODO re-level legend to have RB RA S C N
g0 <- ggiNEXT(mosq_div0, type=1, se=TRUE, facet.var="None",
              color.var="Assemblage", 
              grey=FALSE) +
       scale_colour_manual(values = site.cols) +
       scale_fill_manual(values = site.cols) +
       ylab('Diversity of mosquito species') + 
       xlab('Number of mosquitoes')
g0

pdf(paste0(dir_plot_path,"/Fig_S1_rarefaction_mosq_q0_",data_year,suffix_for_plot,today,".pdf"), 
      width = 10, height = 10) 
  grid.arrange(g0, ncol=1)
dev.off()




## ----raref-mosq-q12, warnings = FALSE--------------------------------------------------------------------------------------------------
#### Hill q = 1 ; Shannon like
mosq_div1 <- iNEXT(AB_mosq_list, q=1, datatype="abundance")
g1 <- ggiNEXT(mosq_div1, type=1, se=TRUE, facet.var="None",  color.var="Assemblage", 
        grey=FALSE) +
  theme(legend.position="right") +
  ylab('Hill number  q = 1') +
  guides(linetype=guide_legend(title="Method"), 
        colour=guide_legend(title="Site"), 
        fill=guide_legend(title="Site"), 
        shape=guide_legend(title="Site")) +
       scale_colour_manual(values = site.cols) +
       scale_fill_manual(values = site.cols) +
       xlab('Number of mosquitoes')

g1

pdf(paste0(dir_plot_path,"/Fig_rarefaction_mosq_q1_",data_year,suffix_for_plot,today,".pdf"), 
      width = 10, height = 10) 
  grid.arrange(g1, ncol=1)
dev.off()


#### Hill q = 2 ; Simpson like
mosq_div2 <- iNEXT(AB_mosq_list, q=2, datatype="abundance")
g2 <- ggiNEXT(mosq_div2, type=1, se=TRUE, facet.var="None", color.var="Assemblage", 
        grey=FALSE) +
  theme_bw(base_size = 18) +
  theme(legend.position="right") +
  ylab('Hill number  q = 2') +
  guides(linetype=guide_legend(title="Method"), 
        colour=guide_legend(title="Site"), 
        fill=guide_legend(title="Site"), 
        shape=guide_legend(title="Site")) +
         scale_colour_manual(values = site.cols) +
       scale_fill_manual(values = site.cols) +
       xlab('Number of mosquitoes')
g2

pdf(paste0(dir_plot_path,"/Fig_rarefaction_mosq_q2_",data_year,suffix_for_plot,today,".pdf"), 
      width = 10, height = 10) 
  grid.arrange(g2, ncol=1)
dev.off()


## ----raref-wnv-q0, warnings = FALSE----------------------------------------------------------------------------------------------------

# if (lapply(AB_wnv_list, length)[1] > 1) { ## more elegant to avoid the year setting
if (data_year != 2024) {
wnv_div0 <- iNEXT(AB_wnv_list, q=0, datatype="abundance") 
wnv_r <- ggiNEXT(wnv_div0, type=1, se=TRUE, facet.var="None",
              color.var="Assemblage", grey=FALSE) + 
  scale_colour_manual(values = site.cols) +
  scale_fill_manual(values = site.cols) +
  ylab('Diversity of WNV-infected mosquito species') + 
  xlab('Number of samples')

wnv_r

pdf(paste0(dir_plot_path,"/Fig_rarefaction_wnv_q0_",data_year,suffix_for_plot,today,".pdf"), 
      width = 10, height = 10) 
  grid.arrange(wnv_r, ncol=1)
dev.off()
}


## ----bc-mosq, warnings = FALSE---------------------------------------------------------------------------------------------------------
AB_mosq_beta <- t(as.data.frame(AB_mosq_list))
dimnames(AB_mosq_beta) <- list(rownames(AB_mosq_beta),
                               gsub(pattern="AB_",replacement = "",
                               x = names(ab_mosq)[-1])) 

#class(AB_mosq_beta)

## in case for PCA
write.table(x = as.data.frame(AB_mosq_beta), 
            file = paste0(here("data"),"/ab_mosq_spec_per_site_",data_year,".csv"),
            sep= ";",
            col.names = TRUE,
            row.names = TRUE,
            dec ="." )




BC_vegan_diss <- round(vegdist(AB_mosq_beta, method = 'bray'),digits=2)
(BC_vegan_sim <- 1-BC_vegan_diss)

## convert to data.frame to save it as table
tmp1 <- as.matrix(BC_vegan_sim)
tmp2 <- as.data.frame(tmp1,row.names=rownames(tmp1))
tmp2$site <-rownames(tmp1) 
#tmp2

ft <- flextable(tmp2)
save_as_docx(ft, path =  paste0(dir_output_path,
             "/ft_AB_mosq_BrayCurtis_similarity_",data_year,suffix_for_plot,today,".docx"))



## ----bc-mnv-ab, warnings = FALSE-------------------------------------------------------------------------------------------------------
AB_wnv_beta <- t(as.data.frame(AB_wnv_list))
  
## give back the infected mosquito names without WNV in front
dimnames(AB_wnv_beta) <- list(rownames(AB_wnv_beta),
                            gsub(pattern="WNV_",replacement = "",
                            x = names(ab_mosq_wnv)[-1])) 

## in case for PCA
write.table(x = as.data.frame(AB_wnv_beta), 
            file = paste0(here("data"),"/ab_mosq_wnv_spec_per_site_",data_year,".csv"),
            sep= ";",
            col.names = TRUE,
            row.names = TRUE,
            dec ="." )


if(data_year != 2024){
  BC_wnv_diss <- round(vegdist(AB_wnv_beta, method = 'bray'),digits=2)
  (BC_wnv_sim <- 1-BC_wnv_diss) ## similarity

  tmp1 <- as.matrix(BC_wnv_sim)
  tmp2 <- as.data.frame(tmp1,row.names=rownames(tmp1))
  tmp2$site <-rownames(tmp1) 
  #tmp2
    ft <- flextable(tmp2)
    save_as_docx(ft, path =  paste0(dir_output_path,
                    "/ft_wnv_abs_BrayCurtis_similarity_",data_year,suffix_for_plot,today,".docx"))
}



## ----bc-wnv-mir, warnings = FALSE------------------------------------------------------------------------------------------------------
if (data_year != 2024) {
  AB_wnv_beta_rel <- t(as.data.frame(mir_site_spec)) ## see MIR per site and species


  BC_wnv_diss_rel <- round(vegdist(AB_wnv_beta_rel, method = 'bray'),digits=2)
  BC_wnv_sim_rel <- 1-BC_wnv_diss_rel
  BC_wnv_sim_rel #similarity

  tmp1 <- as.matrix(BC_wnv_sim_rel)
  tmp2 <- as.data.frame(tmp1,row.names=rownames(tmp1))
  tmp2$site <-rownames(tmp1) 
  #tmp2


  ## this is similarity in infected species groups, not in total inf
  ft <- flextable(tmp2)
  save_as_docx(ft, path =  paste0(dir_output_path,
                    "/ft_wnv_mir_BrayCurtis_similarity_",data_year,suffix_for_plot,today,".docx"))
}



## ----hc-mosq, warnings = FALSE---------------------------------------------------------------------------------------------------------
my_a <- data.matrix(AB_mosq_beta)
my_a


## take the log of the abundance and add 1 to avoid zero issue
## zeros will stay 0, but we have to add to the 1 for the color scale
## to avoid that o and 1 are coerced
my_a_log <- log2(my_a+1)

#create the breaks to draw 0 found in grey
bk2 = unique(seq(0, round(max(my_a_log)), length=round(max(my_a_log))+1))
  
a <- which(my_a_log == 1)
my_a_log[a] <- 1.1 ## trick to distinguish 0 from 1 in breaks

#set different color vectors for each interval
col1 = rep("lightgrey", 1) # set 0 to grey
col2 <- inferno(round(max(my_a_log))+1)
colors2 <- c(col1, col2)

p_heat <- pheatmap(my_a_log, color=colors2, breaks=bk2, fontsize = 16) # logarithm!

pdf(paste0(dir_plot_path,"/Fig_5A_heat_mosqAB_log_",data_year,suffix_for_plot,today,".pdf"), 
      width = 12, height = 10) 
  p_heat
dev.off()


## ----hc-wnv-ab, warnings = FALSE-------------------------------------------------------------------------------------------------------
print("Fig 5B") ## TODO - bug here that fig 5B is never saved
dev.off() ## TODO is this the error?


if (data_year != 2024) {
  my_b <- data.matrix(AB_wnv_beta)
  my_b
  
  #create the breaks to draw 0 found in grey
  bk2 = unique(seq(0, max(my_b), length=max(my_b)+1))
  
  a <- which(my_b == 1)
  my_b[a] <- 1.1 ## trick to distinguish 0 from 1 in breaks

  #set different color vectors for each interval
  col1 = rep("lightgrey", 1) # set 0 to grey
  col2 <- inferno(max(my_b)+1)
  colors2 <- c(col1, col2)
  
  #draw heatmap
  p_heat_wnv <- pheatmap(my_b, color=colors2, breaks=bk2, fontsize= 16) ##
  p_heat_wnv

  pdf(paste0(dir_plot_path,"/Fig_5B_heat_wnvAB_",data_year,suffix_for_plot,today,".pdf"), 
      width = 10, height = 10) 
    p_heat_wnv
  dev.off()

}

dev.off() ## TODO


## ----hc-wnv-mir, warnings = FALSE------------------------------------------------------------------------------------------------------

if (data_year != 2024) {
## colour breaks / quantiles
my_c <- data.matrix(AB_wnv_beta_rel)
my_c 


quantile_breaks <- function(xs, n = 10) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n))
  breaks[!duplicated(breaks)]
}
mat_breaks <- quantile_breaks(my_c, n = 49)


p_heat_wnv_rel <- pheatmap(mat = my_c, 
                           color = inferno(length(mat_breaks) - 1),
                           breaks= mat_breaks,
                           fontsize=16) 
p_heat_wnv_rel

pdf(paste0(dir_plot_path,"/Fig_heat_wnv_mir_",data_year,suffix_for_plot,today,".pdf"), 
      width = 12, height = 10) 
  p_heat_wnv_rel
dev.off()

}
dev.off()



## ----sessionInfo-----------------------------------------------------------------------------------------------------------------------
Sys.time()
#git2r::repository() ## uncomment if you are using GitHub
sessionInfo()

