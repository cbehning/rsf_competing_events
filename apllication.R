##############################################################################
#####            Random Survival Forests with Competing Events:          #####
#####            A Subdistribution-Based Imputation Approach             #####
#####                        Electronic Supplement                       #####
##############################################################################
#####             Author: Charlotte Behning                              #####
##############################################################################
#####             Content: R-Code of the Simulations                     #####
#####                      presented in Section 3                        ##### 
##############################################################################


# The following contains R-Code to create results for a sythetic simulation 
# data set.
# 
# Please note: due to data protection contraints, the original appication data
# set could not be provided in this repository. Therefore, we created a 
# synthetic dataset using the synthpop package and function syn.
# After creating the synthetic dataset, we used a subset of n=2000 row.
# Please note, that the synthtic dataset does not lead to the same results as 
# withthe orginal data provided in the paper.

#### Not run ####
# create synthetic data. ! Original data is not provided, running not possible
# 
library(tidyverse)
# library(synthpop)
# #load required functions
source("functions_data_generating.R")
#
## inread original data as original
# original <- read.csv(file = <path to original data>, header = TRUE, sep = ",")
# 
# # define rules to ensure consistency
# rules.list <- list(
#   p_renal = "fuehrende_nierenerkr == 1",
#   acute_fail = "fuehrende_nierenerkr == 2",
#   other = "fuehrende_nierenerkr == 3",
#   diab_neph = "fuehrende_nierenerkr == 4", 
#   single_kidney = "fuehrende_nierenerkr == 5",
#   hereditary = "fuehrende_nierenerkr == 6", 
#   int_neph = "fuehrende_nierenerkr == 7", 
#   pr_glom_path = "fuehrende_nierenerkr == 9",
#   hyp_neph = "fuehrende_nierenerkr == 11",
#   unknown = "fuehrende_nierenerkr ==1",
#   unknown = "fuehrende_nierenerkr ==2",
#   unknown = "fuehrende_nierenerkr ==3",
#   unknown = "fuehrende_nierenerkr ==4",
#   unknown = "fuehrende_nierenerkr ==5",
#   unknown = "fuehrende_nierenerkr ==6",
#   unknown = "fuehrende_nierenerkr ==7",
#   unknown = "fuehrende_nierenerkr ==9",
#   unknown = "fuehrende_nierenerkr ==10",
#   unknown = "fuehrende_nierenerkr ==11")# unknown = "fuehrende_nierenerkr ==1 ,2,3,4,5,6,7,9,10,11)"),
# rules.value.list <- list(
#   p_renal = 1, 
#   acute_fail = 1, 
#   other = 1, 
#   diab_neph = 1, 
#   single_kidney = 1,
#   hereditary = 1,
#   int_neph = 1,
#   pr_glom_path = 1,
#   hyp_neph = 1,
#   unknown = 0,
#   unknown = 0,
#   unknown = 0,
#   unknown = 0,
#   unknown = 0,
#   unknown = 0,
#   unknown = 0,
#   unknown = 0,
#   unknown = 0,
#   unknown = 0
# )
# 
# 
# synth.obj <- synthpop::syn(original, #cont.na = cont.na.list, 
#                            rules = rules.list, rvalues = rules.value.list, seed = 28062024)
# 
# synth.obj$syn$e1 <-  (synth.obj$syn$status == 1) *1.0
# synth.obj$syn$e2 <- (synth.obj$syn$status > 1) *1.0
# 
# original$e1 <-  (original$status == 1) *1.0
# original$e2 <- (original$status > 1) *1.0
# 
#
# ## Sample IDs to create a subset of synthetic data using n=2000 rows/subjects.
# set.seed(01072024)
# sample_ids <- sample(size = 2000, x = nrow(synth.obj$syn), replace = FALSE)
# 
# 
# ### create impute once data
# syn_gckd_oio <- DRSA_createSampledRawOutput21(dataS = synth.obj$syn[sample_ids, ],
#                                                eventCols = c("e1", "e2"), eoi = "e1", timeCol = "time", seed2 = 081123)
# # new status
# syn_gckd_oio$status <- as.numeric(syn_gckd_oio[, "e1"] == 1)
# # drop columns
# syn_gckd_oio[, c("subDistWeights", "v_samplegew", "y", "eventCols", "time")] <- NULL
# # rename
# syn_gckd_oio <- syn_gckd_oio %>%  rename(time = timeInt) %>% arrange(obj) %>% data.frame()
# # remove e1, e2 abd obj
# syn_gckd_oio[,c("obj", "e1", "e2", "subDistWeights", "v_samplegew", "y")] <- NULL
#
#

load("syn_gckd.rda")
# create paths
gckd_path <- "synthetic_gckd" 
dir.create(gckd_path, showWarnings = FALSE)
gckd_data_path <- file.path(paste("synthetic_gckd","data", sep =.Platform$file.sep))
dir.create(gckd_data_path, showWarnings = FALSE)
results_path <- file.path(paste("synthetic_gckd","results", sep =.Platform$file.sep))
dir.create(results_path, showWarnings = FALSE)

#write imputed data
write.table(syn_gckd_oio, file = "synthetic_gckd/data/syn_gckd_data_cc_fn_d7_oio.csv",
            sep = ",", quote = FALSE, col.names = TRUE,row.names = FALSE)
# write data subset
write.table(syn_gckd, #synth.obj$syn[sample_ids, ] %>% select(-e1, -e2)
            file = "synthetic_gckd/data/syn_gckd_data_cc_fn_d7.csv",
            sep = ",", quote = FALSE, col.names = TRUE,row.names = FALSE)

# recode event columns
syn_gckd$e1 <-  (syn_gckd$status == 1) *1.0
syn_gckd$e2 <- (syn_gckd$status > 1) *1.0

# Create imputeOnce data for 10 different seeds
for(seed2 in 101:110){
  df_sample_syn <- DRSA_createSampledRawOutput21(dataS = syn_gckd,
                                                 eventCols = c("e1", "e2"), eoi = "e1", timeCol = "time", seed2 = seed2)
  # new status
  df_sample_syn$status <- as.numeric(df_sample_syn[, "e1"] == 1)
  # drop columns
  df_sample_syn[, c("subDistWeights", "v_samplegew", "y", "eventCols", "time")] <- NULL
  # rename
  df_sample_syn <- df_sample_syn %>%  rename(time = timeInt) %>% arrange(obj) %>% data.frame()
  # remove e1, e2 abd obj
  df_sample_syn[,c("obj", "e1", "e2", "subDistWeights", "v_samplegew", "y")] <- NULL
  
  write.table(df_sample_syn,
              file = paste(paste(gckd_data_path, "syn_gckd_data_cc_fn_d7_oio_", sep = .Platform$file.sep), seed2,".csv", sep =""),
              sep = ",", quote = FALSE, col.names = TRUE,row.names = FALSE)
}


# Run ranger simulations 
# Use code as illustrated in train_predict.R
# ranger stand lone needs to be compiled and avlaible at <path to ranger>
# see bash scripts as example to run the 4 different method apporaches:
# imputeNode, imputeOnce, naive, imputeRoot

## ImputeNode
#
# #!/bin/bash	
# <path to ranger> \
# 		--file data/syn_gckd_data_cc_fn_d7.csv \
# 		--treetype 5  \
# 		--depvarname time  \
# 		--statusvarname status  \
# 		--write  \
# 		--seed 20231107  \
# 		--catvars dem_famstand,dem_beruf,employment,fuehrende_nierenerkr \
# 		--impmeasure 6 \
# 		--crimputesubdist  \
# 		--outprefix results/imputeNode_syn_gckd_data_cc_fn_d7 
#
# <path to ranger> \
#     --predict results/imputeNode_syn_gckd_data_cc_fn_d7.forest \
#     --file data/syn_gckd_data_cc_fn_d7.csv \
#     --outprefix results/p_imputeNode_syn_gckd_data_cc_fn_d7

## Naive Approach
# 
# #!/bin/bash
# <path to ranger> \
# 		--file data/syn_gckd_data_cc_fn_d7.csv \
# 		--treetype 5  \
# 		--depvarname time  \
# 		--statusvarname status  \
# 		--write  \
# 		--seed 20231107  \
# 		--catvars dem_famstand,dem_beruf,employment,fuehrende_nierenerkr \
# 		--impmeasure 6 \
# 		--outprefix results/Naive_syn_gckd_data_cc_fn_d7 
#
# <path to ranger> \
#     --predict results/Naive_syn_gckd_data_cc_fn_d7.forest \
#     --file data/syn_gckd_data_cc_fn_d7.csv \
#     --outprefix results/p_Naive_syn_gckd_data_cc_fn_d7

## ImputeOnce
#
# #!/bin/bash
# <path to ranger> \
# 		--file data/syn_gckd_data_cc_fn_d7_oio.csv\
# 		--treetype 5  \
# 		--depvarname time  \
# 		--statusvarname status  \
# 		--write  \
# 		--seed 20231107  \
# 		--catvars dem_famstand,dem_beruf,employment,fuehrende_nierenerkr \
# 		--impmeasure 6 \
# 		--outprefix results/imputeOnce_syn_gckd_data_cc_fn_d7
#
# <path to ranger> \
#     --predict results/imputeOnce_syn_gckd_data_cc_fn_d7.forest \
#     --file data/syn_gckd_data_cc_fn_d7_oio.csv\
#     --outprefix results/p_imputeOnce_syn_gckd_data_cc_fn_d7


## imputeRoot 
#
# #!/bin/bash
# <path to ranger> \
# 		--file data/syn_gckd_data_cc_fn_d7.csv \
# 		--treetype 5  \
# 		--depvarname time  \
# 		--statusvarname status  \
# 		--crimputesubdistonlyinroot \
# 		--crimputesubdist  \
# 		--write  \
# 		--seed 20231107  \
# 		--catvars dem_famstand,dem_beruf,employment,fuehrende_nierenerkr \
# 		--impmeasure 6 \
# 		--outprefix results/imputeRoot_syn_gckd_data_cc_fn_d7
#
# <path to ranger> \
#      --predict results/imputeRoot_syn_gckd_data_cc_fn_d7.forest \
#      --file data/syn_gckd_data_cc_fn_d7.csv \
#      --outprefix results/p_imputeRoot_syn_gckd_data_cc_fn_d7


## For 10 impuation runs 
## Use the following bash script with slurm to submit 10, one for each imputed dataset

# #!/bin/bash
#
# mkdir results_10
# declare -a dataarr=("syn_gckd_data_cc_fn_d7" ) 
# declare -a seed2arr=($(seq 101 1 110))
# 
# 
# for data in "${dataarr[@]}"
# do
# for seed2 in "${seed2arr[@]}"
# do
# 
# # imputeOnce
# 
# echo -e "#!/bin/bash\n	<path to ranger> \
# 		--file data/"${data}"_oio_"${seed2}".csv\
# 		--treetype 5  \
# 		--depvarname time  \
# 		--statusvarname status  \
# 		--write  \
# 		--seed 20231107  \
# 		--catvars dem_famstand,dem_beruf,employment,fuehrende_nierenerkr \
# 		--impmeasure 6 \
# 		--outprefix results_10/imputeOnce_"${data}"_"${seed2}"  
# 		
# 		
# 		<path to ranger> \
# 		--predict results/imputeOnce_"${data}"_"${seed2}".forest \
# 		--file data/"${data}"_oio_"${seed2}".csv\
# 		--outprefix results_10/p_imputeOnce_"${data}"_"${seed2}"
# 		
# 		rm results/imputeOnce_"${data}"_"${seed2}".forest
# 		
# 		"|sbatch  --job-name=r_"$seed2"_imputeOnce_"$data"
# 
# done
# done


##############################################################################
###    To proceed with this code, the above commands need to be run to     ###
###    generate results in the results folder.                             ###
###    Generated results here differ from the original application.        ### 
###    Aggregated summarized results and plot + table creation can be      ###
###    found  at evaluation.r                                              ###
##############################################################################

# set to true, if the ranger code was run as indicated above.
# If no .importance and .prediction files are available at
# synthetic_gckd/results, the code cannot be executed
if(FALSE){
  ###########################################
  ## load prediction results for CIF plotting
  gckd_pred_files <- list.files(results_path, pattern="*.prediction", full.names=TRUE, recursive = TRUE)
  
  setting <- "gckd_data_cc_fn_d7"
  approaches <- c("Naive", "imputeNode", "imputeRoot", "imputeOnce")
  
  # for both events
  gckd_chf <-  lapply(gckd_pred_files, read.table, sep ="", skip = 4)
  
  # creat names names
  names(gckd_chf) <- gsub(gckd_pred_files,
                          pattern = results_path, replacement = "") %>% 
    gsub(pattern = ".prediction", replacement = "") %>% 
    gsub(pattern = paste("^.*",.Platform$file.sep,"p_", sep =""),replacement = "")
  
  time_names <- lapply(gckd_pred_files, read.table, sep ="", skip = 1, nrow=1 )
  
  # 1) compute hazard
  #simulation_h <- lapply(simulation_chf, function(x) (cbind(x, 0) - cbind(0, x))) #[, time_vector+1]
  # 2) compute CIF as CIF = 1 - prod( 1- haz)
  #simulation_cif <- lapply(simulation_h, function(x) 1- matrixStats::rowCumprods(as.matrix(1-x), cols = time_vector))
  # 1) compute CIF via 1-s = 1-exp(-Lambda), Lambda = chf
  
  for(i in 1:length(gckd_chf)){
    names(gckd_chf[[i]]) <- time_names[[i]]
  }
  
  gckd_cif <- lapply(gckd_chf, function(x) 1-exp(-x))
  
  # 3) compute mean CIF
  gckd_mean_cif <- lapply(gckd_cif, colMeans)
  
  # convert into df
  gckd_mean_cif_df <-  bind_rows(gckd_mean_cif, .id = 'simulation')
  
  # pivot longer for plotting (this results in a data frame as saved as gck_cif for the original data, see evaluation.R)
  gckd_mean_cif_df_long <- cbind(gckd_mean_cif_df, "X0" = 0) %>%
    pivot_longer(-simulation, names_to = "time", values_to = "mean_cif")%>%
    mutate(time = gsub(time, pattern = "X", replacement = ""),
           discrete_tp = gsub(simulation, pattern = "^.*_d", replacement = ""),
           type = gsub(simulation, pattern = "_.*$", replacement = ""))
  
  
  # plot colors
  type_colors <- c( "#c4515c", "grey", "gold" , "steelblue",  "black" ) 
  names(type_colors) <- c("imputeNode", "Naive approach", "imputeOnce",  "imputeRoot",  "Reference"  )
  
  
  # Create plot
  p <- gckd_mean_cif_df_long %>%
    filter(discrete_tp == 7) %>%
    mutate(type = ifelse(type %in% "Naive", "Naive approach", type )) %>%
    mutate(time = as.numeric(time) ) %>% 
    ggplot(aes(x = time, y =  mean_cif, color = type)) +
    geom_step( linewidth = 1.00,  alpha = 0.9) + #aes(linetype = type), linewidth = 1.00,  alpha = 0.9)+
    ylab("CIF")+
    xlim(c(0,7))+
    scale_color_manual(values = type_colors, name = NULL)+
    theme_bw()+
    theme(legend.position = "bottom",
          text = element_text(size = 11))
  p1 <- p + theme(legend.position = c(0.2, 0.85))
  
  # ggsave(p, filename = paste(gckd_path, "syn_gckd_CIF_d7.png", sep = .Platform$file.sep), device = "png", width = 6, height = 5)
  ###########################################
  
  ## Load importance files
  gckdimportance_files <- list.files(results_path, pattern="*.importance", full.names=TRUE, recursive = TRUE)
  # inread
  gckd_importance <-  lapply(gckdimportance_files, read.table, header = TRUE)
  # names
  names(gckd_importance) <- gsub(gckdimportance_files,
                                 pattern =results_path, replacement = "") %>% 
    gsub(pattern = ".importance", replacement = "") %>%
    gsub(pattern = paste("^.*",  .Platform$file.sep, sep = ""), replacement = "")
  
  #compute mean imp per simulation run
  gckd_imp_mean <- lapply(gckd_importance, colMeans)
  
  # convert into df
  gckd_imp_mean_df <-  bind_rows(gckd_importance, .id = 'simulation')
  
  # get information from names and pivot longer
  # impute in all nodes
  gckd_imp_mean_df_long <- gckd_imp_mean_df %>%
    pivot_longer(-simulation,  names_to = "column", values_to = "VIMP")%>%
    mutate( discrete_tp = gsub(simulation, pattern = "^.*_d", replacement = ""),
            type = gsub(simulation, pattern = "_.*$", replacement = ""),
            type = ifelse(type %in% "Naive", "Naive approach", type ) )
  
  
  p <- gckd_imp_mean_df_long %>%
    group_by(column, type) %>% 
    summarise(VIMP = mean(VIMP,na.rm = TRUE)) %>%
    group_by(type) %>% 
    arrange(desc(VIMP)) %>% 
    slice(1:10) %>%
    ungroup() %>% 
    mutate( column_o = factor(paste0(column, "__", type)) %>%  fct_reorder(VIMP) ) %>%
    data.frame() %>% 
    ggplot(aes(x = column_o, y = VIMP, fill = type))+
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8)+
    scale_fill_manual(values = type_colors, name = NULL)+
    scale_x_discrete(name=NULL, labels=function(x) gsub('__(.*)$', '', x)) +
    ylab("Mean permutation VIMP")+
    xlab("Variables selected")+
    facet_wrap(~type, nrow = 2, scales = "free")+
    coord_flip()+
    theme_bw()+
    theme(legend.position = "none", text = element_text(size = 11))

  tmp7 <- 
    gckd_imp_mean_df_long %>%
    group_by(column, type) %>% 
    summarise(VIMP = mean(VIMP,na.rm = TRUE)) %>%
    group_by(type) %>% 
    arrange(desc(VIMP)) %>% 
    slice(1:10) %>%
    ungroup() %>% 
    mutate( column_o = factor(paste0(column, "__", type)) %>%  fct_reorder(VIMP) ) %>%
    data.frame() 
  


p2 <- p+ scale_y_continuous(n.breaks = 3, expand = c(0, 0))+
  shadowtext::geom_shadowtext(
    data = subset(tmp7, VIMP < 0.0025) %>% 
      mutate(labels = gsub('__(.*)$', '', column_o)),
    aes(y = VIMP, x = column_o, label = labels),
    hjust = 0,
    #nudge_y = 0.002,
    colour = "black",
    bg.colour = "white",
    bg.r = 0.2,
    size = 3
  ) + 
  geom_text(
    data = subset(tmp7, VIMP >= 0.0025)%>% 
      mutate(labels = gsub('__(.*)$', '', column_o)),
    aes(y =0, x = column_o, label = labels),
    hjust = 0,
    #nudge_y = 0.003,
    colour = "black",
    size = 3
  )+
  theme(
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    plot.margin = margin(t = 5,  # Top margin
                         r = 10,  # Right margin
                         b = 10,  # Bottom margin
                         l = 30) # Left margin
  ) 

ggsave(cowplot::plot_grid(p1, p2, labels = c('A', 'B'))
       , filename = paste(gckd_path, "syn_gckd_CIF_VIMP_4_d7.png", sep =.Platform$file.sep), device = "png", width = 10, height = 7)

}

# Results of 10 fold imputation
# set to true, if the ranger code was run as indicated above.
# If no .importance and .prediction files are available at
# synthetic_gckd/results_10, the code cannot be executed
if(FALSE){
  # inread prediction files as created in the slurm scripts above. 
  # Here, we assume that the results lie in results_10
  gckd_pred_files_10 <- list.files(paste(gckd_path, "results_10", sep = .Platform$file.sep ), pattern="*.prediction", full.names=TRUE, recursive = TRUE)
  
  setting <- "gckd_data_cc_fn_d7"
  approaches <- c("Naive", "imputeNode", "imputeRoot", "imputeOnce")
  
  # inread
  gckd_chf_10 <-  lapply(gckd_pred_files_10, read.table, sep ="", skip = 4)
  
  # names
  names(gckd_chf_10) <- gsub(gckd_pred_files_10,
                             pattern = paste( gckd_path, "results_10", sep = .Platform$file.sep ), replacement = "") %>% 
    gsub(pattern = ".prediction", replacement = "") %>% 
    gsub(pattern = paste("^.*", .Platform$file.sep,"p_", sep =""), replacement = "")
  
  
  time_names <- lapply(gckd_pred_files_10, read.table, sep ="", skip = 1, nrow=1 )
  
  # 1) compute hazard
  #simulation_h <- lapply(simulation_chf, function(x) (cbind(x, 0) - cbind(0, x))) #[, time_vector+1]
  # 2) compute CIF as CIF = 1 - prod( 1- haz)
  #simulation_cif <- lapply(simulation_h, function(x) 1- matrixStats::rowCumprods(as.matrix(1-x), cols = time_vector))
  # 1) compute CIF via 1-s = 1-exp(-Lambda), Lambda = chf
  
  for(i in 1:length(gckd_chf_10)){
    names(gckd_chf_10[[i]]) <- time_names[[i]]
  }
  gckd_cif_10 <- lapply(gckd_chf_10, function(x) 1-exp(-x))
  
  # 3) compute mean CIF
  gckd_mean_cif_10 <- lapply(gckd_cif_10, colMeans)
  
  # convert into df
  gckd_mean_cif_df_10 <-  bind_rows(gckd_mean_cif_10, .id = 'simulation')
  
  # pivot longer for plotting
  gckd_mean_cif_df_long_10 <- cbind(gckd_mean_cif_df_10, "X0" = 0) %>%
    pivot_longer(-simulation, names_to = "time", values_to = "mean_cif")%>%
    mutate(time = gsub(time, pattern = "X", replacement = ""),
           discrete_tp = gsub(simulation, pattern = "^.*_d", replacement = ""),
           seed2 = gsub(discrete_tp, pattern = "^.*_", replacement = ""),
           discrete_tp = gsub(discrete_tp, pattern = "_.*$", replacement = ""),
           type = gsub(simulation, pattern = "_.*$", replacement = ""))
  
  
  # Pooled CIF of ten imputation runs
  pooled10 <- gckd_mean_cif_df_long_10 %>% select(seed2, time, mean_cif) %>% 
    mutate(time = as.numeric(time)) %>% 
    pivot_wider(names_from = seed2, values_from = mean_cif) %>%
    arrange(time) %>% 
    data.frame() %>% 
    rowwise() %>%
    mutate(pooled = mean(c_across(c( X101, X102, X103, X104, X105, X106, X107, X108,,X109,X110)))
    )
  
  pooled10 %>%
    filter(time != 0) %>%
    kableExtra::kable(caption = "Pooled CIF of 10 imputations",
                      digits = 4) %>% # add , format = "latex" to get latex table
    kableExtra::kable_styling(latex_options = c("striped"), full_width = FALSE)
}

