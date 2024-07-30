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


# This document provides a description on how to access the other files in the 
# repository. Following the Readme allows to replicate the simulation results of
# Random Survival Forests with Competing Events: A Subdistribution-Based Imputation Approach
# by: C Behning, A Bigerl, M N Wright, P Sekula, M Berger, M Schmid

# For questions or comments, please contact Charlotte Behning at University Bonn:
# behning@imbie.uni-bonn.de

# This code has been written in R version 4.3.0
# The working directory is expected to be the same for all settings. If the code is not 
# run as pulled from the git project, please make sure to set the workspace appropriately,
# e.g. by setwd(). Subfolders will be created in some of the scripts.

# We prepared a master script called rsf_ce_master.R to help you run the code.

# If the training with the random survival forests is to be reproduced, the adapted
# ranger version must be compiled and installed on your machine.
# It needs to be pulled and compiled from this branch of the ranger project:
# https://github.com/cbehning/ranger
# The c++ command line standalone is then used to run the simulation and application.

# Functions for generating the data of the simulation setting are provided in data_generation.R
# Utility functions are included in functions_data_generating.R
# Explanations on how to train and predict ranger from the command line can be found in  
# train_predict.R . To run this code, it is necessary, that the ranger version is compiled.
# In line 120, please set the path to ranger (e.g. ranger.exe on windows or ranger on linux).

# Prediction results for 1 simulation run are available in the repository and can be
# extracted using untar("pred_setup1.tar.gz").

# The prediction results need further preprocessing and the required format for plotting
# is created in preprocessing_results.R

# WARNING: Running all 90,000 simulation setting (both 45,000 for each setup), is very time
# and resource consuming. If these need to be run, the number of repeats in data_generation.R
# train_predict.R and preprocessing_results.R need to be increased. With the current settings,
# Only one simulation run will be performed.

# To reproduce the simulation run for setup1 and setup2, please load the 
# results needed for creating plots and tables:
#"<setup>_status_freq.rda" contains status frequencies of all runs.
#"<setup>_importance.rda" contains all VIMP measures.
#"<setup>_results_data.rda" contains summary statistics needed to create all figure and tables

# To reproduce the figures presented in the manuscript please run the master files or directly run the
# evaluation file. 
# If all data was reproduced, please make sure to save them in preprocessing results, i.e. replace
# the .rda files mentioned above by your own simulation runs.

# The evaluation file loads all summarized measures of the original application data.
# However, Tables S5-S10 are created pm a synthetic mock data set and therefore lead to differnt results
# than presented in the mansucript-

# To run the application with mock data, please refer to  application.R
# Here, you can find  instructions to train and predict and preprocess results using a
# synthetic data subset of the gckd data.
# This mock data can be loaded with 
# load("syn_gckd.rda")
# To follow the instructions in application.R, the ranger C++ code has to be compiled first.
# Please note, that the synthetic data loaded here will only produce similar, but not
# identical results as the original GCKD data.


# The file rsf_ce_master.R contains the master script and can be used to run all other files 
# without manually opening them.



####Session Info####

# session Info of the device the code was run on
# sessionInfo() 
# R version 4.3.0 (2023-04-21 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# 
# Matrix products: default
# 
# 
# locale:
# [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8    LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] kableExtra_1.3.4 lubridate_1.9.2  forcats_1.0.0    stringr_1.5.0    dplyr_1.1.2      purrr_1.0.1      readr_2.1.4      tidyr_1.3.0     
# [9] tibble_3.2.1     ggplot2_3.4.2    tidyverse_2.0.0 
# 
# loaded via a namespace (and not attached):
# [1] treeClust_1.1-7     tidyselect_1.2.0    viridisLite_0.4.2   farver_2.1.1        fastmap_1.1.1       digest_0.6.31      
# [7] rpart_4.1.19        timechange_0.2.0    lifecycle_1.0.3     cluster_2.1.4       survival_3.5-5      magrittr_2.0.3     
# [13] compiler_4.3.0      sass_0.4.6          rlang_1.1.1         tools_4.3.0         functional_0.6      utf8_1.2.3         
# [19] data.table_1.14.8   knitr_1.42          labeling_0.4.2      timereg_2.0.5       xml2_1.3.4          withr_2.5.0        
# [25] numDeriv_2016.8-1.1 pec_2023.04.12      geepack_1.3.9       grid_4.3.0          stats4_4.3.0        fansi_1.0.4        
# [31] discSurv_2.0.0      colorspace_2.1-0    future_1.32.0       globals_0.16.2      scales_1.2.1        iterators_1.0.14   
# [37] MASS_7.3-58.4       cli_3.6.1           mvtnorm_1.1-3       rmarkdown_2.21      ragg_1.2.5          generics_0.1.3     
# [43] rstudioapi_0.14     future.apply_1.11.0 httr_1.4.6          tzdb_0.4.0          cachem_1.0.8        splines_4.3.0      
# [49] rvest_1.0.3         parallel_4.3.0      vctrs_0.6.2         shadowtext_0.1.2    webshot_0.5.4       Matrix_1.5-4       
# [55] jsonlite_1.8.4      VGAM_1.1-8          hms_1.1.3           listenv_0.9.0       systemfonts_1.0.4   foreach_1.5.2      
# [61] jquerylib_0.1.4     glue_1.6.2          parallelly_1.36.0   codetools_0.2-19    mvnfast_0.2.8       cowplot_1.1.1      
# [67] stringi_1.7.12      gtable_0.3.3        munsell_0.5.0       pillar_1.9.0        htmltools_0.5.5     lava_1.7.2.1       
# [73] R6_2.5.1            textshaping_0.3.6   Rdpack_2.4          evaluate_0.21       lattice_0.21-8      highr_0.10         
# [79] rbibutils_2.2.13    backports_1.4.1     broom_1.0.4         bslib_0.4.2         Rcpp_1.0.10         svglite_2.1.1      
# [85] nlme_3.1-162        prodlim_2023.03.31  mgcv_1.8-42         ranger_0.15.1       xfun_0.39           pkgconfig_2.0.3 






