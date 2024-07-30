#########################################################################
#                                                                       #
#          Random Survival Forests with Competing Events:               #
#           A Subdistribution-Based Imputation Approach                 #
#                                                                       #
#    C Behning, A Bigerl, M N Wright, P Sekula, M Berger, M Schmid      #
#                                                                       #
#                           MASTER                                      #
#                                                                       #
#########################################################################
#This master file summarizes the results of the analysis
#Please run all other files beforehand (see Read Me for instructions)
####################
####Session Info####
####################
#session Info of the device the code was run on
#sessionInfo() 
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



#Of note: Some of the results were produced outside of R using a C++ standalone.
#Please refer to the README file for details 

###############
####Library####
###############
library(tidyverse)
library(discSurv)
library(kableExtra)
library(cowplot)

# load functions
source("functions_data_generating.R")

#set WD
#setwd("Please define")

#######################################
##### Part 1: Intermediate results ####
#######################################

# A)  Generate the simulation train and test files
# The data_generation.R file contains all function calls used to generate data for the simulations.
# These will serve as input data for the RSF and are devided into a training and a test dataset.
# With the current settings, only data for one simulation run (one seed) for setup 1 will be created.
 source("data_generation.R")
# Input data for each seed will be stored in sub folders of data_setup1/ .
# To reproduce the data of the paper, 1000 simulation runs need to be performed.
# To do so, please change the repeats in line 45 to 1000-1.
# To chose setup 2, change to "setup2" in line 37.
# WARNING: Creating and saving the input data needs ~14,1 MB per repeat and might add up to 15 GB
# for 1000 repeats. 


# B) Train the random forests and get predictions
# To run the random survival forest, please refer to the installation descrition of the C++ standalone in:
# source(train_predict.R)
# first. Without a compiled version of the modified ranger, train_predict-R cannot be executed. 
# The C++ standalone can then be run either using bash scripts or directly from the command line.
# The train_predict.R will call the command line and run a single simulation run (5 approaches, 9 settings).
# WARNING: We do not advise the following:
# Bash scripts are provided in bash/ to run all 1000 simulation setups and can only be run, if all 1000
# data settings were created first.
# WARNING: The bash code will run a long time and use a lot of resources. 

# For one simulation run of all 9 settings and 5 approaches, we provide the intermediate prediction results in the folder
# pred_setup1/n_1000/
# For the intermediate variable importance results, please see
# pred_setup1/imp2/
# These can be extracted using:
untar("pred_setup1.tar.gz")

# also, example output to create the illustration figure of G are included pred_setup1/
# These G hat data were included for illustration purpose. As this requires to install a version of ranger, that
# prints *all* intermediate outputs, we would not advice rerunning this. However, if needed, the version streaming
# all intermediate output can be found at https://github.com/cbehning/ranger/tree/competing_risks_subdist_retrieve-cens-times (22.07.2024)
# It can be istalled analogous to the description in train_predict.R and due to it's printing cannot be used for productive model fitting.

# C) Preprocessing of results
# The file "preprocessing_results.R" contains  all processing steps to gather simulation results in the required format
# for plotting. It can be executed after unzipping the folder pred_setup1, but only for
# one simulation run of setup1. The data generated by 1000 repeats of the simulation 
# are provided as summary statistics in this repository.

# To run the evaluation without running 1000 repeats, you can load the following files (also loaded in evaluation.R)
#"<setup>_status_freq.rda" contains status frequencies of all runs.
#"<setup>_importance.rda" contains all VIMP measures.
#"<setup>_results_data.rda" contains summary statistics needed to create all figure and tables.
# To run the evaluation on your own 1000 repeats, please make sure to save the files accordingly 
# as commented in "preprocessing_results.R" (line 96, line 709, line 719).


################################
##### Part 2:  Evaluation   ####
################################
# The script evaluation.R creates all figures and tables of the simulation results as presented in the manuscript
# or the supplement. The script also creates figures and tables of application of RSF methods on GCKD data.
source("evaluation.R")
# The setup parameter in line 28 determines which simulation setup is evaluated. Change to "setup2" to get for setup 2.
# Please note, that the script will execute data_generation.R, if the necessary files of seed 71123 do not already exist.
# WARNING: In case you have modified the parameters in data_generation.R, running evaluation.R might become
# resource intensive (if no data for seed 71123 exists.)
# Please note: Results for Table S5-S10 are created on the mock data and therefore lead to slightly different results that 
# the original data set.


# APPLICATION
# To run the Application using a synthetic mock data set, please refer to 
source("application.R")
# This script will generate data for training and it contains further instruction to train, predict and preprocess results using a
# synthetic data subset of the gckd data.
# Please note, that the synthetic data loaded here will only produce similar, but not
# identical results as the original GCKD data.
# For evaluation, summary measures of the original data are provided and loaded in evaluation.R















