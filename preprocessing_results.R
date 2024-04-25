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


# The following contains R-code to preporcess result data, and will create the
# data loaded with 

# load(paste(setup,"_results_data.rda", sep =  ""))


# First, all necessary training data files must be created.
# The training and the prediction of the forest needs to be run,
# such that prediction files exist.
# Reading the data of 1000 simulation runs can become resource consuming.
# Using a device with at least 32GB RAM is recommended.

## Input:
# n:          number of subjects (rows of the final dataframe)
# p:          parameter affecting the probability of type 1 events 
# b:          parameter affecting the degree of censoring 
# vars:       number of covariates
# setup:      simulation setup1 or setup2. Coded with setup1: scenario 231121, setup2: scenario 231213
# seed:       seed to start simulation with (paper used 71123 as stating seed)
# repeats:    number of simulation repeats -1

require(tidyverse)
source("functions_data_generating.R")

# define setup to evaluate
setup <- "setup1"

# set a seed
seed_start <- 71123
# set number of repeats
repeats <- 1-1 
# number of subjects evaluated
n <- 1000
n_pattern <- paste("_", n, "_", sep = "")


## possible values for parameter affecting e1 rate
#p_vector <- c(0.2, 0.4, 0.8)
## possible values for parameter affecting censoring rate
# b_vector <- c(0.85, 1, 1.25)

# Path to training data files
# ! Training data data needs to be generated using data_generation.R
simulation_data_path <- paste(paste("data_", setup,  .Platform$file.sep, sep =""))

# Path to prediction files
# in the repository, only results for one seed (71123) are available
# ! Training and prediction as described in train_predict.R has to be executed
# ! Results (.prediction and .importance files) need to be stored in simulation_path
#    or subdirectories.
#  create subfolder von each n
simulation_path <- paste("pred_", setup, .Platform$file.sep, sep = "")


# read files used to train data
filenames_p <- list.files(simulation_data_path, pattern="*_k.csv", full.names=TRUE, recursive = TRUE)
filenames_oio <- list.files(simulation_data_path, pattern="oio_.*.csv", full.names=TRUE, recursive = TRUE)

# remove raw and true CIF files
filenames_p <- filenames_p[!str_detect(filenames_p, pattern = "raw_data") & !str_detect(filenames_p, pattern = "obj_List") &
                             !str_detect(filenames_p, pattern = "true_cdf") & !str_detect(filenames_p, pattern = "oio")]
filenames_oio <- filenames_oio[!str_detect(filenames_oio, pattern = "raw_data") & !str_detect(filenames_oio, pattern = "obj_List") & !str_detect(filenames_oio, pattern = "true_cdf")]

# only keep files corresponding to parameter n
filenames_p <- filenames_p[str_detect(filenames_p, pattern = paste(n_pattern, "p", sep = "")) ]
filenames_oio <- filenames_oio[str_detect(filenames_oio, pattern = n_pattern)]


## Inread files for all settings
# both events present in data (training data fro imputeRoot und imputeNodes)
simulation_data_files <- lapply(filenames_p[!str_detect(filenames_p, pattern = "e1") &
                                              !str_detect(filenames_p, pattern = "ignore") ], read.csv)
# ignoring competing event (naive)
simulation_data_files_ignore <- lapply(filenames_p[str_detect(filenames_p, pattern = "ignore")], read.csv)
# one event, only e1 (reference)
simulation_data_files_e1 <- lapply(filenames_p[str_detect(filenames_p, pattern = "e1")], read.csv)
# only impute once oio
simulation_data_files_oio <- lapply(filenames_oio, read.csv)


# create shorter names
names(simulation_data_files) <- gsub(filenames_p[!str_detect(filenames_p, pattern = "e1") & !str_detect(filenames_p, pattern = "ignore")],
                                     pattern = paste( simulation_data_path, "/", sep = "" ),
                                     replacement = "") %>% 
  gsub(pattern = ".csv", replacement = "") %>% 
  gsub(pattern = "^.*/", replacement = "")

names(simulation_data_files_ignore) <- gsub(filenames_p[str_detect(filenames_p, pattern = "ignore")],
                                            pattern = paste( simulation_data_path, "/", sep = "" ),
                                            replacement = "") %>% 
  gsub(pattern = ".csv", replacement = "") %>% 
  gsub(pattern = "^.*/", replacement = "")

names(simulation_data_files_e1) <- gsub(filenames_p[str_detect(filenames_p, pattern = "e1")],
                                        pattern = paste( simulation_data_path, "/", sep = "" ),
                                        replacement = "") %>% 
  gsub(pattern = ".csv", replacement = "") %>% 
  gsub(pattern = "^.*/", replacement = "")

names(simulation_data_files_oio) <- gsub(filenames_oio,
                                         pattern = paste( simulation_data_path, "/", sep = "" ),
                                         replacement = "") %>% 
  gsub(pattern = ".csv", replacement = "") %>% 
  gsub(pattern = "^.*/", replacement = "")


# read names of all prediction files
simulation_pred_files <- list.files(paste(simulation_path, "n_", n, "/", sep = ""), pattern="*.prediction", full.names=TRUE, recursive = TRUE)

## Inread all prediction files
# impuation in all nodes (imputeNodes)
simulation_chf <-  lapply(simulation_pred_files[!str_detect(simulation_pred_files, pattern = "e1") & !str_detect(simulation_pred_files, pattern = "ignore") & !str_detect(simulation_pred_files, pattern = "oio") & !str_detect(simulation_pred_files, pattern = "oinroot")],
                          read.table, sep ="", skip = 4)
# ignoring competing events mechanism (naive)
simulation_chf_ignore <- lapply(simulation_pred_files[str_detect(simulation_pred_files, pattern = "ignore")  ],  read.table, sep ="", skip = 4)
# For true event e1  (reference)
simulation_chf_e1 <-  lapply(simulation_pred_files[str_detect(simulation_pred_files, pattern = "e1")], read.table, sep ="", skip = 4)
# only impute once - oio (impuation outside the tree)
simulation_chf_oio <-  lapply(simulation_pred_files[str_detect(simulation_pred_files, pattern = "oio")], read.table, sep ="", skip = 4)
# only impute in root nodes - oinroot
simulation_chf_oinroot <-  lapply(simulation_pred_files[str_detect(simulation_pred_files, pattern = "oinroot")], read.table, sep ="", skip = 4)

# create shorter names for prediction files
names(simulation_chf) <- gsub(simulation_pred_files[!str_detect(simulation_pred_files, pattern = "e1") & !str_detect(simulation_pred_files, pattern = "ignore")  & !str_detect(simulation_pred_files, pattern = "oio") & !str_detect(simulation_pred_files, pattern = "oinroot")],
                              pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".prediction", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")

names(simulation_chf_ignore) <- gsub(simulation_pred_files[str_detect(simulation_pred_files, pattern = "ignore")],
                                     pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".prediction", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")

names(simulation_chf_e1) <- gsub(simulation_pred_files[str_detect(simulation_pred_files, pattern = "e1")],
                                 pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".prediction", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")


names(simulation_chf_oio) <- gsub(simulation_pred_files[str_detect(simulation_pred_files, pattern = "oio")],
                                  pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".prediction", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")

names(simulation_chf_oinroot) <- gsub(simulation_pred_files[str_detect(simulation_pred_files, pattern = "oinroot")],
                                      pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".prediction", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")


time_names <- lapply(simulation_pred_files[!str_detect(simulation_pred_files, pattern = "e1") &
                                             !str_detect(simulation_pred_files, pattern = "ignore") & !str_detect(simulation_pred_files, pattern = "oio") & !str_detect(simulation_pred_files, pattern = "oinroot")], read.table, sep ="", skip = 1, nrow=1 )

time_names_e1 <- lapply(simulation_pred_files[str_detect(simulation_pred_files, pattern = "e1")], read.table, sep ="", skip = 1, nrow=1 )

time_names_ignore <- lapply(simulation_pred_files[str_detect(simulation_pred_files, pattern = "ignore")], read.table, sep ="", skip = 1, nrow=1 )

time_names_oio <- lapply(simulation_pred_files[str_detect(simulation_pred_files, pattern = "oio")], read.table, sep ="", skip = 1, nrow=1 )

time_names_oinroot <- lapply(simulation_pred_files[str_detect(simulation_pred_files, pattern = "oinroot")], read.table, sep ="", skip = 1, nrow=1 )

## For each setting:
# 1) compute hazard
#simulation_h <- lapply(simulation_chf, function(x) (cbind(x, 0) - cbind(0, x))) #[, time_vector+1]
# 2) compute CIF as CIF = 1 - prod( 1- haz)
#simulation_cif <- lapply(simulation_h, function(x) 1- matrixStats::rowCumprods(as.matrix(1-x), cols = time_vector))
# 1) compute CIF via 1-s = 1-exp(-Lambda), Lambda = chf

# imputeNodes
for(i in 1:length(simulation_chf)){
  names(simulation_chf[[i]]) <- time_names[[i]]
}
# Naive
for(i in 1:length(simulation_chf_ignore)){
  names(simulation_chf_ignore[[i]]) <- time_names_ignore[[i]]
}
# Reference
for(i in 1:length(simulation_chf_e1)){
  names(simulation_chf_e1[[i]]) <- time_names_e1[[i]]
}
# imputeOnce
for(i in 1:length(simulation_chf_oio)){
  names(simulation_chf_oio[[i]]) <- time_names_oio[[i]]
}
# imputeRoot
for(i in 1:length(simulation_chf_oinroot)){
  names(simulation_chf_oinroot[[i]]) <- time_names_oinroot[[i]]
}

# repair and add missing event times so that all data frames have the time span from 1:20
simulation_chf_120 <- lapply(simulation_chf, function(x) repair_dataframe(df = x))
simulation_chf_ignore_120 <- lapply(simulation_chf_ignore, function(x) repair_dataframe(df = x))
simulation_chf_e1_120 <- lapply(simulation_chf_e1, function(x) repair_dataframe(df = x))
simulation_chf_oio_120 <- lapply(simulation_chf_oio, function(x) repair_dataframe(df = x))
simulation_chf_oinroot_120 <- lapply(simulation_chf_oinroot, function(x) repair_dataframe(df = x))

# 2) compute CIF from CHF
simulation_cif <- lapply(simulation_chf_120, function(x) 1-exp(-x))
simulation_cif_ignore <- lapply(simulation_chf_ignore_120, function(x) 1-exp(-x))
simulation_cif_e1 <- lapply(simulation_chf_e1_120, function(x) 1-exp(-x))
simulation_cif_oio <- lapply(simulation_chf_oio_120, function(x) 1-exp(-x))
simulation_cif_oinroot <- lapply(simulation_chf_oinroot_120, function(x) 1-exp(-x))

# 3) compute mean CIF across all simulation runs
simulation_mean_cif <- lapply(simulation_cif, colMeans)
simulation_mean_cif_e1 <- lapply(simulation_cif_e1, colMeans)
simulation_mean_cif_ignore <- lapply(simulation_cif_ignore, colMeans)
simulation_mean_cif_oio <- lapply(simulation_cif_oio, colMeans)
simulation_mean_cif_oinroot <- lapply(simulation_cif_oinroot, colMeans)

# convert into dataframes
simulation_mean_cif_df <-  bind_rows(simulation_mean_cif, .id = 'simulation')
simulation_mean_cif_df_e1 <-  bind_rows(simulation_mean_cif_e1, .id = 'simulation')
simulation_mean_cif_df_ignore <-  bind_rows(simulation_mean_cif_ignore, .id = 'simulation')
simulation_mean_cif_df_oio <-  bind_rows(simulation_mean_cif_oio, .id = 'simulation')
simulation_mean_cif_df_oinroot <-  bind_rows(simulation_mean_cif_oinroot, .id = 'simulation')

# Show results on test data
set <- "test"


## Pivot all dfs to longer format  for plotting
# ImputeNodes
simulation_mean_cif_long <- cbind(simulation_mean_cif_df, "X0" = 0) %>%
  pivot_longer(-simulation, names_to = "time", values_to = "mean_cif")%>%
  mutate(time = gsub(time, pattern = "X", replacement = ""),
         p = gsub(simulation, pattern = paste(set, "_crsubdist_0", sep =""), replacement = "0.") %>%
           gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
         data_seed = gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
         b =  gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "_[^_]+$", replacement = "" ) %>%
           gsub(pattern = "^.*_", replacement = ""),
         n = gsub(simulation, pattern = "^.*_", replacement = ""))
# Reference
simulation_mean_cif_long_e1 <- cbind(simulation_mean_cif_df_e1, "X0" = 0) %>%
  pivot_longer(-simulation, names_to = "time", values_to = "mean_cif")%>%
  mutate(time = gsub(time, pattern = "X", replacement = ""),
         p = gsub(simulation, pattern = paste(set, "_crsubdist_e1_0", sep =""), replacement = "0.") %>%
           gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
         data_seed = gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
         b =  gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "_[^_]+$", replacement = "" ) %>%
           gsub(pattern = "^.*_", replacement = ""),
         n = gsub(simulation, pattern = "^.*_", replacement = ""))
# Naive
simulation_mean_cif_long_ignore <- cbind(simulation_mean_cif_df_ignore, "X0" = 0) %>%
  pivot_longer(-simulation, names_to = "time", values_to = "mean_cif")%>%
  mutate(time = gsub(time, pattern = "X", replacement = ""),
         p = gsub(simulation, pattern = paste(set, "_crsubdist_ignore_0", sep =""), replacement = "0.") %>%
           gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
         data_seed = gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
         b =  gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "_[^_]+$", replacement = "" ) %>%
           gsub(pattern = "^.*_", replacement = ""),
         n = gsub(simulation, pattern = "^.*_", replacement = ""))
# Impute Once
simulation_mean_cif_long_oio <- cbind(simulation_mean_cif_df_oio, "X0" = 0) %>%
  pivot_longer(-simulation, names_to = "time", values_to = "mean_cif")%>%
  mutate(time = gsub(time, pattern = "X", replacement = ""),
         p = gsub(simulation, pattern = paste(set, "_crsubdist_oio_0", sep =""), replacement = "0.") %>%
           gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
         data_seed = gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
         b =  gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "_[^_]+$", replacement = "" ) %>%
           gsub(pattern = "^.*_", replacement = ""),
         n = gsub(simulation, pattern = "^.*_", replacement = ""))
#ImputeRoot oinroot
simulation_mean_cif_long_oinroot <- cbind(simulation_mean_cif_df_oinroot, "X0" = 0) %>%
  pivot_longer(-simulation, names_to = "time", values_to = "mean_cif")%>%
  mutate(time = gsub(time, pattern = "X", replacement = ""),
         p = gsub(simulation, pattern = paste(set, "_crsubdist_oinroot_0", sep =""), replacement = "0.") %>%
           gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
         data_seed = gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
         b =  gsub(simulation, pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "_[^_]+$", replacement = "" ) %>%
           gsub(pattern = "^.*_", replacement = ""),
         n = gsub(simulation, pattern = "^.*_", replacement = ""))


# done

time_vector <- c(0:20)


#### Create all_data df####
filter_seed <- c(seed_start:(seed_start+repeats))

all_data <- rbind(
  # imputeNodes 
  simulation_mean_cif_long %>% 
    filter(data_seed %in% filter_seed) %>%
    mutate(n = as.numeric(n)) %>%
    group_by(n, b, p, time) %>%
    summarise(mean_cif = mean(mean_cif, na.rm = TRUE)) %>%
    mutate(time = as.numeric(time),
           type = "imputeNode"),
  #Reference
  simulation_mean_cif_long_e1 %>%
    filter(data_seed %in% filter_seed) %>%
    mutate(across( c(n, time),  ~as.numeric(.))) %>%
    group_by(n, b, p, time) %>%
    summarise(mean_cif = mean(mean_cif, na.rm = TRUE)) %>%
    mutate(type = "Reference") ,
  # Naive
  simulation_mean_cif_long_ignore %>%
    filter(data_seed %in% filter_seed) %>%
    mutate(n = as.numeric(n)) %>%
    group_by(n, b, p, time) %>%
    summarise(mean_cif = mean(mean_cif, na.rm = TRUE)) %>%
    mutate(time = as.numeric(time),
           type = "Naive approach"),
  # imputeOnce
  simulation_mean_cif_long_oio %>%
    filter(data_seed %in% filter_seed) %>%
    mutate(n = as.numeric(n)) %>%
    group_by(n, b, p, time) %>%
    summarise(mean_cif = mean(mean_cif, na.rm = TRUE)) %>%
    mutate(time = as.numeric(time),
           type = "imputeOnce"),
  # ImputeRoot
  simulation_mean_cif_long_oinroot %>%
    filter(data_seed %in% filter_seed) %>%
    mutate(n = as.numeric(n)) %>%
    group_by(n, b, p, time) %>%
    summarise(mean_cif = mean(mean_cif, na.rm = TRUE)) %>%
    mutate(time = as.numeric(time),
           type = "imputeRoot"),
)



