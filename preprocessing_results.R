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


# The following contains R-code to preprocess result data, and will create the
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


#### Compute C-index ####
#consider e1 data as true reference data

# caluclate marker values.
# for each subject (row) calculate the cumsum of H

if(TRUE){ # can take a long time to run, when repeats get larger
  markerlist <- list()
  markerlist[["impute"]] <- lapply(simulation_chf_120, rowSums)
  markerlist[["e1"]] <- lapply(simulation_chf_e1_120, rowSums)
  markerlist[["ignore"]] <- lapply(simulation_chf_ignore_120, rowSums)
  markerlist[["oio"]] <- lapply(simulation_chf_oio_120, rowSums)
  markerlist[["oiroot"]] <- lapply(simulation_chf_oinroot_120, rowSums)
  
  # initialize dataframe for C-index
  cindex_df <- expand.grid(method = c("impute", "e1", "ignore", "oio", "oiroot"),
                           b = c("0.85", "1", "1.25"),
                           p = c("2", "4", "8"),
                           k = 50,
                           seed = filter_seed,
                           n = 1000,
                           cindexCR = NA,
                           cindex = NA)
  
  for(i in  1:nrow(cindex_df)){
    
    if(i %% 1000==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", i, "\n"))
    }
    
    p <- cindex_df$p[i]  
    b <- cindex_df$b[i]  
    n <- cindex_df$n[i]
    k <- cindex_df$k[i]
    method <- cindex_df$method[i]
    seed <- cindex_df$seed[i]
    
    # get training data
    test_data <- simulation_data_files[[paste("data_test_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k" , sep = "")]][, c("time", "status")] 
    test_data$e1 <- (test_data$status == 1)*1
    test_data$e2 <- (test_data$status == 2)*1
    test_time <- test_data$time
    test_status <- test_data[, c("e1", "e2")]
    # get test 
    train_data <- simulation_data_files[[paste("data_train_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k",  sep = "")]][, c("time", "status")]
    train_data$e1 <- (train_data$status == 1)*1
    train_data$e2 <- (train_data$status == 2)*1
    train_time <- train_data$time
    train_status <- train_data[, c("e1", "e2")]
    
    if(method == "impute"){
      markername <- paste("test_crsubdist_0",p,"_",b,"_",seed,"_", n, sep = "")
    }else if(method =="e1"){
      markername <- paste("test_crsubdist_e1_0",p,"_",b,"_",seed,"_", n, sep = "")
    }else if(method =="ignore"){
      markername <- paste("test_crsubdist_ignore_0",p,"_",b,"_",seed,"_", n, sep = "")
    }else if(method =="oio"){
      markername <- paste("test_crsubdist_oio_0",p,"_",b,"_",seed,"_", n, sep = "")
    }else if(method == "oiroot"){
      markername = paste("test_crsubdist_oinroot_0",p,"_",b,"_",seed,"_", n, sep = "")
    }
    marker <- markerlist[[method]][[markername]] %>% as.matrix()
    
    # x <- discSurv::cIndexCompRisks(marker = marker,
    #                                testTime = test_time,
    #                                testEvents = test_status,
    #                                trainTime = train_time,
    #                                trainEvents = train_status
    # )
    # 
    # cindex_df[i, "cindexCR"]  <- x
    
    
    cindex_df[i, "cindex"]  <- discSurv::cIndex(marker = marker[, 1], 
                                                testTime = test_time,
                                                testEvent = test_status[,1], 
                                                trainTime = train_time,
                                                trainEvent = train_status[, 1 ]
    )
  }
  
  write.csv(cindex_df, file= paste(simulation_path, "cindex_df_rep1000.csv", paste =""), row.names = FALSE)
}else{
  cindex_df<- read.csv(file= paste(simulation_path, "cindex_df_rep1000.csv", paste =""))
}

mapping_method_type  <- data.frame(
  method = c("impute",  "ignore", "oio", "e1", "oiroot"), 
  type =  c( "imputeNode",  "Naive approach", "imputeOnce", "Reference", "imputeRoot" ))

## Tables and graphs for Brier Score

# use package "pec" to compute prediction error curves and Brier Scores

require(prodlim)
require(pec)


if(TRUE){ # can take long with increasing number of repeats, run once and save data
  tmax <- 20
  scenario <- gsub("^.*_([0-9]+)\\/", "\\1", simulation_path)
  
  bierscore_df <- expand.grid(method = c("impute", "e1", "ignore", "oio", "oiroot"),
                              b = c("0.85", "1", "1.25"),
                              p = c("2", "4", "8"),
                              seed = filter_seed,
                              n = 1000,
                              k = 50, 
                              IBS = NA)
  bierscore_2<- matrix(nrow = nrow(bierscore_df), ncol = 20 ) #[, paste0("bs_", 1:20)] <- NA
  
  for(i in  1:nrow(bierscore_df)){ #
    
    if(i %% 1000==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", i, "\n"))
    }
    
    p <- bierscore_df$p[i]  
    b <- bierscore_df$b[i]  
    n <- bierscore_df$n[i]
    k <- bierscore_df$k[i]
    method <- bierscore_df$method[i]
    seed <- bierscore_df$seed[i]
    
    test_data <- simulation_data_files[[paste("data_test_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k" , sep = "")]][, c("time", "status")] 
    
    train_data <- simulation_data_files[[paste("data_train_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k" , sep = "")]][, c("time", "status")]
    
    
    
    if(method == "impute"){
      markername <- paste("test_crsubdist_0",p,"_",b,"_",seed,"_", n, sep = "")
      sprCIF <- simulation_cif[[markername]] %>% as.matrix()
    }else if(method =="e1"){
      markername <- paste("test_crsubdist_e1_0",p,"_",b,"_",seed,"_", n, sep = "")
      sprCIF <- simulation_cif_e1[[markername]] %>% as.matrix()
    }else if(method =="ignore"){
      markername <- paste("test_crsubdist_ignore_0",p,"_",b,"_",seed,"_", n, sep = "")
      sprCIF <- simulation_cif_ignore[[markername]] %>% as.matrix()
    }else if(method =="oio"){
      markername <- paste("test_crsubdist_oio_0",p,"_",b,"_",seed,"_", n, sep = "")
      sprCIF <- simulation_cif_oio[[markername]] %>% as.matrix()
    }else if(method =="oiroot"){
      markername <- paste("test_crsubdist_oinroot_0",p,"_",b,"_",seed,"_", n, sep = "")
      sprCIF <- simulation_cif_oinroot[[markername]] %>% as.matrix()
    }
    
    
    
    #sprCIF <- as.matrix(simulation_cif[[paste("test_crsubdist_0",p,"_",b,"_",seed,"_",n, sep = "")]] )
    
    tmax_i <- max(test_data$time)
    timepoints_i <- timepoints_i_0 <- test_data$time %>% unique() %>% sort()
    if(length(timepoints_i_0) == timepoints_i_0[length(timepoints_i_0)]){
      timepoints_i_0 <- timepoints_i_0[-length(timepoints_i_0)]
      #timepoints_i_0 <- 1:(tmax-1)
    }else if(length(timepoints_i) < tmax_i ){
      timepoints_i <- 1:(length(timepoints_i) +1)
    }
    # print("-----------------")
    # print(paste("i:", i))
    # print(length(timepoints_i_0))
    # print(length(timepoints_i))
    # print(tmax_i)
    
    
    # prediction error curves
    PECS <- pec::pec(matrix(sprCIF[, timepoints_i], nrow = nrow(sprCIF)), # matrix containing the predicted survival curves
                     formula = Hist(time, status) ~1, # the formula for the censoring model
                     data = cbind(test_data, covar = 1), # test data
                     #traindata = train_data, # training data
                     times =timepoints_i_0, # vector of time points on which to evaluate
                     cens.model = "marginal",
                     cause=1,
                     exact = FALSE
    )
    
    bierscore_df[i, "IBS"] <- pec::ibs(PECS)[2]
    bierscore_2[i,1:tmax_i] <- PECS$AppErr$matrix[1:tmax_i]
    
    
    #pec cindex
    
    # cINDs<- pec::cindex(matrix(sprCIF[, timepoints_i], nrow = nrow(sprCIF)), 
    #             formula= Hist(time, status) ~1,
    #             data =  cbind(test_data, covar = 1), 
    #             eval.times = timepoints_i, 
    #             #pred.times = seq(1, 14, 1), 
    #             cause=1, 
    #             lyl = FALSE,
    #             cens.model = "marginal"
    #)
  }
  
  
  write.csv(bierscore_df, file= paste(simulation_path, "integrated_brierscore_",n,"_rep1000.csv", sep =""), row.names = FALSE)
  write.csv(bierscore_2, file= paste(simulation_path, "td_brierscore_",n,"_rep1000.csv", sep =""), row.names = FALSE) 
}else{
  bierscore_df <- read.csv(paste(simulation_path, "integrated_brierscore_",n,"_rep1000.csv", sep =""))
  bierscore_2 <- paste(simulation_path, "td_brierscore_",n,"_rep1000.csv", sep ="")
}

#### inread importance files ####
# list all files to check number available
importance_files_e1 <- list.files(paste(simulation_path, "imp2/Reference/", sep = ""), pattern="*.importance", full.names=TRUE, recursive = TRUE)
importance_files_nodes <- list.files(paste(simulation_path, "imp2/imputeNodes/", sep = ""), pattern="*.importance", full.names=TRUE, recursive = TRUE)
importance_files_ignore <- list.files(paste(simulation_path, "imp2/Naive/", sep = ""), pattern="*.importance", full.names=TRUE, recursive = TRUE)
importance_files_oio <- list.files(paste(simulation_path, "imp2/imputeOnce/", sep = ""), pattern="*.importance", full.names=TRUE, recursive = TRUE)
importance_files_oiroot <- list.files(paste(simulation_path, "imp2/imputeRoot/", sep = ""), pattern="*.importance", full.names=TRUE, recursive = TRUE)

# for both events
importance_nodes <-  lapply(importance_files_nodes, FUN = inread_importance_value)
# ignoring competing events mechanism
importance_ignore <- lapply(importance_files_ignore, FUN = inread_importance_value)
# and for e1 only
importance_e1 <-  lapply(importance_files_e1, FUN = inread_importance_value)
# only impute once - oio
importance_oio <-  lapply(importance_files_oio, FUN = inread_importance_value)
# only impute in root nodes - oinroot
importance_oinroot <-  lapply(importance_files_oiroot, FUN = inread_importance_value)


# names
names(importance_nodes) <- gsub(importance_files_nodes,
                                pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".importance", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")

names(importance_ignore) <- gsub(importance_files_ignore,
                                 pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".importance", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")

names(importance_e1) <- gsub(importance_files_e1,
                             pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".importance", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")


names(importance_oio) <- gsub(importance_files_oio,
                              pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".importance", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")

names(importance_oinroot) <- gsub(importance_files_oiroot,
                                  pattern = paste( simulation_path, "/", sep = "" ), replacement = "") %>% 
  gsub(pattern = ".importance", replacement = "") %>% gsub(pattern = "^.*/", replacement = "")

# prepare data for importance plot
# create dataframes from list
importance_nodes_df <- bind_rows(importance_nodes, .id = 'simulation') %>% t() %>%data.frame() %>% rownames_to_column() 
importance_ignore_df <- bind_rows(importance_ignore, .id = 'simulation') %>% t() %>%data.frame() %>% rownames_to_column() 
importance_e1_df <- bind_rows(importance_e1, .id = 'simulation') %>% t() %>%data.frame() %>% rownames_to_column() 
importance_oio_df <- bind_rows(importance_oio, .id = 'simulation') %>% t() %>%data.frame() %>% rownames_to_column() 
importance_oinroot_df <- bind_rows(importance_oinroot, .id = 'simulation') %>% t() %>%data.frame() %>% rownames_to_column() 

# pivot to long for plotting
importance_nodes_df_long <- importance_nodes_df %>%
  pivot_longer(-rowname ,  names_to = "column", values_to = "VIMP")%>%
  mutate( p = gsub(rowname , pattern = "crsubdist_0", replacement = "0.") %>%
            gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
          data_seed = gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
            gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
          b =  gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
            gsub(pattern = "_[^_]+$", replacement = "" ) %>%
            gsub(pattern = "^.*_", replacement = ""),
          n = gsub(rowname , pattern = "_123", replacement = "")%>%
            gsub(pattern = "^.*_", replacement = ""),
          type = "imputeNode" )
# true event 1
importance_e1_df_long <- importance_e1_df %>%
  pivot_longer(-rowname , names_to = "column", values_to = "VIMP")%>%
  mutate( p = gsub(rowname , pattern = "crsubdist_e1_0",
                   replacement = "0.") %>%
            gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
          data_seed = gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
            gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
          b =  gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
            gsub(pattern = "_[^_]+$", replacement = "" ) %>%
            gsub(pattern = "^.*_", replacement = ""),
          n = gsub(rowname , pattern = "_123", replacement = "")%>%
            gsub(pattern = "^.*_", replacement = ""),
          type = "Reference")
# ignore: competing event time as censoring time
importance_ignore_df_long <- importance_ignore_df %>%
  pivot_longer(-rowname , names_to = "column", values_to = "VIMP")%>%
  mutate( p = gsub(rowname , pattern = "crsubdist_ignore_0", replacement = "0.") %>%
            gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
          data_seed = gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
            gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
          b =  gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
            gsub(pattern = "_[^_]+$", replacement = "" ) %>%
            gsub(pattern = "^.*_", replacement = ""),
          n = gsub(rowname , pattern = "^.*_", replacement = ""),
          type = "Naive approach")
# only impte once
importance_oio_df_long <- importance_oio_df %>%
  pivot_longer(-rowname ,  names_to = "column", values_to = "VIMP")%>%
  mutate(p = gsub(rowname , pattern = "crsubdist_oio_0", replacement = "0.") %>%
           gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
         data_seed = gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
         b =  gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "_[^_]+$", replacement = "" ) %>%
           gsub(pattern = "^.*_", replacement = ""),
         n = gsub(rowname , pattern = "^.*_", replacement = ""), 
         type = "imputeOnce")
#impute only in root nodes oinroot
importance_oinroot_df_long <-   importance_oinroot_df %>%
  pivot_longer(-rowname , names_to = "column", values_to = "VIMP") %>%
  mutate(p = gsub(rowname , pattern = "crsubdist_oinroot_0", replacement = "0.") %>% gsub(pattern = "_.*$", replacement = "" )%>% as.numeric(),
         data_seed = gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "+$", replacement = "" ) %>% gsub(pattern = "^.*_", replacement = ""),
         b =  gsub(rowname , pattern = "_[^_]+$", replacement = "" ) %>% 
           gsub(pattern = "_[^_]+$", replacement = "" ) %>%
           gsub(pattern = "^.*_", replacement = ""),
         n = gsub(rowname , pattern = "^.*_", replacement = ""),
         type = "imputeRoot")


importance_long <- rbind(importance_e1_df_long,
                         importance_ignore_df_long, 
                         importance_nodes_df_long,
                         importance_oinroot_df_long,
                         importance_oio_df_long)

#save(importance_long, file = paste(simulation_path, "/setup1_importance.rda", sep =""))



