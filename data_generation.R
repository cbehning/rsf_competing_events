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


# The following contains R-Code to create the data needed to train the random
# survial forest.
# 
# It will create a folder containing subfolders for each seed with generated for 
# each seed and simulation method.
# imputeRoot and imputeNode will use the same datasets for training.
# Naive, Reference and imputeOnce have seperate dataset for training.


## Input:
# n:          number of subjects (rows of the final dataframe)
# p:          parameter affecting the probability of type 1 events 
# b:          parameter affecting the degree of censoring 
# vars:       number of covariates
# setup:      simulation setup1 or setup2. Coded with setup1: scenario 231121, setup2: scenario 231213
# seed:       seed to start simulation with (paper used 71123 as stating seed)
# repeats:    number of simulation repeats -1



#load required functions
source("functions_data_generating.R")

# chose a scenario:
setup <- "setup1"
scenario <- ifelse(setup == "setup1", "231121",
                   ifelse(setup == "setup2", "231213",
                          "please chose setup1 or setup2"))

# set a seed
seed_start <- 71123
# set number of repeats
repeats <- 1-1 # Warning: if set to 1000 for original analysis will create ~ 20GB of data (~14MB per repeat)

# possible values for parameter affecting e1 rate
p_vector <- c(0.2, 0.4, 0.8)
# possible values for parameter affecting censporing rate
b_vector <- c(0.85, 1, 1.25)
# Number of subjects (rows)
n <- 1000
vars <- 50

# set path to store data
path_prefix <- file.path(paste("data_", setup, sep =""), fsep = .Platform$file.sep)
dir.create(path_prefix, showWarnings = FALSE)

# Create training and test data sets
# create raw data and object lists
# creates data for Referene, imputeNode and imputeRoot
for(p in p_vector){
  for(vars in c(50)){
    limits <- get_limits(p = p, nvar = vars, scenario = scenario )
    
    for(seed in  c(seed_start:(seed_start+repeats))   ) { 
      for(b in b_vector){
        f_path <- paste(path_prefix, seed, sep =  .Platform$file.sep)
        # create subfolders if they don't exist
        dir.create(f_path, showWarnings = FALSE)
        f_path <- paste(f_path, .Platform$file.sep, sep = "")
        
        for(n in c(1000)){
          set.seed(seed)
          print(paste("e1 and raw data -p:", p, "    seed:", seed, "   n:", n, "  b:", b))
          createSampledData(  p = p, b = b, n = n,  seed = seed, path = f_path, limits = limits, vars = vars, scenario = scenario)
        }
      }
    }
  }
}

# Write extra files ignoring the censoring mechanism:
# instead of the true censoring time use the e2 time as censoring time
# Write files were the competing event time is only imputed once
# write only for the training data set
for(set in c("train")){
  for(p in p_vector){
    for(vars in c(50)){
      for(seed in  c(seed_start:(seed_start+repeats))   ) { 
        for(b in b_vector){
          # create path
          f_path <- paste(paste(path_prefix, seed, sep =  .Platform$file.sep),
                          .Platform$file.sep, sep = "")
          for(n in c(1000)){
            # inread training data used for imputeRoot and imputeNode with times for e1, e2 and cens
            tmp <- read.csv(file = paste(f_path,"data_", set,"_", round(p,1),"_", b,"_", seed, "_", n, "_p_", vars,"_k.csv", sep = "" ),
                            header = TRUE, sep = ",")
            ## create data for Naive approach, treating competing event times as censoring
            #inread data
            tmp_ignore <- tmp
            tmp_ignore$status <- (tmp_ignore$status ==1 )*1
            write.table(tmp_ignore, file = paste(f_path,"/data_", set,"_ignore_", round(p,1),"_",b,"_", seed, "_", n, "_p_",vars,"_k.csv", sep = ""),
                        sep = ",", quote = FALSE, col.names = TRUE,row.names = FALSE)
            
            ## create data for imputeOnce
            # preprocess for discSurv: create columns needed for discSurv package
            tmp <- tmp[, c("time", "status", paste("X", 1:vars, sep = ""))]
            tmp$e1 <-  (tmp$status == 1) *1.0
            tmp$e2 <- (tmp$status > 1) *1.0
            
            # impute once, as used in https://github.com/shekoufeh/Deep-Survival-Analysis-With-Competing-Events
            df_sample_syn <- DRSA_createSampledRawOutput21(dataS = tmp, eventCols = c("e1", "e2"), eoi = "e1", timeCol = "time", seed2 = 081123)
            # new status
            df_sample_syn$status <- as.numeric(df_sample_syn[, "e1"] == 1)
            # drop columns
            df_sample_syn[, c("subDistWeights", "v_samplegew", "y", "eventCols", "time", "day")] <- NULL
            # rename
            df_sample_syn <- df_sample_syn %>% rename(time = timeInt) %>% arrange(obj) %>% data.frame()
            # remove e1, e2 abd obj
            df_sample_syn[,c("obj", "e1", "e2")] <- NULL
            
            write.table(df_sample_syn, file = paste(f_path,"/data_", set,"_oio_", round(p,1),"_",b,"_", seed, "_", n,"_", vars, "_k.csv", sep = ""),
                        sep = ",", quote = FALSE, col.names = TRUE,row.names = FALSE)
            
          }
        }
      }
    }
  }
}


