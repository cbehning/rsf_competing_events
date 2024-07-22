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


# Training the models and model prediction was not performed in R.
#
# To train and predict the random forest, the c++ standalone needs to be
# compiled on the machine.
#
# This document provides some guidance to run the training and the prediction
# using command line tools or bash scripts.

####  Installing the random forest survival (RSF) standalone ####

# Depending on the operating system, the executable file needs to be build differently. 
# The git repository holding the C++ code can be pulled from
# https://github.com/cbehning/ranger/tree/competing_risks_subdist .

# To install the C++ version of ranger in Linux or Mac OS X you will need a compiler
# supporting C++14 (i.e. gcc >= 5 or Clang >= 3.4) and Cmake. 

# To build start a terminal from the ranger main directory and run the following commands

 
# cd cpp_version
# mkdir build
# cd build
# cmake ..
# make
 

# After compilation there should be an executable called "ranger" in the build 
# directory which will be called and needed for running the RSF methods.
# To compile ranger on our cluster, we needed to run the following bash script:
   
# #!/bin/bash
#   
# module load toolchain/foss/2022a
# module load devel/CMake/3.23.1-GCCcore-11.3.0
# module load devel/make/4.3-GCCcore-11.3.0
# module load tools/Ninja/1.1<q>-GCCcore-11.3.0
# 
# cd <Path to ranger/cpp_version>
#   # cleans up previous builds
#   rm -rf build
# 
# # configures project
# cmake -S . -B build -G Ninja
# # actually builds
# cmake --build build
# ls -lah build


#### Running the RSF methods ####
# Bash scripts are provided in the bash script folder and can be used in case slurm 
# is used as a scheduling manager. For running individual configurations of the RSF 
# methods, the following code can be used from the command line (given that ranger 
# is compiled).
 
#### Training ####
# $ranger_exe:  path to compiled ranger executable
# $OUTPUT_FILE: output of training, will generate an output using this prefix, 
#               ending with .forest
# 
### Reference ###
# run:
#   $ranger_exe --file $INPUT_FILE --treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --outprefix $OUTPUT_FILE
# with:
#   $INPUT_FILE: For the reference setting, the dataset with the true generated censoring time must be used. The status variable is either 0 or 1. (data_e1_<set>_<q>_<b>_<seed>_<n>_p_<k>_k.csv)

### Naive ### 
# run:
#   $ranger_exe --file $INPUT_FILE --treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --outprefix $OUTPUT_FILE 
# with:
#   $INPUT_FILE: For the Naive setting, the dataset using the competing event time as cesoring time must be used. The status variable is either 0 or 1.  (data_<set>_ignore_<q>_<b>_<seed>_<n>_p_<k>_k.csv)

###  ImputeNodes ### 
# run:
#   $ranger_exe --file $INPUT_FILE --treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --crimputesubdist --outprefix $OUTPUT_FILE
# with:
#   $INPUT_FILE: For the ImputeNodes and ImputeRoot settings, the dataset using the competing event time and status of 0,1,2 must be used (data_<set>_<q>_<b>_<seed>_<n>_p_<k>_k.csv)
 
###  ImputeRoot ### 
# run:
#   $ranger_exe --file $INPUT_FILE --treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --crimputesubdistonlyinroot --crimputesubdist --outprefix $OUTPUT_FILE
# with:
#   $INPUT_FILE: For the ImputeNodes and ImputeRoot settings, the dataset using the competing event time and status of 0,1,2 must be used (data_<set>_<q>_<b>_<seed>_<n>_p_<k>_k.csv)

###  ImputeOnce ### 
# run:
#   $ranger_exe --file $INPUT_FILE --treetype 5 --depvarname time --statusvarname status --impmeasure 2  --write --seed 20231107 --outprefix $OUTPUT_FILE
# with:
#   $INPUT_FILE: For the ImputeOnce setting, the dataset with readily imputed censoring times for competing event cases must be used. The status variable is either 0 or 1. (```data__oio_____.csv``)
 
#### Prediction on test data ####

# $ranger_exe:  path to compiled ranger executable
# $forest_file: output of training, ending with .forest
# $test_file:   test file corresponding to the training parameter configuration
# $pred_name:   prefix of prediction file

# run:
# $ranger_exe --predict $forest_file --file $test_file --outprefix $pred_name




# To run individual simulation run from command line from R without bash scripts, use the following code

# set path to ranger installation
ranger_exe <- "C:/Users/behning/CLionProjects/ranger/cpp_version/test/cmake-build-relwithdebinfo/ranger_build/ranger.exe"

# to check if calling ranger works from your system try:
system(paste(ranger_exe, "--help"))

prediction_results_path <- paste("tmp_pred_", setup, sep = "") # change from "tmp_pred" to "pred_" if this data should be used in the following scripts
dir.create(prediction_results_path, showWarnings = FALSE)
dir.create(paste(prediction_results_path, "/n_", n, sep = ""), showWarnings = FALSE)
dir.create(paste(prediction_results_path, "/imp2", n, sep = ""), showWarnings = FALSE)

# All parameters were set previously in data_generation.R
for(i in c(seed_start:(seed_start+repeats))){ # single seed for single simulation run
  for(p in p_vector){
    for(b in p_vector){
      ###################
      #### Reference ####
      ###################
      # names for input and output data, use data with true simulated e1 rate as reference
      INPUT_FILE <- paste(path_prefix,"/", i,"/data_e1_train_",p,"_", b,"_",i,"_",n,"_p_",vars,"_k.csv", sep ="")
      OUTPUT_FILE <- paste(prediction_results_path,"/n_",n, "/crsubdist_e1_",gsub(p, pattern = "\\.", replacement = ""),
                        "_",b,"_",i,"_", n, sep ="") 
      # names for forest file
      forest_file <- paste(OUTPUT_FILE, ".forest", sep ="")
      filename_importance <- paste(OUTPUT_FILE,".importance", sep ="")
      # Names for prediction files
      pred_name <- paste(prediction_results_path,"/n_",n, "/test_crsubdist_e1_", gsub(p, pattern = "\\.", replacement = ""),
                        "_",b,"_",i,"_", n, sep ="")
      filename_prediction <- paste(pred_name, ".prediction", sep ="")
      test_file <- paste(path_prefix,"/", i,"/data_test_",p,"_", b,"_",i,"_",n,"_p_",vars,"_k.csv", sep ="")
    
      
      train_call <- paste(ranger_exe, "--file",INPUT_FILE,
                          "--treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --outprefix", OUTPUT_FILE)
      pred_call <- paste(ranger_exe, "--predict ",forest_file," --file ", test_file, "--outprefix", pred_name)
      
      # Train the forest
      system(train_call,  show.output.on.console = TRUE)
      #Predict the forest
      system(pred_call, show.output.on.console = TRUE)
      
      system(paste("rm", forest_file))
      
      ###################
      ####   Naive   ####
      ###################
      # uses data with competing event time as censoring time 
      INPUT_FILE <- paste(path_prefix,"/", i,"/data_train_ignore_",p,"_", b,"_",i,"_",n,"_p_",vars,"_k.csv", sep ="")
      OUTPUT_FILE <- paste(prediction_results_path,"/n_", n, "/crsubdist_ignore_",gsub(p, pattern = "\\.", replacement = ""),
                        "_",b,"_",i,"_", n, sep ="")
      forest_file <- paste(OUTPUT_FILE, ".forest", sep ="")
      filename_importance <- paste(OUTPUT_FILE,".importance", sep ="")
      pred_name <- paste(prediction_results_path,"/n_",n, "/test_crsubdist_ignore_", gsub(p, pattern = "\\.", replacement = ""),
                         "_",b,"_",i,"_", n, sep ="")
      filename_prediction <- paste(pred_name, ".prediction", sep ="")
      # uses same test data as above
      
      train_call <- paste(ranger_exe, "--file", INPUT_FILE,
                          "--treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --outprefix", OUTPUT_FILE)
      pred_call <- paste(ranger_exe, "--predict ",forest_file," --file ", test_file, "--outprefix", pred_name)
      
      # Train the forest
      system(train_call,  show.output.on.console = TRUE)
      #Predict the forest
      system(pred_call, show.output.on.console = TRUE)
      system(paste("rm", forest_file))
      
      #####################
      #### imputeNodes ####
      #####################
      # uses original data but imputes within the tree 
      INPUT_FILE <- paste(path_prefix,"/", i,"/data_train_",p,"_", b,"_",i,"_",n,"_p_",vars,"_k.csv", sep ="")
      OUTPUT_FILE <- paste(prediction_results_path,"/n_", n, "/crsubdist_",gsub(p, pattern = "\\.", replacement = ""),
                           "_",b,"_",i,"_", n, sep ="")
      forest_file <- paste(OUTPUT_FILE, ".forest", sep ="")
      filename_importance <- paste(OUTPUT_FILE,".importance", sep ="")
      pred_name <- paste(prediction_results_path,"/n_",n, "/test_crsubdist_", gsub(p, pattern = "\\.", replacement = ""),
                         "_",b,"_",i,"_", n, sep ="")
      filename_prediction <- paste(pred_name, ".prediction", sep ="")
      # same test data as above

      # build model
      train_call <- paste(ranger_exe, "--file", INPUT_FILE,
                          "--treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --crimputesubdist --outprefix", OUTPUT_FILE)
      pred_call <- paste(ranger_exe, "--predict ",forest_file," --file ", test_file, "--outprefix", pred_name)
      
      # Train the forest
      system(train_call,  show.output.on.console = TRUE)
      #Predict the forest
      system(pred_call, show.output.on.console = TRUE)
      system(paste("rm", forest_file))
      
      ####################
      #### imputeRoot ####
      ####################
      # uses same input file as imputeNodes
      OUTPUT_FILE <- paste(prediction_results_path,"/n_", n, "/crsubdist_oinroot_",gsub(p, pattern = "\\.", replacement = ""),
                           "_",b,"_",i,"_", n, sep ="")
      forest_file <- paste(OUTPUT_FILE, ".forest", sep ="")
      filename_importance <- paste(OUTPUT_FILE,".importance", sep ="")
      pred_name <- paste(prediction_results_path,"/n_",n, "/test_crsubdist_oinroot_", gsub(p, pattern = "\\.", replacement = ""),
                         "_",b,"_",i,"_", n, sep ="")
      filename_prediction <- paste(pred_name, ".prediction", sep ="")
      # same test data as above
     
      # build model
      train_call <- paste(ranger_exe, "--file", INPUT_FILE,
                          "--treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --crimputesubdist --crimputesubdistonlyinroot --outprefix", OUTPUT_FILE)
      pred_call <- paste(ranger_exe, "--predict ",forest_file," --file ", test_file, "--outprefix", pred_name)
      
      # Train the forest
      system(train_call,  show.output.on.console = TRUE)
      #Predict the forest
      system(pred_call, show.output.on.console = TRUE)
      system(paste("rm", forest_file))
      
      
      ####################
      #### imputeOnce ####
      ####################  
      # Uses input data that uses previpusly imputed Censoring times
      INPUT_FILE <- paste(path_prefix,"/", i,"/data_train_oio_",p,"_", b,"_",i,"_",n,"_",vars,"_k.csv", sep ="")
      OUTPUT_FILE <- paste(prediction_results_path,"/n_", n, "/crsubdist_oio_",gsub(p, pattern = "\\.", replacement = ""),
                           "_",b,"_",i,"_", n, sep ="")
      forest_file <- paste(OUTPUT_FILE, ".forest", sep ="")
      filename_importance <- paste(OUTPUT_FILE,".importance", sep ="")
      pred_name <- paste(prediction_results_path,"/n_",n, "/test_crsubdist_oio_", gsub(p, pattern = "\\.", replacement = ""),
                         "_",b,"_",i,"_", n, sep ="")
      filename_prediction <- paste(pred_name, ".prediction", sep ="")
      # same test data as above
      
      # build model
      train_call <- paste(ranger_exe, "--file", INPUT_FILE,
                          "--treetype 5 --depvarname time --statusvarname status --impmeasure 2 --write --seed 20231107 --outprefix", OUTPUT_FILE)
      pred_call <- paste(ranger_exe, "--predict ",forest_file," --file ", test_file, "--outprefix", pred_name)
      
      # Train the forest
      system(train_call,  show.output.on.console = TRUE)
      #Predict the forest
      system(pred_call, show.output.on.console = TRUE)
      system(paste("rm", forest_file))
    }
  }
}





















