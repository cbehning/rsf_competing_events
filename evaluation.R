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


# The following contains R-code to evaluate the simulations and to replicate all
# tables and figures as contained in the result section of the manuscript.


require(tidyverse)

# define setup to evaluate
setup <- "setup1"
# load data
load(paste(setup,"_results_data.rda", sep =  ""))
