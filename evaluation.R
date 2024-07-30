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

## Input:
# n:          number of subjects (rows of the final dataframe)
# p:          parameter affecting the probability of type 1 events 
# b:          parameter affecting the degree of censoring 
# vars:       number of covariates
# setup:      simulation setup1 or setup2. Coded with setup1: scenario 231121, setup2: scenario 231213
# seed:       seed to start simulation with (paper used 71123 as stating seed)
# repeats:    number of simulation repeats -1

require(tidyverse)

# define setup to evaluate
setup <- "setup1"

# set a seed
seed_start <- 71123
# set number of repeats
repeats <- 1000-1 
# number of subjects evaluated
n <- 1000

## possible values for parameter affecting e1 rate
#p_vector <- c(0.2, 0.4, 0.8)
## possible values for parameter affecting censporing rate
# b_vector <- c(0.85, 1, 1.25)



# load data of simulation evaluation
load(paste(setup,"_results_data.rda", sep =  ""))
#load status frequencies of individual simuation runs
load(paste(setup,"_status_freq.rda", sep =  ""))

## define aesthetics for plotting
type_colors <- c( "#c4515c", "grey", "gold" , "steelblue",  "black" )
names(type_colors) <- c("imputeNode", "Naive approach", "imputeOnce",  "imputeRoot",  "Reference"  )
type_linetype <- c("solid","solid", "solid", "solid", "11" )
names(type_linetype) <- c("Naive approach", "imputeOnce",  "imputeRoot","imputeNode",  "Reference"  )

## second set of colors for plots, where gold is barely visible
type_colors2 <- c( "#c4515c", "grey", "sienna2" , "steelblue",  "grey20" )
names(type_colors2) <- c("imputeNode",  "Naive approach", "imputeOnce", "imputeRoot", "Reference" )

# create path for output
output_prefix <- file.path("output", fsep = .Platform$file.sep)
dir.create(output_prefix, showWarnings = FALSE)
output_prefix <- paste("output",.Platform$file.sep, sep = "")



#### Figure: Calibration graph of the CIF ####
## Creates Figure 2 or 3 of the manuscript, depending on the choice of setup
all_data1000 %>%
  mutate(type = factor(type, levels = c( "Naive approach", "imputeOnce",  "imputeRoot","imputeNode",  "Reference"  ))) %>%
  mutate(n = paste("n = ", n )) %>% 
  rename(q = p) %>% 
  ggplot(aes(x = time, y = mean_cif, color = type))+
  geom_step(aes(linetype = type), linewidth = 1.00,  alpha = 0.9)+
  facet_grid(b~q, scale = "free_y", labeller = "label_both")+
  ylab("Mean CIF")+
  scale_color_manual(values = type_colors, name = NULL)+
  scale_linetype_manual(values = type_linetype, name = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 18))

if(setup == "setup1"){
  ggsave(filename = paste(output_prefix, "Figure_2.png", sep = ""),
         device = "png", width = 10, height = 7)
  
}else if(setup == "setup2"){
  ggsave(filename = paste(output_prefix, "Figure_3.png", sep = ""),
         device = "png", width = 10, height = 7)
  
}



### Supplementary Figures and Tables

#### Figure: relative frequency event and censoring times ####
## Creates Figure S1 or S2 of the manuscript, depending on the choice of setup

status_raw_data_df %>%
  rename(q = p) %>% 
  group_by(q, b, Var1) %>%
  summarise(Freq_mean = mean(Freq/1000),
            Freq_min = min(Freq/1000),
            Freq_max = max(Freq/1000)) %>%
  ggplot(aes(x = Var1, y = Freq_mean))+
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5, alpha = 0.8) +
  geom_errorbar(aes(ymin = Freq_min, ymax = Freq_max),
                width = 0.2, position = position_dodge(width = 0.5),
                color = "grey20") +
  facet_grid(b~q, labeller = "label_both")+
  ylab("Relative frequqency")+
  xlab("status")+
  theme_bw()+
  theme( text = element_text(size = 18))

if(setup == "setup1"){
  ggsave(filename = paste(output_prefix,"Figure_S1.png", sep =""),
         device = "png", width = 10, height = 7)
  
}else if(setup == "setup2"){
  ggsave(filename = paste(output_prefix, "Figure_S2.png", sep =""),
         device = "png", width = 10, height = 7)
  
}



#### Figure: Estimated CIF with Reference limits ####
## Creates Figure S6 or S7 of the manuscript, depending on the choice of setup

# calculate width of reference bands for 1000 repeats
rep <- repeats+1
all_data1000_2 <- all_data1000 %>%
  mutate(lower = mean_cif - 1.96 * sqrt(mean_cif * (1-mean_cif)/rep),
         upper = mean_cif + 1.96 * sqrt(mean_cif * (1-mean_cif)/rep),)

# plot width of reference bands for n =1000
all_data1000_2 %>%
  filter(time %in% c(10, 15, 20)) %>%
  mutate(type = factor(type, levels = c( "Naive approach", "imputeOnce",  "imputeRoot","imputeNode",  "Reference"  )),
         time = factor(time, levels = c(10,15,20))) %>%
  #filter(time <= 19) %>%
  mutate(n = paste("n = ", n )) %>% 
  rename(q = p) %>% 
  ggplot(aes(x = time, y = mean_cif, color = type))+
  geom_point(  alpha = 0.9, position = position_dodge(width = 0.5))+
  geom_errorbar(aes( ymin=lower, ymax=upper), width=0.2, linewidth=1,
                position = position_dodge(width = 0.5)) + 
  facet_grid(b~q, scale = "free_y", labeller = "label_both")+
  ylab("Mean CIF")+
  scale_color_manual(values = type_colors, name = NULL)+
  scale_linetype_manual(values = type_linetype, name = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 18))

if(setup == "setup1"){
  ggsave(filename = paste(output_prefix, "Figure_S6.png", sep = ""), 
         device = "png", width = 10, height = 7)
  
}else if(setup == "setup2"){
  ggsave(filename = paste(output_prefix, "Figure_S7.png", sep = ""), 
         device = "png", width = 10, height = 7)
  
}




#### Figure: Illustration of comparison of C_i and \hat{C}_i ####
# Creates Figure S3, only created for setup1
if(setup == "setup1"){
  # WARNING: Can only be computed if data was generated first using data_generation.R
  # with respective, n, k=vars, and seed values
  if(! exists('data_setup1/71123/data_train_0.2_0.85_71123_1000_p_50_k.csv')){
    print("Figure S3, S4, S5 can only be run, if the training data was generated first.")
    print("Will run data_generation.R now")
    source("data_generation.R")
  }
  # initialize values for loading
  n <- 1000
  k <- 50
  seed <- 71123
  C_times_df <- data.frame(type= NA,
                           time = NA,
                           ce_rows = NA,
                           p = NA,
                           b = NA,
                           n_e2 = NA)
  
  for(p in c(2,4,8)){
    for(b in c(0.85, 1, 1.25)){
      # inread training data
      train_data <- read.csv(paste("data_", setup,.Platform$file.sep, seed,.Platform$file.sep,
                                   "data_train_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k.csv",  sep = ""))[, c("time", "status")]
      # define competing event row identifier
      ce_rows <- which(train_data$status ==2)
      n_ce <- length(ce_rows)
      # read all reference censoring times where competing events happen
      train_data_e1 <- read.csv(paste("data_", setup,.Platform$file.sep, seed,.Platform$file.sep,
                                      "data_e1_train_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k.csv",  sep = ""))[ce_rows, c("time", "status")]
      # read all imputed once censoring times
      train_data_oio <- read.csv(paste("data_", setup,.Platform$file.sep, seed,.Platform$file.sep,
                                       "data_train_oio_0.",p,"_",b,"_",seed,"_",n,"_",k , "_k.csv",  sep = ""))[ce_rows, c("time", "status")]
      train_data_naive <- read.csv(paste("data_", setup,.Platform$file.sep, seed,.Platform$file.sep,
                                         "data_train_ignore_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k.csv",  sep = ""))[ce_rows, c("time", "status")]
      
      C_times_df_tmp <- data.frame(type= c(rep("Reference",n_ce ),
                                           rep("imputeOnce", n_ce),
                                           rep("Naive approach", n_ce)),
                                   time = c(train_data_e1$time, train_data_oio$time, train_data_naive$time),
                                   ce_rows= c(ce_rows, ce_rows, ce_rows),
                                   p = rep(p, 3*n_ce),
                                   b = rep(b, 3 *n_ce),
                                   n_e2 =rep(n_ce, 3 *n_ce) )
      # append new rows
      C_times_df <- rbind(C_times_df, C_times_df_tmp)
    }
  }
  # remove empty rows
  C_times_df <- C_times_df %>%
    filter(!is.na(type))
  
  
  C_times_df %>%
    mutate(type = factor(type, levels = c(  "Reference", "imputeOnce", "Naive approach"  )),
    ) %>%
    rename(q = p) %>% 
    ggplot(aes(x = time,  fill = type))+
    geom_histogram(  alpha=0.5, position="identity", binwidth = 1)+
    facet_grid(b~q, scale = "free_y", labeller = "label_both")+
    ylab("Count")+
    xlab("Censoring times")+
    scale_fill_manual(values = type_colors, name = NULL)+
    theme_bw()+
    theme(legend.position = "bottom",
          text = element_text(size = 18))
  
  ggsave(filename = paste(output_prefix, "Figure_S3.png", sep = ""),
         device = "png", width = 10, height = 7)
  
}


#### Figure: Estimate of G for imputeRoot and imputeNode #####
# Creates figures S4 and S5
# For one simulation setting (q=0.2, b= 0.85, Steup1), we can show the distribution of 
# the estimated censoring suvival functions.
# The output has been flushed from the ranger console output.
# Compile branch https://github.com/cbehning/ranger/tree/competing_risks_subdist_retrieve-cens-times
# to create the output files. (Code is a quick-and-dirty workaround, as parts are send to err)

if(setup == "setup1" ){
  n <- 1000
  k <- 50
  seed <- 71123
  p <- 2
  b <- 0.85
  
  # inread data that was flushed as output from one simulation run of ranger
  # all treeIDs, nodeIDS, censoring times (imputed and true)
  trees_nodes <- read.table( paste("pred_setup1",.Platform$file.sep ,"cens_times_imputeNodes_0.",p,"_",b,"_",seed,"_",n,"_p_",k,"_k.log" ,sep = ""),
                             sep = ",",skip = 1,header = FALSE)
  names(trees_nodes) <- c("treeID", "nodeID", "time", "sampleID")
  
  trees_root <- read.table( paste("pred_setup1",.Platform$file.sep ,"cens_times_imputeRoot_0.",p,"_",b,"_",seed,"_",n,"_p_",k,"_k.log" , sep = ""),
                            sep = ",", skip = 1,header = FALSE)
  names(trees_root) <- c("treeID", "nodeID", "time", "sampleID")
  
  train_data <- read.csv(paste("data_", setup,.Platform$file.sep, seed,.Platform$file.sep, 
                               "data_train_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k.csv",  sep = ""))#[, c("time", "status")]
  ce_rows <- which(train_data$status ==2)
  n_ce <- length(ce_rows)
  # read all reference censoring times where competing events happen
  train_data_e1 <- read.csv(paste("data_", setup,.Platform$file.sep, seed,.Platform$file.sep,
                                  "data_e1_train_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k.csv",  sep = ""))[ce_rows, c("time", "status")]
  # read all times and status
  train_data_e1_all <- read.csv(paste("data_", setup,.Platform$file.sep, seed,.Platform$file.sep,
                                      "data_e1_train_0.",p,"_",b,"_",seed,"_",n,"_p_",k , "_k.csv",  sep = ""))[, c("time", "status")] %>% mutate(sampleID = (1:n())-1)
  
  train_data_oio <- read.csv(paste("data_", setup,.Platform$file.sep, seed,.Platform$file.sep,
                                   "data_train_oio_0.",p,"_",b,"_",seed,"_",n,"_",k , "_k.csv",  sep = ""))[, c("time", "status")] %>% mutate(sampleID = (1:n())-1)
  
  mse_oio <- train_data_oio[ce_rows, ] %>% 
    left_join(train_data_e1_all %>% select(sampleID, time), by = "sampleID", suffix = c("_oio", "_e1")) %>%
    mutate(diff2 = (time_oio - time_e1)^2 )
  
  print("MSE between imputeOnce:")
  print(1/n_distinct(mse_oio$sampleID) * sum(mse_oio$diff2))
  
  # which have been censored and which have had a competing event?
  trees_root2 <- trees_root %>%
    mutate(ce = sampleID %in% ce_rows) %>% 
    left_join(train_data_e1_all %>% select(sampleID, time), by = "sampleID", suffix = c("_ranger", "_e1")) %>%
    left_join(train_data %>% mutate(sampleID = (1:n())-1 ) %>% select(sampleID, status, time), by = "sampleID", suffix = c("_Reference", "_train"))
  # Test 1: time of ranger is the same for event 1
  (trees_root2 %>% filter(status ==1 & time_ranger != time) %>% nrow())==0
  # Test 2: time of ranger is the same for censored
  (trees_root2 %>% filter(status ==0 & time_ranger != time) %>% nrow())==0
  # Test 3: time of ranger differs at least fo some for comepting event
  (trees_root2 %>% filter(status ==2 & time_ranger != time) %>% nrow())!=0
  
  trees_root_ce <- trees_root2 %>% filter(status == 2) %>% mutate(diff2 = (time_ranger - time_e1)^2 )
  
  mse_root <- trees_root_ce %>%
    group_by(treeID, nodeID, sampleID) %>%
    summarise(MSE = mean(diff2)) %>%
    group_by(sampleID, treeID)%>%
    summarise(n_node = n(),
              MSE = 1/n_node * sum(MSE)) %>%
    group_by(sampleID)%>%
    summarise(n_tree = n(),
              MSE = 1/n_tree * sum(MSE)) 
  print("MSE between imputeRoot:")
  print(1/n_distinct(mse_root$sampleID) * sum(mse_root$MSE))
  
  # which have been censored and which have had a competing event?
  trees_nodes2 <- trees_nodes %>%
    mutate(ce = sampleID %in% ce_rows) %>% 
    left_join(train_data_e1_all %>% select(sampleID, time), by = "sampleID", suffix = c("_ranger", "_e1")) %>%
    left_join(train_data %>% mutate(sampleID = (1:n())-1 ) %>% select(sampleID, status, time), by = "sampleID", suffix = c("_Reference", "_train"))
  # Test 1: time of ranger is the same for event 1
  (trees_nodes2 %>% filter(status ==1 & time_ranger != time) %>% nrow())==0
  # Test 2: time of ranger is the same for censored
  (trees_nodes2 %>% filter(status ==0 & time_ranger != time) %>% nrow())==0
  # Test 3: time of ranger differs at least fo some for comepting event
  (trees_nodes2%>% filter(status ==2 & time_ranger != time) %>% nrow())!=0
  
  trees_nodes_ce <- trees_nodes2 %>% filter(status == 2) %>% mutate(diff2 = (time_ranger - time_e1)^2 )
  
  mse_nodes <- trees_nodes_ce %>%
    group_by(treeID, nodeID, sampleID) %>%
    summarise(MSE = mean(diff2)) %>%
    group_by(sampleID, treeID)%>%
    summarise(n_node = n(),
              MSE = 1/n_node * sum(MSE)) %>%
    group_by(sampleID)%>%
    summarise(n_tree = n(),
              MSE = 1/n_tree * sum(MSE)) 
  
  print("MSE between imputeNodes:")
  print(1/n_distinct(mse_nodes$sampleID) * sum(mse_nodes$MSE))
  
  
  ### inread censoring times g_hat
  ghat_nodes <- read.table( paste("pred_setup1",.Platform$file.sep ,"g_hat_imputeNodes_0.",p,"_",b,"_",seed,"_",n,"_p_",k,"_k.log" , sep = ""),
                            sep = ",",header = FALSE)
  names(ghat_nodes ) <- c("treeID", "nodeID", 1:20)
  
  ghat_root <- read.table( paste("pred_setup1",.Platform$file.sep ,"g_hat_imputeRoot_0.",p,"_",b,"_",seed,"_",n,"_p_",k,"_k.log" , sep = ""),
                           sep = ",",header = FALSE)
  names(ghat_root) <- c("treeID", "nodeID",  1:20)
  # Test if all rows have equal values
  (ghat_root[, -2] %>% n_distinct()) == (ghat_root[, 1] %>% n_distinct())
  # remove duplicated entries
  ghat_root <- ghat_root %>% filter(nodeID==0)
  
  # compute Ghat on whole training subset
  G_hat_train_data <- data.frame(time = 1:20,
                                 G_hat = discSurv::estSurvCens(dataShort = train_data %>% mutate(cens = status ==0, e1 = status ==1, e2 = status ==2),
                                                               timeColumn = "time",
                                                               eventColumns = c("e1", "e2")),
                                 treeID = "Reference"
  )
  # plot ghat in all root nodes (trees)
  ghat_root %>%
    select(-nodeID) %>%
    pivot_longer(-treeID, names_to = "time", values_to = "G_hat") %>% 
    mutate(across(c(time, G_hat), as.numeric )) %>%
    ggplot(aes( x = time, y = G_hat, group =treeID)) +
    geom_step(color = "grey", alpha = 0.5) +
    geom_step(data = G_hat_train_data,color = "black", alpha = 1, linewidth = 1.2) +
    #geom_step( direction="vh", linetype=3) +
    #scale_x_continuous( expand = c(0, 0)) +
    #scale_y_continuous( expand = c(0, 1)) +
    ylab("Estimate of G(T=t)")+
    xlab("Time")+
    theme_bw()
  
  ggsave(filename = paste(output_prefix, "Figure_S4.png", sep =""), device = "png", width = 6, height = 5)
  
  ghat_nodes_long <- ghat_nodes %>%
    mutate(treeID = paste(treeID, nodeID) ) %>%
    select(-nodeID) %>%
    pivot_longer(-treeID, names_to = "time", values_to = "G_hat") %>% 
    mutate(across(c(time, G_hat), as.numeric )) 
  
  # plot ghat in all  nodes (trees)
  ghat_nodes_long %>%
    ggplot(aes( x = time, y = G_hat, group =treeID)) +
    geom_step(color = "grey", alpha = 0.5) +
    geom_step(data = G_hat_train_data,color = "black", alpha = 1, linewidth = 1.2) +
    #geom_step( direction="vh", linetype=3) +
    #scale_x_continuous( expand = c(0, 0)) +
    #scale_y_continuous( expand = c(0, 1)) +
    ylab("Estimate of G(T=t)")+
    xlab("Time")+
    theme_bw()
  
  ggsave(filename = paste(output_prefix, "Figure_S5.png", sep =""), device = "png", width = 6, height = 5)
}


#### Table: C-Index ####
require(kableExtra)

if(setup == "setup1"){
  caption_title <- "Table S1: Mean (SD) C-index values computed via cIndex in the discSurv Package."
}else if(setup == "setup2"){
  caption_title <- "Table S2: Mean (SD) C-index values computed via cIndex in the discSurv Package."
}

table_12 <- cindex_df %>%
  left_join(mapping_method_type, by = "method") %>%
  mutate(type = factor(type, levels = c("imputeNode", "imputeRoot", "imputeOnce", "Naive approach", "Reference"))) %>% 
  group_by(type, p, b) %>%
  rename(q = p) %>% 
  mutate(q = paste("0.", q, sep = "")) %>%
  summarise(mean_c_index = paste( sprintf(mean(cindex, na.rm = TRUE),fmt = "%.4f"), " (", sprintf(sd(cindex, na.rm = TRUE),fmt = "%.3f"), ")", sep = "" )) %>%
  pivot_wider(names_from = type, values_from = mean_c_index)

table_12  %>%
  kableExtra::kable(caption = caption_title,
                    digits = 4) %>% # add format = "latex"
  kableExtra::kable_styling(latex_options = c("striped"), full_width = FALSE) 

if(setup == "setup1"){
  write.csv(table_12, file = paste(output_prefix, "Table_S1.csv", sep = ""),
            row.names = FALSE)
}else if(setup == "setup2"){
  write.csv(table_12, file = paste(output_prefix, "Table_S2.csv", sep = ""),
            row.names = FALSE)
}



#### Figure: Monte Carlo Error Mean C-Index on 10 batches ####
# compute mean c-index for every 100 simulation runs
# compute  standard deviation of resulting 10 means

mc_cindex_100_10 <- cindex_df %>% 
  mutate(mcrep = (seed-seed_start) %% 10 ) %>% # form groups
  left_join(mapping_method_type, by = "method") %>%
  mutate(type = factor(type, levels = c("imputeNode", "imputeRoot", "imputeOnce", "Naive approach", "Reference"))) %>% 
  rename(q = p) %>% 
  mutate(q = paste("0.", q, sep = "")) %>%
  group_by(type, q, b, mcrep) %>%
  summarise(mean_c_index_100 = mean(cindex, na.rm = TRUE)) %>%
  group_by(type, q, b) %>%
  summarise(mean_c_index = mean(mean_c_index_100),
            n = n_distinct(mcrep),
            sd_mean = sd(mean_c_index_100),
            MC_mean = sd_mean/sqrt(n)) 


# Plot Mean of C-Index + MC Error
mc_cindex_100_10 %>%
  ggplot(aes(x = type, y = mean_c_index, color = type))+
  geom_point( position = position_dodge(width = 0.5))+
  geom_errorbar(aes( ymin=mean_c_index-MC_mean, ymax =mean_c_index+MC_mean), width=0.2, linewidth=1, 
                position = position_dodge(width = 0.5)) + 
  facet_grid(b~q,  labeller = "label_both")+
  ylab("Mean C-Index with MC Error")+
  xlab("") +
  scale_color_manual(values = type_colors, name = NULL)+
  scale_linetype_manual(values = type_linetype, name = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


if(setup == "setup1"){
  ggsave(filename = paste(output_prefix, "Figure_S8.png", sep = ""),
         device = "png", width = 10, height = 7)  
}else if(setup == "setup2"){
  ggsave(filename = paste(output_prefix,"Figure_S10.png", sep = ""),
         device = "png", width = 10, height = 7)  
}



#### Figure: Monte Carlo Error SD C-Index on 10 batches ####
# compute sd of c-index for every 100 simulation runs
# compute mean of 10 computed sds
# compute  standard deviation of resulting 10 means

mc_cindex_sd_100_10 <- cindex_df %>% 
  mutate(mcrep = (seed - seed_start) %% 10 ) %>% # form groups
  left_join(mapping_method_type, by = "method") %>%
  mutate(type = factor(type, levels = c("imputeNode", "imputeRoot", "imputeOnce", "Naive approach", "Reference"))) %>% 
  rename(q = p) %>% 
  mutate(q = paste("0.", q, sep = "")) %>%
  group_by(type, q, b, mcrep) %>%
  summarise(sd_c_index_100 = sd(cindex, na.rm = TRUE)) %>%
  group_by(type, q, b) %>%
  summarise(mean_sd_c_index = mean(sd_c_index_100), # pooled sd of 10 repeats of size 100
            n = n_distinct(mcrep),
            sd_sd = sd(sd_c_index_100),
            MC_sd = sd_sd/sqrt(n)) 


# Plot SD of C-Index + MC Error
mc_cindex_sd_100_10 %>%
  ggplot(aes(x = type, y = mean_sd_c_index, color = type))+
  geom_point( position = position_dodge(width = 0.5))+
  geom_errorbar(aes( ymin=mean_sd_c_index-MC_sd, ymax =mean_sd_c_index+MC_sd), width=0.2, linewidth=1, 
                position = position_dodge(width = 0.5)) + 
  facet_grid(b~q,  labeller = "label_both")+
  ylab("SD of C-Index with MC Error")+
  xlab("") +
  scale_color_manual(values = type_colors, name = NULL)+
  scale_linetype_manual(values = type_linetype, name = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

if(setup == "setup1"){
  ggsave(filename = paste(output_prefix, "Figure_S9.png", sep = ""),
         device = "png", width = 10, height = 7)  
}else if(setup == "setup2"){
  ggsave(filename = paste(output_prefix,"Figure_S11.png", sep = ""),
         device = "png", width = 10, height = 7)  
}



#### Table: Integrated Brier Score ####

if(setup == "setup1"){
  caption_title <- "Table S3: Mean (SD) integrated Brier score (IBS)  calculated via the pec package."
}else if(setup == "setup2"){
  caption_title <- "Table S4:Mean (SD) integrated Brier score (IBS)  calculated via the pec package."
}


table_s34 <- bierscore_df %>%
  left_join(mapping_method_type, by = "method") %>%
  mutate(type = factor(type, levels = c("imputeNode", "imputeRoot", "imputeOnce", "Naive approach", "Reference"))) %>% 
  group_by(type, p, b) %>%
  rename(q = p) %>% 
  mutate(q = paste("0.", q, sep = "")) %>%
  summarise(mean_c_index = paste( sprintf(mean(IBS, na.rm = TRUE),fmt = "%.4f"), " (", sprintf(sd(IBS, na.rm = TRUE),fmt = "%.4f"), ")", sep = "" )) %>%
  pivot_wider(names_from = type, values_from = mean_c_index) 

table_s34 %>%
  kableExtra::kable(caption = caption_title,
                    digits = 4) %>% # add  format = "latex"
  kableExtra::kable_styling(latex_options = c("striped"), full_width = FALSE) 

if(setup == "setup1"){
  write.csv(table_s34, file = paste(output_prefix, "Table_S3.csv", sep = ""),
            row.names = FALSE)
}else if(setup == "setup2"){
  write.csv(table_s34, file = paste(output_prefix, "Table_S4.csv", sep = ""),
            row.names = FALSE)
}


#### Figure: Monte Carlo Error Mean IBS on 10 batches#####
# compute mean IBS for every 100 simulation runs
# compute  standard deviation of resulting 10 means
mc_bs_100_10 <- bierscore_df %>% 
  mutate(mcrep = (seed - seed_start) %% 10 ) %>% # form groups
  left_join(mapping_method_type, by = "method") %>%
  mutate(type = factor(type, levels = c("imputeNode", "imputeRoot", "imputeOnce", "Naive approach", "Reference"))) %>% 
  rename(q = p) %>% 
  mutate(q = paste("0.", q, sep = "")) %>%
  group_by(type, q, b, mcrep) %>%
  summarise(mean_IBS_100 = mean(IBS, na.rm = TRUE)) %>%
  group_by(type, q, b) %>%
  summarise(mean_IBS = mean(mean_IBS_100),
            n = n_distinct(mcrep),
            sd_mean = sd(mean_IBS_100),
            MC_mean = sd_mean/sqrt(n)) 


# Plot Mean of C-Index + MC Error
mc_bs_100_10 %>%
  ggplot(aes(x = type, y = mean_IBS, color = type))+
  geom_point( position = position_dodge(width = 0.5))+
  geom_errorbar(aes( ymin=mean_IBS-MC_mean, ymax =mean_IBS+MC_mean), width=0.2, linewidth=1, 
                position = position_dodge(width = 0.5)) + 
  facet_grid(b~q,  labeller = "label_both")+
  ylab("Mean IBS with MC Error")+
  xlab("") +
  scale_color_manual(values = type_colors, name = NULL)+
  scale_linetype_manual(values = type_linetype, name = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


if(setup == "setup1"){
  ggsave(filename = paste(output_prefix, "Figure_S12.png", sep = ""), device = "png", width = 10, height = 7)  
}else if(setup == "setup2"){
  ggsave(filename = paste(output_prefix, "Figure_S14.png", sep = ""), device = "png", width = 10, height = 7)  
}




#### Figure: Monte Carlo Error SD IBS on 10 batches ####
# compute sd of IBS for every 100 simulation runs
# compute mean of 10 computed sds
# compute  standard deviation of resulting 10 means

mc_bs_sd_100_10 <- bierscore_df %>% 
  mutate(mcrep = (seed - seed_start) %% 10 ) %>% # form groups
  left_join(mapping_method_type, by = "method") %>%
  mutate(type = factor(type, levels = c("imputeNode", "imputeRoot", "imputeOnce", "Naive approach", "Reference"))) %>% 
  rename(q = p) %>% 
  mutate(q = paste("0.", q, sep = "")) %>%
  group_by(type, q, b, mcrep) %>%
  summarise(sd_IBS_100 = sd(IBS, na.rm = TRUE)) %>%
  group_by(type, q, b) %>%
  summarise(mean_sd_IBS = mean(sd_IBS_100), # pooled sd of 10 repeats of size 100
            n = n_distinct(mcrep),
            sd_sd = sd(sd_IBS_100),
            MC_sd = sd_sd/sqrt(n)) 


# Plot SD of C-Index + MC Error
mc_bs_sd_100_10 %>%
  ggplot(aes(x = type, y = mean_sd_IBS, color = type))+
  geom_point( position = position_dodge(width = 0.5))+
  geom_errorbar(aes( ymin=mean_sd_IBS-MC_sd, ymax =mean_sd_IBS+MC_sd), width=0.2, linewidth=1, 
                position = position_dodge(width = 0.5)) + 
  facet_grid(b~q,  labeller = "label_both")+
  ylab("SD of IBS with MC Error")+
  xlab("") +
  scale_color_manual(values = type_colors, name = NULL)+
  scale_linetype_manual(values = type_linetype, name = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 18),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

if(setup == "setup1"){
  ggsave(filename = paste(output_prefix, "Figure_S13.png", sep = ""), device = "png", width = 10, height = 7)  
}else if(setup == "setup2"){
  ggsave(filename = paste(output_prefix, "Figure_S15.png", sep = ""), device = "png", width = 10, height = 7)  
}


#### Figures: Variable Importance #####
# Plot 10 most important variables
# Creates Figures S16-S20 or S21-S25, depending on the setup 

load(paste(setup,"_importance.rda", sep=""))
approach_names <- c("Reference", "Naive approach", "imputeOnce", "imputeRoot", "imputeNode")

for(type_ in names(type_colors2)){
  p <- importance_long %>% ungroup() %>%
    rename(q = p) %>%
    filter(type %in% type_) %>% 
    group_by(column, type, q, b) %>% 
    summarise(VIMP = mean(VIMP,na.rm = TRUE)) %>%
    group_by(q,b) %>% 
    arrange(desc(VIMP)) %>% 
    slice(1:10) %>%
    mutate( column_q_b = paste0(column, "_",q,b),
            column_q_b = factor(column_q_b) %>% fct_reorder(desc(VIMP)) ) %>%
    data.frame() %>% 
    ggplot(aes(x = column_q_b, y = VIMP, fill = type))+
    geom_bar(stat = "identity", position = "dodge")+
    scale_fill_manual(values = type_colors2, name = NULL)+
    scale_x_discrete(name=NULL, labels=function(x) gsub('_(.*)$', '', x)) +
    ylab("Mean permutation VIMP")+
    xlab("Variables selected")+
    facet_wrap(b~q, scale = "free", labeller = "label_both")+
    theme_bw()+
    theme(legend.position = "none", text = element_text(size = 11))
  
  figno <- 15 + which(approach_names == type_) + (setup=="setup2")*5
  
  ggsave(p, filename = paste(output_prefix, "Figure_S", figno ,".png", sep =""), device = "png", width = 10, height = 7)
}
rm(figno)

#### Figures: Application on GCKD data #####
# Original GCKD data cannot be provided.
# Nevertheless, the summary statistics used to create the graphs can be loaded.
# To perform analyses on synthetic data of a subset of the gckd data
# resulting in slightly differing results, please see application.R

# load summary data
untar("gckd_orignal_summary.tar.gz")
load("gckd_orignal_summary.rda")

# Figure 4: CIF and 10 most important variables

# Plot 4a: CIF
p <- gckd_cif %>%
  mutate(type = ifelse(type %in% "Naive", "Naive approach", type )) %>%
  mutate(time = as.numeric(time) ) %>% 
  ggplot(aes(x = time, y =  mean_cif, color = type)) +
  geom_step( linewidth = 1.00,  alpha = 0.9) +
  ylab("CIF")+
  xlim(c(0,7))+
  scale_color_manual(values = type_colors, name = NULL)+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size = 11))

p_4a <- p + theme(legend.position = c(0.2, 0.85))

# create subset for 10 most important vars
tmp7 <- 
  gckd_imp %>%
  mutate(column = column_pretty) %>% 
  group_by(column, type) %>% 
  summarise(VIMP = mean(VIMP,na.rm = TRUE)) %>%
  group_by(type) %>% 
  arrange(desc(VIMP)) %>% 
  slice(1:10) %>%
  ungroup() %>% 
  mutate( column_o = factor(paste0(column, "__", type)) %>%  fct_reorder(VIMP) ) %>%
  data.frame() 

p <- gckd_imp %>%
  mutate(column = column_pretty) %>% 
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

# prettier appearance
p_4b <- p+ scale_y_continuous(n.breaks = 3, expand = c(0, 0))+
  shadowtext::geom_shadowtext(
    data = subset(tmp7, VIMP < 0.0025) %>% 
      mutate(labels = gsub('__(.*)$', '', column_o)),
    aes(y = VIMP, x = column_o, label = labels),
    hjust = 0,
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

# combine (A) an (B) plot
ggsave(cowplot::plot_grid(p_4a, p_4b, labels = c('A', 'B'))
       , filename = paste(output_prefix, "Figure_4.png", sep =""), device = "png", width = 10, height = 7)

#### Table: 10-fold imputation on GCKD ####
# Create Table S11: Estimated CIF on multiple imputation runs using impute once
pooled_10 %>%
  kableExtra::kable(caption = "Table S11: Pooled CIF of 10 imputations",
                    digits = 4) %>% # add , format = "latex" to get latex table
  kableExtra::kable_styling(latex_options = c("striped"), full_width = FALSE)

write.csv(pooled_10, file = paste(output_prefix, "Table_S11.csv", sep = ""),
          row.names = FALSE)



#### Table: GCKD VIMP of all covariates ####
# Create Table S12: variable importance of all 38 covariates
ts12 <- gckd_imp %>%
  mutate(column = column_pretty) %>% 
  group_by(column, type) %>% 
  summarise(VIMP = mean(VIMP,na.rm = TRUE)) %>%
  group_by(type) %>% 
  arrange(desc(VIMP)) 

ts12 %>% 
  pivot_wider(names_from = type, values_from = VIMP) %>%
  kableExtra::kable(caption = "Table S12: Variable importance computed via local (casewise) imputation (ranger impmeasure = 6)",
                    digits = 6) %>% # add , format = "latex"
  kableExtra::kable_styling(latex_options = c("striped"), full_width = FALSE) 

write.csv(ts12, file = paste(output_prefix, "Table_S12.csv", sep = ""),
          row.names = FALSE)


################
## MOCK DATA ###
################


#### Table: GCKD Characteristics on synthetic data ####

# WARNING: Please not that the tables below are only provided on mock data. 
# Therefore, the resulting tables will not perfectly align with the published results
# on the original GCKD data.

# load synthetic data
load("syn_gckd.rda")

# recode into meaningful names
syn_gckd_recoded <- syn_gckd %>%
  mutate(across(c(status, incl_criteria, fuehrende_nierenerkr, gender, employment,
                  dem_famstand, dem_beruf, dem_privat, alcohol, smoking,
                  chd, stroke, diab_neph, 
                  aa_schlag2, aa_schmerzmittel, aa_copd, aa_asthma, aa_nierenerk_1,hyp_neph, hypertension, pr_glom_path, systemic,
                  p_renal, int_neph, acute_fail, single_kidney, hereditary, other, unknown ), 
                ~as.factor(.))) %>%
  # manually rename to match manuscript output
  mutate( status = fct_recode(status,  "Censored" = "0", "KF"= "1", "Death"="2"),
          Sex = fct_recode(gender, "male" = "0", "female" = "1"),
          alcohol = fct_recode(alcohol, "low-normal drinking" = "0",  "heavy drinking"= "1"),
          smoking = fct_recode(smoking, "non smokers"="0", "former smokers"="1", "smokers"="2"),
          family_status = fct_recode(dem_famstand, 
                                     "single" = "1", "married or in a stable partnership" = "2", "separated or divorced" = "3",
                                     "widowed" = "4"),
          employment = fct_recode(employment, 
                                  "fully employed" = "1", "part-time" = "2", "housework" = "3", "pension" = "4", 
                                  "job-seeker" = "5", "training" = "6"), #,  "other" = "7"),
          private_insurance = fct_recode(dem_privat, "no" = "0", "yes" = "1"),
          professional_qualification = fct_recode(dem_beruf, 
                                                  "still in training" = "1", "apprenticeship" = "2", "master (craftsperson)" = "3", 
                                                  "university degree" = "4", "without degree" = "5", "other" = "6", "unknown" = "7"),
          Study_enrollment = fct_recode(incl_criteria,  "proteinuria" = "0", "low eGFR value" = "1"),
          hypertension = fct_recode(hypertension, "no" = "0", "yes" = "1"),
          coronary_heart_disease = fct_recode(chd, "no" = "0", "yes" = "1"),
          stroke = fct_recode(stroke, "no" = "0", "yes" = "1"),
          asthma = fct_recode(aa_asthma, "yes" = "1", "no" = "2"),# "unknown" = "3"),
          copd = fct_recode(aa_copd, "yes" = "1", "no" = "2", "unknown" = "3"),
          taking_painkillers = fct_recode(aa_schmerzmittel, "regularly" = "1", "when required" = "2", "never" = "3", "unknown" = "4"),
          leading_kidney_disease = fct_recode(fuehrende_nierenerkr, 
                                              "Obstructive nephropathy" = "1", 
                                              "Acute kidney injury" = "2", 
                                              "Miscellaneous" = "3", 
                                              "Diabetic nephropathy" = "4", 
                                              "Single kidney" = "5", 
                                              "Hereditary kidney disease" = "6", 
                                              "Interstitial nephropathy" = "7", 
                                              "Undetermined" = "8", 
                                              "Primary glomerulopathy" = "9", 
                                              "Systemic disease" = "10", 
                                              "Vascular nephropathy" = "11"),
          Vascular_nephropathy = fct_recode(hyp_neph, "no" = "0", "yes" = "1"),
          Primary_glomerulopathy = fct_recode(pr_glom_path, "no" = "0", "yes" = "1"),
          Diabetic_nephropathy = fct_recode(diab_neph, "no" = "0", "yes" = "1"),
          Systemic_disease = fct_recode(systemic, "no" = "0", "yes" = "1"),
          Hereditary_kidney_disease = fct_recode(hereditary, "no" = "0", "yes" = "1"),
          Interstitial_nephropathy = fct_recode(int_neph, "no" = "0", "yes" = "1"),
          single_kidney = fct_recode(single_kidney, "no" = "0", "yes" = "1"),
          Obstructive_nephropathy = fct_recode(p_renal, "no" = "0", "yes" = "1"),
          Acute_kidney_injury = fct_recode(acute_fail, "no" = "0", "yes" = "1"),
          Miscellaneous = fct_recode(other, "no" = "0", "yes" = "1"),
          Undetermined = fct_recode(unknown, "no" = "0", "yes" = "1")
  ) %>% 
  rename(number_of_siblings = aa_geschwist,
         number_of_people_living_in_the_household = dem_anz_pers,
         BMI = bmi_korr,
         serum_creatinine = crea_original,
         eGFR = eGFR_CKD_EPI,
         CRP = crpvalue1,
         LDL = ldlvalue1,
         HDL =  hdlvalue1,
         number_of_siblings_with_stroke = aa_schlag2,
         number_of_relatives_with_kidney_disease =  aa_nierenerk_1,
         
  )

# Create Table S5 on synthetic data
# status 0: Censored, 1: KF, 2: Death
ts5 <- syn_gckd_recoded %>% select(time, status) %>% table()

write.table(ts5, paste(output_prefix,
                       .Platform$file.sep, "Table_S5_syn.csv", sep = ""),
            row.names = FALSE)

# Create Table S6
ts6 <- table1::table1(~ age + Sex + alcohol + smoking + family_status +  number_of_siblings + 
                        number_of_people_living_in_the_household + employment + private_insurance + professional_qualification 
                      , data = syn_gckd_recoded 
)
write.table(ts6, paste(output_prefix,
                       .Platform$file.sep, "Table_S6_syn.csv", sep = ""),
            row.names = FALSE)

# Create Table S7
ts7 <- table1::table1(~ Study_enrollment + BMI + hypertension + coronary_heart_disease + stroke +  asthma + copd +  taking_painkillers
                      , data = syn_gckd_recoded 
)
write.table(ts7, paste(output_prefix,
                       .Platform$file.sep, "Table_S7_syn.csv", sep = ""),
            row.names = FALSE)
# Create Table S8
ts8 <- table1::table1(~ serum_creatinine + uacr + eGFR  + CRP + LDL + HDL,
                      data = syn_gckd_recoded 
)
write.table(ts8, paste(output_prefix,
                       .Platform$file.sep, "Table_S8_syn.csv", sep = ""),
            row.names = FALSE)

# Create Table S9
ts9 <- table1::table1(~  number_of_siblings_with_stroke + number_of_relatives_with_kidney_disease, 
                      data = syn_gckd_recoded 
)
write.table(ts9, paste(output_prefix,
                       .Platform$file.sep, "Table_S9_syn.csv", sep = ""),
            row.names = FALSE)
# Create Table S10
ts10 <- table1::table1(~leading_kidney_disease + Vascular_nephropathy + Primary_glomerulopathy + Diabetic_nephropathy +
                         Systemic_disease +  Hereditary_kidney_disease + Interstitial_nephropathy + single_kidney +
                         Obstructive_nephropathy + Acute_kidney_injury + Miscellaneous + Undetermined
                       ,data = syn_gckd_recoded 
)
write.table(ts10, paste(output_prefix,
                        .Platform$file.sep, "Table_S10_syn.csv", sep = ""),
            row.names = FALSE)
