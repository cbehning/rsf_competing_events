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

ggsave(filename = paste(output_prefix, setup, "CIF_plot_",n,"_", (repeats+1),"rep.png", sep = ""),
       device = "png", width = 10, height = 7)



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

ggsave(filename = paste(output_prefix, setup,"_1000_status_frequencies.png", sep =""),
       device = "png", width = 10, height = 7)

#### Figure: Estimated CIF with Reference limits ####
## Creates Figure S3 or S4 of the manuscript, depending on the choice of setup

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

ggsave(filename = paste(output_prefix,setup, "_CIF_CI_",n,"_",rep,"_rep1.png", sep = ""), 
       device = "png", width = 10, height = 7)


#### Figure: Illustration of comparison of C_i and \hat{C}_i ####
# Creates Figure S5, only created for setup1
if(setup == "setup1"){
  # can only be computed if data was generated first using data_generation.R
  # with respective, n, k=vars, and seed values
  # initialize values
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
  
  ggsave(filename = paste(output_prefix, setup, "_Cens_",n,"_",rep,"_71123.png", sep = ""),
         device = "png", width = 10, height = 7)
  
}


#### Figure: Estimate of G for imputeRoot and imputeNode #####
# Creates figures S6 and S7
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
  
  ggsave(filename = paste(output_prefix, setup, "_Root_Ghat.png", sep =""), device = "png", width = 6, height = 5)
  
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
  
  ggsave(filename = paste(output_prefix, setup, "_Node_Ghat", ".png", sep =""), device = "png", width = 6, height = 5)
}


#### Table: C-Index ####
require(kableExtra)

cindex_df %>%
  left_join(mapping_method_type, by = "method") %>%
  mutate(type = factor(type, levels = c("imputeNode", "imputeRoot", "imputeOnce", "Naive approach", "Reference"))) %>% 
  group_by(type, p, b) %>%
  rename(q = p) %>% 
  mutate(q = paste("0.", q, sep = "")) %>%
  summarise(mean_c_index = paste( sprintf(mean(cindex, na.rm = TRUE),fmt = "%.4f"), " (", sprintf(sd(cindex, na.rm = TRUE),fmt = "%.3f"), ")", sep = "" )) %>%
  pivot_wider(names_from = type, values_from = mean_c_index) %>%
  kableExtra::kable(caption = "Mean (SD) C-index values computed via cIndexin the discSurv Package.",
                    digits = 4) %>% # add format = "latex"
  kableExtra::kable_styling(latex_options = c("striped"), full_width = FALSE) 


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

ggsave(filename = paste(output_prefix,set_up, "_C-index_MCE_",n,"_",repeats+1,"rep.png", sep = ""), device = "png", width = 10, height = 7)  

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

ggsave(filename = paste(output_prefix,setup, "_C-index_SD_MCE",n,"_",repeats+1,"rep.png", sep = ""), device = "png", width = 10, height = 7)  


#### Table: Integrated Brier Score ####
bierscore_df %>%
  left_join(mapping_method_type, by = "method") %>%
  mutate(type = factor(type, levels = c("imputeNode", "imputeRoot", "imputeOnce", "Naive approach", "Reference"))) %>% 
  group_by(type, p, b) %>%
  rename(q = p) %>% 
  mutate(q = paste("0.", q, sep = "")) %>%
  summarise(mean_c_index = paste( sprintf(mean(IBS, na.rm = TRUE),fmt = "%.4f"), " (", sprintf(sd(IBS, na.rm = TRUE),fmt = "%.4f"), ")", sep = "" )) %>%
  pivot_wider(names_from = type, values_from = mean_c_index) %>%
  kableExtra::kable(caption = "Mean (SD) integrated Brier score (IBS)  calculated via the pec package.",
                    digits = 4) %>% # add  format = "latex"
  kableExtra::kable_styling(latex_options = c("striped"), full_width = FALSE) 


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

ggsave(filename = paste(output_prefix, setup, "_C-index_IBS_",n,"_",repeats+1,"rep.png", sep = ""), device = "png", width = 10, height = 7)  


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

ggsave(filename = paste(output_prefix,setup, "_C-index_SD_IBS",n,"_",repeats+1,"rep.png", sep = ""), device = "png", width = 10, height = 7) 


#### Figures: Variable Importance #####
# Plot 10 most important variables
# Creates Figures S16-S20 or S21-S25, dependeing on the setup 

load("setup1_importance.rda" )

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
  
  ggsave(p, filename = paste(output_prefix, setup,"_1000_VIMP_",type_, ".png", sep =""), device = "png", width = 10, height = 7)
}

