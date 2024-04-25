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


# The following contains R-Code to help createing simulation data of section 3

require(tidyverse)
require(discSurv)

# compute quantiles (limits) for discretizing 
# the simulated data 
get_limits <- function(p, nvar = vars, scenario = scenario){
  set.seed(300623)
  n     <- 1000000
  X <- matrix(rnorm(n*nvar), nrow = n, ncol = nvar)
  # 1 a) Cause specific hazard for event 1 and event 2
  # tree like structure for both linear predictors
  # 20231121
  if(scenario == "231121"){
    eta1 <- (X[,1]<0.5) * ((X[,2]<0) * X[, 4] + (X[,2]>=0) * X[, 5]) +
      (X[,1]>=0.5) * ((X[,3]<0) * X[, 4] + (X[,3]>=0) * X[, 4] * X[, 5])
    
    eta2 <- (X[,1]<1) * ((X[,1]<0) * (-1) *X[, 4] + (X[,1]>=0) * X[, 5]) +
      (X[,1]>=1) * ((X[,3]<0) * X[, 4] + (X[,3]>=0) * (-1) * X[, 4] * X[, 6])
  }else if(scenario == "231211"){
    # linear multiplicative
    eta1 <- X[,1] * X[, 2] * X[, 3] +
      X[,1] * X[, 4] * X[, 5] +
      X[,1] * X[, 3] * X[, 5] + 
      X[,1] * X[, 3] * X[, 4] +
      X[,2] * X[, 3] * X[, 4] 
    
    eta2 <- X[,1] * 1 * X[, 3] +
      X[,4] * X[, 6] * X[, 7] +
      X[,1] * X[, 4] * X[, 6] + 
      X[,1] * X[, 3] * X[, 7] +
      X[,1] * X[, 3] * X[, 4] 
  }else if(scenario == "231213"){
    eta1 <- X[,1] * X[, 2] * X[, 3] +
      X[,1] * X[, 4] * X[, 5] +
      X[,1] * X[, 3] * X[, 5] + 
      X[,1] * X[, 3] * X[, 4] +
      X[,2] * X[, 3] * X[, 4] 
    
    eta2 <- X[,1] * 1 * X[, 3] +
      X[,4] * X[, 6] * X[, 7] +
      X[,1] * X[, 4] * X[, 6] + 
      X[,1] * X[, 3] * X[, 7] +
      X[,1] * X[, 3] * X[, 4] 
    eta1 <- 2* eta1
    eta2 <- 2* eta2
  }
  
  p1  <- prob1(p, eta1)
  ev  <- sapply(1:n, function(j) rbinom(1, 1, 1 - p1[j]) + 1)
  
  u1  <- runif(n, 0, 1)
  t1  <- invcdf1(p, u1, eta1, p1)
  t2  <- sapply(1:n, function(j) rexp(1, rate = exp(eta2[j])))
  
  t <- ifelse(ev == 1, t1, t2)
  
  quantile(t, probs = seq(0.05, 0.95, by = 0.05))
}


# probability for type 1
prob1 <- function(p, eta){
  1-(1-p)^exp(eta)
}

# subdistribution for type 1 
cdf1 <- function(t, p, eta){
  1-(1-p*(1-exp(-t)))^(exp(eta))
}

# inverse subdistribution for type 1 
invcdf1 <- function(p, u, eta, p1){
  -log(((1-p1*u)^(exp(-eta))-1+p)/p)
}

# discrete probability of censoring 
cens_dist <- function(b, limits){
  b_pot <- b^((length(limits)+1):1)
  ps    <- b_pot/sum(b_pot)
}

writeObjects <- function(data, obj_set, path, name, vars ){
  write.table(x= data[data$obj %in% obj_set, -1 ],
              file = paste(path, "data_", name, "_p_",vars, "_k.csv", sep = ""), # todo
              sep =",",
              quote = FALSE,
              col.names = TRUE,
              row.names = FALSE)
}

simfun <- function(n = 300, k = 20,b =1,  alpha = 3, nvar=vars,  path, writeBeforeCensoring = FALSE, scenario = scenario ){
  # 0. Generate set of nvar uniform distributed covariates for n persons/samples
  X <- round(matrix(rnorm(n*nvar), nrow = n, ncol = nvar),4)
  # 1 specific hazard for event 1 and event 2
  # tree like structure for both linear predictors
  if(scenario == "231121"){
    eta1 <- (X[,1]<0.5) * ((X[,2]<0) * X[, 4] + (X[,2]>=0) * X[, 5]) +
      (X[,1]>=0.5) * ((X[,3]<0) * X[, 4] + (X[,3]>=0) * X[, 4] * X[, 5])
    
    eta2 <- (X[,1]<1) * ((X[,1]<0) * (-1) *X[, 4] + (X[,1]>=0) * X[, 5]) +
      (X[,1]>=1) * ((X[,3]<0) * X[, 4] + (X[,3]>=0) * (-1) * X[, 4] * X[, 6])
  }else if(scenario == "231211"){
    # linear multiplicative
    eta1 <- X[,1] * X[, 2] * X[, 3] +
      X[,1] * X[, 4] * X[, 5] +
      X[,1] * X[, 3] * X[, 5] + 
      X[,1] * X[, 3] * X[, 4] +
      X[,2] * X[, 3] * X[, 4] 
    
    eta2 <- X[,1] * 1 * X[, 3] +
      X[,4] * X[, 6] * X[, 7] +
      X[,1] * X[, 4] * X[, 6] + 
      X[,1] * X[, 3] * X[, 7] +
      X[,1] * X[, 3] * X[, 4] # in beiden
  }else if(scenario == "231213"){
    eta1 <- X[,1] * X[, 2] * X[, 3] +
      X[,1] * X[, 4] * X[, 5] +
      X[,1] * X[, 3] * X[, 5] + 
      X[,1] * X[, 3] * X[, 4] +
      X[,2] * X[, 3] * X[, 4] 
    
    eta2 <- X[,1] * 1 * X[, 3] +
      X[,4] * X[, 6] * X[, 7] +
      X[,1] * X[, 4] * X[, 6] + 
      X[,1] * X[, 3] * X[, 7] +
      X[,1] * X[, 3] * X[, 4] 
    eta1 <- 2* eta1
    eta2 <- 2* eta2
  }
  
  
  p1  <- prob1(p, eta1)
  ev  <- sapply(1:n, function(j) rbinom(1,1,1-p1[j])+1)
  
  u1  <- runif(n, 0, 1)
  t1  <- invcdf1(p, u1, eta1, p1)
  t2  <- sapply(1:n, function(j) rexp(1, rate=exp(eta2[j])))
  
  t <- ifelse(ev==1, t1, t2)
  
  t_disc <- sapply(1:n, function(j) findInterval(t[j],limits)+1)
  C_disc <- sample(1:(length(limits)+1), n, prob=cens_dist(b, limits), replace=TRUE)
  
  time   <- sapply(1:n, function(j) min(C_disc[j], t_disc[j]))
  
  status <- ifelse(C_disc<t_disc, 0, ev)
  if(length(table(status))==3){
    valid <- TRUE
  }
  
  data <- data.frame(time = time,
                     status = status, 
                     cens_time = C_disc,
                     t1_time =  sapply(1:n, function(j) findInterval(t1[j],limits)+1),
                     t2_time =  sapply(1:n, function(j) findInterval(t2[j],limits)+1),
                     X)
  if(writeBeforeCensoring){
    tmp <- unlist(lapply(1:n, function(x) cdf1(1:k, p, (eta1)[x])))
    tmp <- t(matrix(tmp, nrow = k)) # t = 20 each row is one true Cdf
    write.table(tmp, file =  paste(paste(path, "true_cdf1_", sep =""),
                                   seed, "_", n, "_", p, "_", b, "_", vars, "_k.csv", sep =""), sep = ",")
  }
  
  return(data)
}

createSampledData <- function(p = 0.2, b = 1, n = 300, seed = seed,  path = f_path, limits, vars = vars, scenario = scenario){
  # get limits for ~ uniform events in time intervals 
  if(is.null(limits)){
    limits <- get_limits(p = p, vars = vars, scenario = scenario)  
  }
  set.seed(seed)
  # create simulation data
  data_sim <- simfun(n = n, k = 20, b=b, nvar = vars,  path = path, writeBeforeCensoring = FALSE, scenario = scenario )
  # write simulation data
  write.table(data_sim, file =  paste(paste(f_path, "raw_data", sep =""), "_",
                                      seed,  "_", n, "_", p, "_", b, "_", vars, "_k.csv", sep =""), sep = ",")
  
  data_sim <- data.frame( obj = 1:nrow(data_sim), data_sim)
  data_sim_cens <- data_sim # e1 data
  data_sim_cens$time <- ifelse(data_sim_cens$status == 2, data_sim_cens$cens_time, data_sim_cens$time )
  data_sim_cens$status <- ifelse(data_sim_cens$status ==2, 0, data_sim_cens$status )
  
  data_sim_cens$cens_time <- NULL
  data_sim_cens$t1_time <- NULL
  data_sim_cens$t2_time <- NULL
  
    # remove information for e1, e2 dataset
  data_sim$cens_time <- NULL
  data_sim$t1_time <- NULL
  data_sim$t2_time <- NULL
  
  #create stratified training and test sets
  for(stat in sort(unique(data_sim$status))){
    obj_list <- unique(data_sim$obj[data_sim$status == stat])
    
    if(! exists("train_set")){
      train_set <- sample(obj_list, length(obj_list) *2/3)
      test_set <- obj_list[!(obj_list %in% train_set)]
      
    }else{
      train_set <- c(train_set, sample(obj_list, length(obj_list) *2/3))
      test_set <- c(test_set, obj_list[!(obj_list %in% train_set)])
    }
  }
  # write reference data - comparison for evaluation:
  writeObjects(data = data_sim_cens,
               obj_set = train_set, path = f_path, name = paste("e1_train", p, b, seed, n, sep = "_"),
               vars = vars )
  writeObjects(data = data_sim_cens,
               obj_set = test_set, path = f_path, name = paste("e1_test", p, b, seed, n, sep = "_"),
               vars = vars)
  
  # write training and test data
  writeObjects(data = data_sim,
               obj_set = train_set, path = f_path, name = paste("train", p, b, seed, n, sep = "_"),
               vars = vars)
  writeObjects(data = data_sim,
               obj_set = test_set, path = f_path, name = paste("test", p, b, seed, n, sep = "_"),
               vars = vars)
  
  # write list with object mapping (train/test)
  write.table(x= data_sim %>%
                mutate(set = ifelse(obj %in% train_set, "train", "test")) %>%
                select(obj, set),
              file = paste(path, paste("obj_List", p, b, seed, n, sep = "_"), "_p_",vars, "_k",".txt", sep = ""),
              sep =",",
              quote = FALSE,
              col.names = TRUE,
              row.names = FALSE)
  
}

## imputeOnce, already part of https://github.com/shekoufeh/Deep-Survival-Analysis-With-Competing-Events
DRSA_createSampledRawOutput21 <- function(dataS = dataS, eventCols = c("e1", "e2"), eoi = "e1", timeCol = "time", seed2 = "1234"){
  set.seed(seed2)
  # initialize dataframe
  df_sample <-  data.frame(matrix(nrow = 0, ncol = ncol(dataS)+5 ) ) 
  names(df_sample) <- c("obj", "timeInt", "y", names(dataS), "subDistWeights", "v_samplegew") #
  
  for(eventofInerest in eoi){ # 
    # build augmented data matrix 
    dataLongSV <-
      dataLongSubDist(dataShort =  dataS, timeColumn = timeCol, eventColumns = eventCols, eventFocus = eventofInerest) %>%
      group_by(obj) %>% 
      arrange(timeInt) %>% 
      mutate( v_samplegew =  (subDistWeights- rev(lag(rev(subDistWeights)))),
              maxt = max(as.numeric(timeInt))) %>% 
      mutate(v_samplegew = ifelse(timeInt == maxt, subDistWeights, v_samplegew))%>%
      select(-maxt) %>%
      data.frame() 
    
    obj_list <- unique(dataLongSV$obj)
    
    # add subjects with events of interest to final df
    df <- cbind(dataLongSV[which(dataLongSV[, eventofInerest] == 1  &
                                   dataLongSV$timeInt == dataLongSV[,timeCol]), ])
    
    # add complete censored subjects
    tmp <- cbind(dataLongSV[which(rowSums(dataLongSV[, eventCols]) == 0 &
                                    dataLongSV$timeInt == dataLongSV[,timeCol]),])
    
    df <- rbind(df, tmp)
    
    
    # only impute subjects with competing event
    obj_list <- obj_list[!(obj_list %in% df$obj)]
    print(paste("Impute" , length(obj_list), "objects for event ", eventofInerest ))
    
    for (obj in obj_list) {
      tmp_subset <- dataLongSV[dataLongSV$obj == obj, ] 
      #tmp_subset$v_samplegew[is.na(tmp_subset$v_samplegew)] <- 0 # set v_ik = 0
      
      t_max <- max(as.numeric(tmp_subset$timeInt))
      
      if( (unique(tmp_subset[ ,timeCol]) != t_max) & sum(tmp_subset$v_samplegew)>0  ){
        #sampling
        t_tmp <- sample(tmp_subset$timeInt, 1, prob = tmp_subset$v_samplegew)
      }else{
        t_tmp <- t_max
      }
      
      tmp <-cbind(tmp_subset[tmp_subset$timeInt == t_tmp, ])
      
      df <- rbind(df, tmp)
    }
    df_sample <- rbind(df_sample, df)
  }
  return(df_sample)
}
