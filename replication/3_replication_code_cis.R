# system: x86_64, darwin17.0          
# R version 4.1.0 (2021-05-18)
# Camp Pontanezen

# Note: This file can run without running previous files.
# Run time can be long. If you encounter errors with the parallel
# processing, please clear your global environment and run again. 

# rm()

## packages
library(devtools) # version ‘2.4.2’
library(lme4) # version ‘1.1.27.1’
library(nlme) # version ‘3.1.152’
library(bife) # version ‘0.7.1’
library(parallel) # version ‘4.1.0’

######## Read in dataframe ######
df_earthquake <- read.csv(here("replication", "df_earthquake.csv"))

######## Set up #########
# Create sampler
# Ref: https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
sampler <- function(dat, 
                    clustervar, 
                    replace = TRUE, 
                    reps = 1) {
  cid <- unique(dat[, clustervar[1]])
  ncid <- length(cid)
  recid <- sample(cid, size = ncid * reps, replace = TRUE)
  if (replace) {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, 
            RowID = sample(which(dat[, clustervar] == recid[i]),
                                      size = length(which(dat[, clustervar] == recid[i])), 
                           replace = TRUE))
    })
  } else {
    rid <- lapply(seq_along(recid), function(i) {
      cbind(NewID = i, 
            RowID = which(dat[, clustervar] == recid[i]))
    })
  }
  dat <- as.data.frame(do.call(rbind, rid))
  dat$Replicate <- factor(cut(dat$NewID, 
                              breaks = c(1, ncid * 1:reps), 
                              include.lowest = TRUE,
                              labels = FALSE))
  dat$NewID <- factor(dat$NewID)
  return(dat)
}

# create helper functions
check_boot <- function(model) {
  if ((length(model$alpha[model$alpha >= 20]) <= 0) == TRUE) {
    out <- model
  } else {
    out <- NA
  }
  out
}

pp_boot <- function(model) {
  
  if (is.na(model)) {
    
    out <- NA
    
  } else {
    
    model <- bife::bias_corr(model)
    
    # obs prob function
    obs_pred <- function(model, op_1, op_0) {
      obs_1 <- predict(object = model,
                       X_new = op_1,
                       type = c("response"))
      obs_0 <- predict(object = model,
                       X_new = op_0,
                       type = c("response"))
      obs <- obs_1 - obs_0
      out <- mean(obs)
      
      out
    }
    
    # abroad
    nd1 = data.frame(intercept = 1,
                     abroad = max(model$data$abroad),
                     fam_log = model$data$fam_log,
                     house_damage = model$data$house_damage,
                     marital_status = model$data$marital_status,
                     age = model$data$age,
                     start_reconstruct = model$data$start_reconstruct,
                     district = model$data$district)
    nd0 = data.frame(intercept = 1,
                     abroad = min(model$data$abroad),
                     fam_log = model$data$fam_log,
                     house_damage = model$data$house_damage,
                     marital_status = model$data$marital_status,
                     age = model$data$age,
                     start_reconstruct = model$data$start_reconstruct,
                     district = model$data$district)
    out_abroad <- obs_pred(model = model,
                           op_1 = nd1,
                           op_0 = nd0)
    
    # out_fam_log
    nd1f = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = max(model$data$fam_log),
                      house_damage = model$data$house_damage,
                      marital_status = model$data$marital_status,
                      age = model$data$age,
                      start_reconstruct = model$data$start_reconstruct,
                      district = model$data$district)
    nd0f = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = min(model$data$fam_log),
                      house_damage = model$data$house_damage,
                      marital_status = model$data$marital_status,
                      age = model$data$age,
                      start_reconstruct = model$data$start_reconstruct,
                      district = model$data$district)
    out_fam_log <- obs_pred(model = model,
                            op_1 = nd1f,
                            op_0 = nd0f)
    
    # out_house_damage
    nd1h = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = model$data$fam_log,
                      house_damage = max(model$data$house_damage),
                      marital_status = model$data$marital_status,
                      age = model$data$age,
                      start_reconstruct = model$data$start_reconstruct,
                      district = model$data$district)
    nd0h = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = model$data$fam_log,
                      house_damage = min(model$data$house_damage),
                      marital_status = model$data$marital_status,
                      age = model$data$age,
                      start_reconstruct = model$data$start_reconstruct,
                      district = model$data$district)
    out_house_damage <- obs_pred(model = model,
                                 op_1 = nd1h,
                                 op_0 = nd0h)
    
    # out_marital_status
    nd1m = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = model$data$fam_log,
                      house_damage = model$data$house_damage,
                      marital_status = max(model$data$marital_status),
                      age = model$data$age,
                      start_reconstruct = model$data$start_reconstruct,
                      district = model$data$district)
    nd0m = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = model$data$fam_log,
                      house_damage = model$data$house_damage,
                      marital_status = min(model$data$marital_status),
                      age = model$data$age,
                      start_reconstruct = model$data$start_reconstruct,
                      district = model$data$district)
    out_marital_status <- obs_pred(model = model,
                                   op_1 = nd1m,
                                   op_0 = nd0m)
    
    # out_age
    nd1a = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = model$data$fam_log,
                      house_damage = model$data$house_damage,
                      marital_status = model$data$marital_status,
                      age = max(model$data$age),
                      start_reconstruct = model$data$start_reconstruct,
                      district = model$data$district)
    nd0a = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = model$data$fam_log,
                      house_damage = model$data$house_damage,
                      marital_status = model$data$marital_status,
                      age = min(model$data$age),
                      start_reconstruct = model$data$start_reconstruct,
                      district = model$data$district)
    out_age <- obs_pred(model = model,
                        op_1 = nd1a,
                        op_0 = nd0a)
    
    # out_start_reconstruct
    nd1s = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = model$data$fam_log,
                      house_damage = model$data$house_damage,
                      marital_status = model$data$marital_status,
                      age = model$data$age,
                      start_reconstruct = max(model$data$start_reconstruct),
                      district = model$data$district)
    nd0s = data.frame(intercept = 1,
                      abroad = model$data$abroad,
                      fam_log = model$data$fam_log,
                      house_damage = model$data$house_damage,
                      marital_status = model$data$marital_status,
                      age = model$data$age,
                      start_reconstruct = min(model$data$start_reconstruct),
                      district = model$data$district)
    out_start_reconstruct <- obs_pred(model = model,
                                      op_1 = nd1s,
                                      op_0 = nd0s)
    
    out <- c(out_abroad, out_fam_log, out_house_damage, out_marital_status,
             out_age, out_start_reconstruct) }
  
  out
  
}

#############################################
### Visiting a govt official by oneself ###
############################################

# data
df_self <- df_earthquake[df_earthquake$visit_office == 1 & 
                           !is.na(df_earthquake$visit_office), ]
df_self <- data.frame(df_self[, c("fam_log", 
                                  "age", 
                                  "marital_status", 
                                  "house_damage", 
                                  "start_reconstruct",
                                  "abroad", 
                                  "district", 
                                  "self_visit")]) # 259 rows
# sample data
set.seed(9999)
reps <- 1000

tmp <- sampler(dat = df_self, 
               clustervar = "district", 
               reps = reps)
bigdata <- cbind(tmp, df_self[tmp$RowID, ])

# make the clusters
cl <- parallel::makeCluster(4, setup_strategy = "sequential")
parallel::clusterExport(cl, c("bigdata"))
parallel::clusterEvalQ(cl, require(bife))

# make the function
abroad_boot <- function(i) {
  model <- try(bife::bife(self_visit ~
                            fam_log + 
                            house_damage +
                            marital_status +
                            age +
                            start_reconstruct +
                            abroad | district,
                            data = bigdata[bigdata$Replicate == i, ]),
               silent = TRUE)
  if (class(model) == "try-error")
    return(model)
  model
}

# run the models
start <- proc.time()
out <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = abroad_boot)
check <- parLapplyLB(cl, X = out, fun = check_boot)
res <- parLapplyLB(cl, X = check, fun = pp_boot)
end <- proc.time()

# shut down the cluster
stopCluster(cl)

# format the cis
out_abroad <- NA
out_fam_log <- NA
out_house_damage <- NA
out_marital_status <- NA
out_age <- NA
out_start_reconstruct <- NA

for (i in 1:length(res)) {
  
  out_abroad[i] <- res[[i]][1]
  out_fam_log[i] <- res[[i]][2]
  out_house_damage[i] <- res[[i]][3]
  out_marital_status[i] <- res[[i]][4]
  out_age[i] <- res[[i]][5]
  out_start_reconstruct[i] <- res[[i]][6]
  
  #return
  out_abroad
  out_fam_log
  out_house_damage
  out_marital_status
  out_age
  out_start_reconstruct

}

self_ci_abroad <- c(quantile(out_abroad, .025, na.rm = TRUE),
               quantile(out_abroad, .975, na.rm = TRUE)) 
self_ci_fam_log <- c(quantile(out_fam_log, .025, na.rm = TRUE),
                quantile(out_fam_log, .975, na.rm = TRUE)) 
self_ci_house_damage <- c(quantile(out_house_damage, .025, na.rm = TRUE),
                     quantile(out_house_damage, .975, na.rm = TRUE)) 
self_ci_marital_status <- c(quantile(out_marital_status, .025, na.rm = TRUE),
                       quantile(out_marital_status, .975, na.rm = TRUE)) 
self_ci_age <- c(quantile(out_age, .025, na.rm = TRUE),
            quantile(out_age, .975, na.rm = TRUE)) 
self_ci_start_reconstruct <- c(quantile(out_start_reconstruct, .025, na.rm = TRUE),
                          quantile(out_start_reconstruct, .975, na.rm = TRUE)) 

#############################################
###    Having knowledge of consulting    ###
############################################

df_consult <- data.frame(df_earthquake[, c("fam_log", 
                                           "age", 
                                           "marital_status", 
                                           "house_damage",
                                           "abroad", 
                                           "start_reconstruct",
                                           "district",
                                           "consult_knowledge")]) # 453 rows
df_consult <- df_consult[!is.na(df_consult$consult_knowledge), ] # 453 rows

# sample data
set.seed(9999)
reps <- 1000

tmp <- sampler(dat = df_consult, 
               clustervar = "district", 
               reps = reps)
bigdata <- cbind(tmp, df_consult[tmp$RowID, ])

# make the clusters
cl <- parallel::makeCluster(4, setup_strategy = "sequential")
parallel::clusterExport(cl, c("bigdata"))
parallel::clusterEvalQ(cl, require(bife))

# make the function
abroad_boot <- function(i) {
  model <- try(bife::bife(consult_knowledge ~
                            fam_log + 
                            house_damage +
                            marital_status +
                            age +
                            start_reconstruct +
                            abroad | district,
                          data = bigdata[bigdata$Replicate == i, ]),
               silent = TRUE)
  if (class(model) == "try-error")
    return(model)
  model
}

# run the models
start <- proc.time()
out <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = abroad_boot)
check <- parLapplyLB(cl, X = out, fun = check_boot)
res <- parLapplyLB(cl, X = check, fun = pp_boot)
end <- proc.time()

# shut down the cluster
stopCluster(cl)

# format the cis
out_abroad <- NA
out_fam_log <- NA
out_house_damage <- NA
out_marital_status <- NA
out_age <- NA
out_start_reconstruct <- NA

for (i in 1:length(res)) {
  
  out_abroad[i] <- res[[i]][1]
  out_fam_log[i] <- res[[i]][2]
  out_house_damage[i] <- res[[i]][3]
  out_marital_status[i] <- res[[i]][4]
  out_age[i] <- res[[i]][5]
  out_start_reconstruct[i] <- res[[i]][6]
  
  #return
  out_abroad
  out_fam_log
  out_house_damage
  out_marital_status
  out_age
  out_start_reconstruct
  
}

consult_ci_abroad <- c(quantile(out_abroad, .025, na.rm = TRUE),
                    quantile(out_abroad, .975, na.rm = TRUE))
consult_ci_fam_log <- c(quantile(out_fam_log, .025, na.rm = TRUE),
                     quantile(out_fam_log, .975, na.rm = TRUE))
consult_ci_house_damage <- c(quantile(out_house_damage, .025, na.rm = TRUE),
                          quantile(out_house_damage, .975, na.rm = TRUE))
consult_ci_marital_status <- c(quantile(out_marital_status, .025, na.rm = TRUE),
                            quantile(out_marital_status, .975, na.rm = TRUE))
consult_ci_age <- c(quantile(out_age, .025, na.rm = TRUE),
                 quantile(out_age, .975, na.rm = TRUE))
consult_ci_start_reconstruct <- c(quantile(out_start_reconstruct, .025, na.rm = TRUE),
                               quantile(out_start_reconstruct, .975, na.rm = TRUE))

#############################################
###    Having a PA in one's own name    ###
############################################

df_pa <- data.frame(df_earthquake[, c("fam_log", 
                                      "age", 
                                      "marital_status", 
                                      "house_damage",
                                      "start_reconstruct",
                                      "abroad", 
                                      "district", 
                                      "pa_name_self"
                                      )]) # 453 rows
df_pa <- df_pa[!is.na(df_pa$pa_name_self), ] # 414 rows

# sample data
set.seed(9999)
reps <- 1000

tmp <- sampler(dat = df_pa, 
               clustervar = "district", 
               reps = reps)
bigdata <- cbind(tmp, df_pa[tmp$RowID, ])

# make the clusters
cl <- parallel::makeCluster(4, setup_strategy = "sequential")
parallel::clusterExport(cl, c("bigdata"))
parallel::clusterEvalQ(cl, require(bife))

# make the function
abroad_boot <- function(i) {
  model <- try(bife::bife(pa_name_self ~
                            fam_log + 
                            house_damage +
                            marital_status +
                            age +
                            start_reconstruct +
                            abroad | district,
                          data = bigdata[bigdata$Replicate == i, ]),
               silent = TRUE)
  if (class(model) == "try-error")
    return(model)
  model
}

# run the models
start <- proc.time()
out <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = abroad_boot)
check <- parLapplyLB(cl, X = out, fun = check_boot)
res <- parLapplyLB(cl, X = check, fun = pp_boot)
end <- proc.time()

# shut down the cluster
stopCluster(cl)

# format the cis
out_abroad <- NA
out_fam_log <- NA
out_house_damage <- NA
out_marital_status <- NA
out_age <- NA
out_start_reconstruct <- NA

for (i in 1:length(res)) {
  
  out_abroad[i] <- res[[i]][1]
  out_fam_log[i] <- res[[i]][2]
  out_house_damage[i] <- res[[i]][3]
  out_marital_status[i] <- res[[i]][4]
  out_age[i] <- res[[i]][5]
  out_start_reconstruct[i] <- res[[i]][6]
  
  #return
  out_abroad
  out_fam_log
  out_house_damage
  out_marital_status
  out_age
  out_start_reconstruct
  
}

pa_ci_abroad <- c(quantile(out_abroad, .025, na.rm = TRUE),
                    quantile(out_abroad, .975, na.rm = TRUE))
pa_ci_fam_log <- c(quantile(out_fam_log, .025, na.rm = TRUE),
                     quantile(out_fam_log, .975, na.rm = TRUE))
pa_ci_house_damage <- c(quantile(out_house_damage, .025, na.rm = TRUE),
                          quantile(out_house_damage, .975, na.rm = TRUE))
pa_ci_marital_status <- c(quantile(out_marital_status, .025, na.rm = TRUE),
                            quantile(out_marital_status, .975, na.rm = TRUE))
pa_ci_age <- c(quantile(out_age, .025, na.rm = TRUE),
                 quantile(out_age, .975, na.rm = TRUE))
pa_ci_start_reconstruct <- c(quantile(out_start_reconstruct, .025, na.rm = TRUE),
                               quantile(out_start_reconstruct, .975, na.rm = TRUE))

