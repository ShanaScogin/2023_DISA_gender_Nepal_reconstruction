# system: x86_64, darwin17.0          
# R version 4.1.0 (2021-05-18)
# Camp Pontanezen

# Note: Please run Replication Code file first.
# This is for the predicted probabilities of all the controls for the table.
# These were not interpreted in the paper

#############################################
 ### Observed Predicted Prob Functions ###
############################################

obs_pred <- function(model,
                       op_1,
                       op_0) {
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

obs_fam <- function(model) {
  op_1 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = max(model$data$fam_log),
                    house_damage = model$data$house_damage,
                    marital_status = model$data$marital_status,
                    age = model$data$age,
                    start_reconstruct = model$data$start_reconstruct,
                    district = model$data$district)
  op_0 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = min(model$data$fam_log),
                    house_damage = model$data$house_damage,
                    marital_status = model$data$marital_status,
                    age = model$data$age,
                    start_reconstruct = model$data$start_reconstruct,
                    district = model$data$district)
  out <- obs_pred(model = model,
                  op_1 = op_1,
                  op_0 = op_0)
  out
}

obs_house <- function(model) {
  op_1 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = model$data$fam_log,
                    house_damage = max(model$data$house_damage),
                    marital_status = model$data$marital_status,
                    age = model$data$age,
                    start_reconstruct = model$data$start_reconstruct,
                    district = model$data$district)
  op_0 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = model$data$fam_log,
                    house_damage = min(model$data$house_damage),
                    marital_status = model$data$marital_status,
                    age = model$data$age,
                    start_reconstruct = model$data$start_reconstruct,
                    district = model$data$district)
  out <- obs_pred(model = model,
                  op_1 = op_1,
                  op_0 = op_0)
  out
}

obs_marital <- function(model) {
  op_1 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = model$data$fam_log,
                    house_damage = model$data$house_damage,
                    marital_status = max(model$data$marital_status),
                    age = model$data$age,
                    start_reconstruct = model$data$start_reconstruct,
                    district = model$data$district)
  op_0 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = model$data$fam_log,
                    house_damage = model$data$house_damage,
                    marital_status = min(model$data$marital_status),
                    age = model$data$age,
                    start_reconstruct = model$data$start_reconstruct,
                    district = model$data$district)
  out <- obs_pred(model = model,
                  op_1 = op_1,
                  op_0 = op_0)
  out
}

obs_age <- function(model) {
  op_1 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = model$data$fam_log,
                    house_damage = model$data$house_damage,
                    marital_status = model$data$marital_status,
                    age = max(model$data$age),
                    start_reconstruct = model$data$start_reconstruct,
                    district = model$data$district)
  op_0 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = model$data$fam_log,
                    house_damage = model$data$house_damage,
                    marital_status = model$data$marital_status,
                    age = min(model$data$age),
                    start_reconstruct = model$data$start_reconstruct,
                    district = model$data$district)
  out <- obs_pred(model = model,
                  op_1 = op_1,
                  op_0 = op_0)
  out
}

obs_reconstruct <- function(model) {
  op_1 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = model$data$fam_log,
                    house_damage = model$data$house_damage,
                    marital_status = model$data$marital_status,
                    age = model$data$age,
                    start_reconstruct = max(model$data$start_reconstruct),
                    district = model$data$district)
  op_0 = data.frame(intercept = 1,
                    abroad = model$data$abroad,
                    fam_log = model$data$fam_log,
                    house_damage = model$data$house_damage,
                    marital_status = model$data$marital_status,
                    age = model$data$age,
                    start_reconstruct = min(model$data$start_reconstruct),
                    district = model$data$district)
  out <- obs_pred(model = model,
                  op_1 = op_1,
                  op_0 = op_0)
  out
}

##############################################
  ### Models and predicted probabilities ###
##############################################

set.seed(9999)

## Observed Predicted Probabilities for Model 1 (self visit)
obs_fam_self <- obs_fam(model = mod_self)
obs_house_self <- obs_house(model = mod_self)
obs_marital_self <- obs_marital(model = mod_self)
obs_age_self <- obs_age(model = mod_self)
obs_reconstruct_self <- obs_reconstruct(model = mod_self)

## Observed Predicted Probabilities for Model 2 (consult knowledge)
obs_fam_consult <- obs_fam(model = mod_consult) 
obs_house_consult <- obs_house(model = mod_consult)
obs_marital_consult <- obs_marital(model = mod_consult)
obs_age_consult <- obs_age(model = mod_consult)
obs_reconstruct_consult <- obs_reconstruct(model = mod_consult)

## Observed Predicted Probabilities for Model 3 (pa name)
obs_fam_pa <- obs_fam(model = mod_pa) 
obs_house_pa <- obs_house(model = mod_pa)
obs_marital_pa <- obs_marital(model = mod_pa)
obs_age_pa <- obs_age(model = mod_pa)
obs_reconstruct_pa <- obs_reconstruct(model = mod_pa)




