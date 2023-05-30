# system: x86_64, darwin17.0          
# R version 4.1.0 (2021-05-18)
# Camp Pontanezen

## packages
library(bife) # version ‘0.7.1’
library(lme4) # version ‘1.1.27.1’
library(here) # version ‘1.0.1’
library(dplyr) # version ‘1.0.9’

######## Set up the dataframe ######
df_earthquake <- read.csv(here("df_earthquake.csv")) # here() for use with Rproj

#############################################
### Visiting a govt official by oneself ###
############################################

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

set.seed(9999)

## ICC
m_empty <- lme4::lmer(self_visit ~ (1 | district), 
                      data = df_self) 
summary(m_empty)
variance <- as.data.frame(lme4::VarCorr(m_empty)) 
variance <- dplyr::select(variance, vcov)
variance
sigma_j_sq <- variance[1, 1]
sigma_i_sq <- variance[2, 1]
icc_empty <- sigma_j_sq / (sigma_j_sq + sigma_i_sq) 
icc_empty ## icc of 0.18

# Fit with bife
fit1_bife <- bife::bife(self_visit ~
                          fam_log + 
                          house_damage +
                          marital_status +
                          age +
                          start_reconstruct +
                          abroad | district,
                        data = df_self)
mod_self <- bife::bias_corr(fit1_bife)
summary(mod_self)

## Predicted Probabilities Average Value
pp_self_1 = data.frame(intercept = 1,
                          abroad = 1,
                          fam_log = mean(mod_self$data$fam_log),
                          house_damage = 3,
                          marital_status = 1,
                          age = mean(mod_self$data$age),
                          start_reconstruct = 1,
                          district = unique(mod_self$data$district))
pp_self_0 = data.frame(intercept = 1,
                          abroad = 0,
                          fam_log = mean(mod_self$data$fam_log),
                          house_damage = 3,
                          marital_status = 1,
                          age = mean(mod_self$data$age),
                          start_reconstruct = 1,
                          district = unique(mod_self$data$district))
pred_self_1 <- predict(object = mod_self,
                                X_new = pp_self_1,
                          type = c("response"))[1]
pred_self_0 <- predict(object = mod_self,
                                X_new = pp_self_0,
                          type = c("response"))[1]
pred_self <- pred_self_1 - pred_self_0 # 0.3842035

## Predicted Probabilities Observed Value
op_self_1 = data.frame(intercept = 1,
                      abroad = 1,
                      fam_log = mod_self$data$fam_log,
                      house_damage = mod_self$data$house_damage,
                      marital_status = mod_self$data$marital_status,
                      age = mod_self$data$age,
                      start_reconstruct = mod_self$data$start_reconstruct,
                      district = mod_self$data$district)
op_self_0 = data.frame(intercept = 1,
                      abroad = 0,
                      fam_log = mod_self$data$fam_log,
                      house_damage = mod_self$data$house_damage,
                      marital_status = mod_self$data$marital_status,
                      age = mod_self$data$age,
                      start_reconstruct = mod_self$data$start_reconstruct,
                      district = mod_self$data$district)
obs_self_1 <- predict(object = mod_self,
                           X_new = op_self_1,
                           type = c("response"))
obs_self_0 <- predict(object = mod_self,
                           X_new = op_self_0,
                           type = c("response"))
obs_self <- obs_self_1 - obs_self_0
obs_abroad_self <- mean(obs_self) # 0.3019357 
                                  # note this is conservative compared to the more
                                  # standard way of estimating predicted probabilities.
                                  # An even better way would be to turn marriage
                                  # off and on to see the difference in predicted probs

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

set.seed(9999)

## ICC
m_empty <- lme4::lmer(consult_knowledge ~ (1 | district), 
                      data = df_consult) 
summary(m_empty)
variance <- as.data.frame(lme4::VarCorr(m_empty)) 
variance <- dplyr::select(variance, vcov)
variance
sigma_j_sq <- variance[1, 1]
sigma_i_sq <- variance[2, 1]
icc_empty <- sigma_j_sq / (sigma_j_sq + sigma_i_sq) 
icc_empty ## icc of 0.21

# Fit with bife
fit1_bife <- bife::bife(consult_knowledge ~
                          fam_log + 
                          house_damage +
                          marital_status +
                          age +
                          start_reconstruct +
                          abroad | district,
                        data = df_consult)
mod_consult <- bife::bias_corr(fit1_bife)
summary(mod_consult)

## Predicted Probabilities Average Value
pp_consult_1 = data.frame(intercept = 1,
                           abroad = 1,
                           fam_log = mean(mod_consult$data$fam_log),
                           house_damage = 3,
                           marital_status = 1,
                           age = mean(mod_consult$data$age),
                           start_reconstruct = 1,
                           district = unique(mod_consult$data$district))
pp_consult_0 = data.frame(intercept = 1,
                           abroad = 0,
                           fam_log = mean(mod_consult$data$fam_log),
                           house_damage = 3,
                           marital_status = 1,
                           age = mean(mod_consult$data$age),
                           start_reconstruct = 1,
                           district = unique(mod_consult$data$district))
pred_consult_1 <- predict(object = mod_consult,
                           X_new = pp_consult_1,
                           type = c("response"))[1]
pred_consult_0 <- predict(object = mod_consult,
                           X_new = pp_consult_0,
                           type = c("response"))[1]
pred_consult <- pred_consult_1 - pred_consult_0 # 0.09994364

## Predicted Probabilities Observed Value
op_consult_1 = data.frame(intercept = 1,
                      abroad = 1,
                      fam_log = mod_consult$data$fam_log,
                      house_damage = mod_consult$data$house_damage,
                      marital_status = mod_consult$data$marital_status,
                      age = mod_consult$data$age,
                      start_reconstruct = mod_consult$data$start_reconstruct,
                      district = mod_consult$data$district)
op_consult_0 = data.frame(intercept = 1,
                      abroad = 0,
                      fam_log = mod_consult$data$fam_log,
                      house_damage = mod_consult$data$house_damage,
                      marital_status = mod_consult$data$marital_status,
                      age = mod_consult$data$age,
                      start_reconstruct = mod_consult$data$start_reconstruct,
                      district = mod_consult$data$district)
obs_consult_1 <- predict(object = mod_consult,
                      X_new = op_consult_1,
                      type = c("response"))
obs_consult_0 <- predict(object = mod_consult,
                      X_new = op_consult_0,
                      type = c("response"))
obs_consult <- obs_consult_1 - obs_consult_0
obs_abroad_consult <- mean(obs_consult) # 0.1367358

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

set.seed(9999)

## ICC
m_empty <- lme4::lmer(pa_name_self ~ (1 | district), 
                      data = df_pa) 
summary(m_empty)
variance <- as.data.frame(lme4::VarCorr(m_empty)) 
variance <- dplyr::select(variance, vcov)
variance
sigma_j_sq <- variance[1, 1]
sigma_i_sq <- variance[2, 1]
icc_empty <- sigma_j_sq / (sigma_j_sq + sigma_i_sq) 
icc_empty ## icc of 0.16

# Fit with bife
fit1_bife <- bife::bife(pa_name_self ~
                          fam_log + 
                          house_damage +
                          marital_status +
                          age +
                          start_reconstruct +
                          abroad | district,
                        data = df_pa)
mod_pa <- bife::bias_corr(fit1_bife)
summary(mod_pa)

## Predicted Probabilities Average Value
pp_pa_1 = data.frame(intercept = 1,
                          abroad = 1,
                          fam_log = mean(mod_pa$data$fam_log),
                          house_damage = 3,
                          marital_status = 1,
                          age = mean(mod_pa$data$age),
                          start_reconstruct = 1,
                          district = unique(mod_pa$data$district))
pp_pa_0 = data.frame(intercept = 1,
                          abroad = 0,
                          fam_log = mean(mod_pa$data$fam_log),
                          house_damage = 3,
                          marital_status = 1,
                          age = mean(mod_pa$data$age),
                          start_reconstruct = 1,
                          district = unique(mod_pa$data$district))
pred_pa_1 <- predict(object = mod_pa,
                          X_new = pp_pa_1,
                          type = c("response"))[1]
pred_pa_0 <- predict(object = mod_pa,
                          X_new = pp_pa_0,
                          type = c("response"))[1]
pred_pa <- pred_pa_1 - pred_pa_0 # 0.1617617

## Predicted Probabilities Observed Value
op_pa_1 = data.frame(intercept = 1,
                          abroad = 1,
                          fam_log = mod_pa$data$fam_log,
                          house_damage = mod_pa$data$house_damage,
                          marital_status = mod_pa$data$marital_status,
                          age = mod_pa$data$age,
                          start_reconstruct = mod_pa$data$start_reconstruct,
                          district = mod_pa$data$district)
op_pa_0 = data.frame(intercept = 1,
                          abroad = 0,
                          fam_log = mod_pa$data$fam_log,
                          house_damage = mod_pa$data$house_damage,
                          marital_status = mod_pa$data$marital_status,
                          age = mod_pa$data$age,
                          start_reconstruct = mod_pa$data$start_reconstruct,
                          district = mod_pa$data$district)
obs_pa_1 <- predict(object = mod_pa,
                         X_new = op_pa_1,
                         type = c("response"))
obs_pa_0 <- predict(object = mod_pa,
                         X_new = op_pa_0,
                         type = c("response"))
obs_pa <- obs_pa_1 - obs_pa_0
obs_abroad_pa <- mean(obs_pa) # 0.1264191

