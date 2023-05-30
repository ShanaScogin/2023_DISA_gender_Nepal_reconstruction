# system: x86_64, darwin17.0          
# R version 4.1.0 (2021-05-18)
# Camp Pontanezen

# This file contains linear probability models (LPMs) for each model to serve for
# comparison to the predicted probabilities and data visualization code.
# The data vis are not in the main article or appendix, but might come in handy.
# The code can also be modified to produce marginal effects plots. Please
# run all other replication files before running the data visualization code.

library(extrafont) # version ‘0.17’
library(ggplot2) # version ‘3.4.1’
library(dplyr) # version ‘1.0.9’
library(gridExtra) # version ‘2.3’

######################################################
### Linear probability models with and w/o controls ###
######################################################
summary(lm(self_visit ~ abroad + # without covariates
             as.factor(district),
           data = df_self))

summary(lm(self_visit ~ abroad + 
             fam_log + 
             house_damage +
             marital_status +
             age +
             start_reconstruct +
             as.factor(district),
           data = df_self))

summary(lm(consult_knowledge ~ abroad + # without covariates
             as.factor(district),
           data = df_consult))

summary(lm(consult_knowledge ~ abroad + 
             fam_log + 
             house_damage +
             marital_status +
             age +
             start_reconstruct +
             as.factor(district),
           data = df_consult))

summary(lm(pa_name_self ~ abroad + # without covariates
             # Author note: this indicator is the most 
             # sensitive to model specification
             as.factor(district),
           data = df_pa))

summary(lm(pa_name_self ~ abroad + 
             fam_log + 
             house_damage +
             marital_status +
             age +
             start_reconstruct +
             as.factor(district),
           data = df_pa))

######################################################
### LPMs with interaction with marriage and abroad ###
######################################################
# We might also want to look at the interaction of the marginal effect of abroad 
# on self_visit moderated by married status, which might be truer to the DGP.
# Since we are interested in the married population, I have recoded married to switch
# the reference population to married 
df_self$marital_status <- dplyr::recode(df_self$marital_status, "1" = 0, "0" = 1)
summary(lm(self_visit ~ abroad + 
             fam_log + 
             house_damage +
             marital_status +
             abroad * marital_status + # interaction term
             age +
             start_reconstruct +
             as.factor(district),
           data = df_self))
# -0.059606 is for non married women (and not stat sig from zero)
# 0.350312 for married women (and stat sig from zero)
df_consult$marital_status <- dplyr::recode(df_consult$marital_status, "1" = 0, "0" = 1)
summary(lm(consult_knowledge ~ abroad + 
             fam_log + 
             house_damage +
             marital_status +
             abroad * marital_status + # interaction term
             age +
             start_reconstruct +
             as.factor(district),
           data = df_consult))
# 0.129468 for non married women (not stat sig from zero)
# 0.123878 for married women (stat sig from zero)
df_pa$marital_status <- dplyr::recode(df_pa$marital_status, "1" = 0, "0" = 1)
summary(lm(pa_name_self ~ abroad + 
             fam_log + 
             house_damage +
             marital_status +
             abroad * marital_status + # interaction term
             age +
             start_reconstruct +
             as.factor(district),
           data = df_pa))
# 0.177108 for non married women (not stat sig from zero)
# 0.097464 for married women (stat sig from zero at 0.05)
# In the data, these statistical associations seem to follow the pattern we would
# expect with an interaction

#####################################################
### Data vis: Visiting a govt official by oneself ###
####################################################

results_self <- as.data.frame(c(obs_abroad_self, 
                                obs_fam_self,
                                obs_house_self,
                                obs_marital_self,
                                obs_age_self,
                                obs_reconstruct_self))
colnames(results_self) <- c("x")

cis_self <- matrix(c(self_ci_abroad,
                       self_ci_fam_log,
                       self_ci_house_damage,
                       self_ci_marital_status,
                       self_ci_age,
                       self_ci_start_reconstruct),
                      byrow = TRUE,
                      ncol = 2)
colnames(cis_self) <- c("low", "high")
df <- cbind(results_self, cis_self)
df$id <- c(6, 5, 4, 3, 2, 1)

## probabilities and marginal effects
p1 <- ggplot2::ggplot(aes(x = x, y = id), data = df) +
  geom_point(stat = "identity", position = "identity") +
  geom_errorbar(aes(xmin = low, xmax = high, width = 0)) +
  labs(x = "", y = "") +
  ggtitle("Visiting a Government Official in Person") +
  geom_vline(xintercept = 0, color = "black", alpha = 0.5, linetype = "longdash") +
  theme_minimal() +
  theme(plot.title = element_text(vjust = 3, size = 20)) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(text = element_text(family = "Times New Roman")) +
  # theme(plot.title = element_text(vjust = 3)) +
  scale_y_discrete(limits = c(1, 2, 3, 4, 5, 6),
                   labels = c("started reconstruction",
                              "age",
                              "marital status",
                              "house damage",
                              "family member (log)",
                              "abroad")) +
  scale_x_continuous(breaks = c(-0.75, -0.50, -0.25, 0, 0.25, 0.50, 0.75))

################################################
### Data vis: Having knowledge of consulting ###
###############################################

results_consult <- as.data.frame(c(obs_abroad_consult, 
                                   obs_fam_consult,
                                   obs_house_consult,
                                   obs_marital_consult,
                                   obs_age_consult,
                                   obs_reconstruct_consult))
colnames(results_consult) <- c("x")

cis_consult <- matrix(c(consult_ci_abroad,
                     consult_ci_fam_log,
                     consult_ci_house_damage,
                     consult_ci_marital_status,
                     consult_ci_age,
                     consult_ci_start_reconstruct),
                   byrow = TRUE,
                   ncol = 2)
colnames(cis_consult) <- c("low", "high")
df <- cbind(results_consult, cis_consult)
df$id <- c(6, 5, 4, 3, 2, 1)

## probabilities and marginal effects
p2 <- ggplot2::ggplot(aes(x = x, y = id), data = df) +
  geom_point(stat = "identity", position = "identity") +
  geom_errorbar(aes(xmin = low, xmax = high, width = 0)) +
  labs(x = "", y = "") +
  ggtitle("Having Knowledge of Where to Consult") +
  geom_vline(xintercept = 0, color = "black", alpha = 0.5, linetype = "longdash") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.50, size = 20)) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(text = element_text(family = "Times New Roman")) +
  scale_y_discrete(limits = c(1, 2, 3, 4, 5, 6), 
                   labels = c("started reconstruction",
                              "age",
                              "marital status",
                              "house damage",
                              "family member (log)",
                              "abroad")) +
  scale_x_continuous(breaks = c(-0.75, -0.50, -0.25, 0, 0.25, 0.50, 0.75))

################################################
### Data vis: Having a PA in one's own name  ###
################################################

results_pa <- as.data.frame(c(obs_abroad_pa, 
                              obs_fam_pa,
                              obs_house_pa,
                              obs_marital_pa,
                              obs_age_pa,
                              obs_reconstruct_pa))
colnames(results_pa) <- c("x")

cis_pa <- matrix(c(pa_ci_abroad,
                        pa_ci_fam_log,
                        pa_ci_house_damage,
                        pa_ci_marital_status,
                        pa_ci_age,
                        pa_ci_start_reconstruct),
                      byrow = TRUE,
                      ncol = 2)
colnames(cis_pa) <- c("low", "high")
df <- cbind(results_pa, cis_pa)
df$id <- c(6, 5, 4, 3, 2, 1)

## probabilities and marginal effects
p3 <- ggplot2::ggplot(aes(x = x, y = id), data = df) +
  geom_point(stat = "identity", position = "identity") +
  geom_errorbar(aes(xmin = low, xmax = high, width = 0)) +
  labs(x = "", y = "") +
  ggtitle("Having Name on Rebuilding Agreement") +
  geom_vline(xintercept = 0, color = "black", alpha = 0.5, linetype = "longdash") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.50, size = 20)) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(axis.title.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 18)) +
  theme(text = element_text(family = "Times New Roman")) +
  scale_y_discrete(limits = c(1, 2, 3, 4, 5, 6), 
                   labels = c("started reconstruction",
                              "age",
                              "marital status",
                              "house damage",
                              "family member (log)",
                              "abroad")) +
  scale_x_continuous(breaks = c(-0.75, -0.50, -0.25, 0, 0.25, 0.50, 0.75))

################################
###    Data vis: Together    ###
################################

all <- gridExtra::arrangeGrob(p2, p1, p3,
                              ncol = 1)
# ggsave(file="ave_pp_three.png", all, dpi=500)


