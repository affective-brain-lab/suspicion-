# Functions to analyse human suspicion responses from the cards task (Zheng et al., 2024)
# Created using R version 4.1.3.

# install.packages("data.table")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("lm.beta")
# install.packages("Rmisc")
# install.packages("BayesFactor")
# install.packages("tidyr")
# install.packages("car")
# install.packages("ggpubr")
# install.packages("cowplot")
# install.packages("corrplot")

library(data.table)
library(lme4)
library(lmerTest)
library(lm.beta)
library(Rmisc)
library(BayesFactor)
library(tidyr)
library(car)
library(ggpubr)
library(cowplot)
library(corrplot)

INCLUDE <- c("PID", # unique subject ID
             
             "normed_signed_e_v", # signed expectation violation, i.e., likelihood that the other player reported red or blue (continuous scale)
             "normed_unsigned_e_v", # surprise, i.e., unsigned expectation violation (continuous scale)
             
             "subject_lied", # whether the subject lied (TRUE) on the given trial or not (FALSE); Boolean var, will be treated as discrete variable
             "subject_lost", # whether the subject won (-1), tied (0) or lost (1) the trial
             
             "suspicion_rating", # human suspicion
             "pp_lied") # perfect lie detector


PARAM_NAMES <- c("normed_signed_e_v", 
            "normed_unsigned_e_v",
            "subject_liedTRUE", 
            "subject_lostTRUE",
            "outcome_blueTRUE")


PARAM_BI_COMBIS <- list(c("normed_signed_e_v", "normed_unsigned_e_v"), 
                        c("normed_signed_e_v", "subject_lied"), 
                        c("normed_signed_e_v", "subject_lost"), 
                        c("normed_unsigned_e_v", "subject_lied"), 
                        c("normed_unsigned_e_v", "subject_lost"), 
                        c("subject_lied", "subject_lost"))


COLNAMES_CORR_MATRIX <- sapply(PARAM_BI_COMBIS, function(x){paste(x[1], x[2], sep="_")})


MODELS_INDIV <- c( "suspicion_rating ~ normed_signed_e_v",
                     "suspicion_rating ~ normed_unsigned_e_v",                                                                                                    
                     "suspicion_rating ~ normed_signed_e_v + normed_unsigned_e_v",                                                            
                     "suspicion_rating ~ subject_lied",                                                                                                                  
                     "suspicion_rating ~ normed_signed_e_v + subject_lied",                                                                          
                     "suspicion_rating ~ normed_unsigned_e_v + subject_lied",                                                                      
                     "suspicion_rating ~ normed_signed_e_v + normed_unsigned_e_v + subject_lied",                              
                     "suspicion_rating ~ subject_lost",                                                                                                                  
                     "suspicion_rating ~ normed_signed_e_v + subject_lost",                                                                          
                     "suspicion_rating ~ normed_unsigned_e_v + subject_lost",                                                                      
                     "suspicion_rating ~ normed_signed_e_v + normed_unsigned_e_v + subject_lost",                              
                     "suspicion_rating ~ subject_lied + subject_lost",                                                                                    
                     "suspicion_rating ~ normed_signed_e_v + subject_lied + subject_lost",                                            
                     "suspicion_rating ~ normed_unsigned_e_v + subject_lied + subject_lost",                                        
                     "suspicion_rating ~ normed_signed_e_v + normed_unsigned_e_v + subject_lied + subject_lost")

MODELS_PERFECT <- c( "pp_lied ~ normed_signed_e_v",
                     "pp_lied ~ normed_unsigned_e_v",                                                                                                    
                     "pp_lied ~ normed_signed_e_v + normed_unsigned_e_v",                                                            
                     "pp_lied ~ subject_lied",                                                                                                                  
                     "pp_lied ~ normed_signed_e_v + subject_lied",                                                                          
                     "pp_lied ~ normed_unsigned_e_v + subject_lied",                                                                      
                     "pp_lied ~ normed_signed_e_v + normed_unsigned_e_v + subject_lied",                              
                     "pp_lied ~ subject_lost",                                                                                                                  
                     "pp_lied ~ normed_signed_e_v + subject_lost",                                                                          
                     "pp_lied ~ normed_unsigned_e_v + subject_lost",                                                                      
                     "pp_lied ~ normed_signed_e_v + normed_unsigned_e_v + subject_lost",                              
                     "pp_lied ~ subject_lied + subject_lost",                                                                                    
                     "pp_lied ~ normed_signed_e_v + subject_lied + subject_lost",                                            
                     "pp_lied ~ normed_unsigned_e_v + subject_lied + subject_lost",                                        
                     "pp_lied ~ normed_signed_e_v + normed_unsigned_e_v + subject_lied + subject_lost")


define_models <- function(params){## prepare binary grid to get all possible combinations of parameters
  params_grid <- expand.grid(0:1, 0:1, 0:1, 0:1) # needs to correspond to number of unique parameters
  names(params_grid) <- params # add parameters as column names
  
  ## check if every row has at least one parameter
  rowSums(params_grid)
  params_grid <- params_grid[-1, ] # first one has 0 parameters, so drop it
  params_combi <- apply(params_grid, 1, function(i) which(i == 1)) # get list of parameters in each defined model
  
  ## write all model fomrulas in lmer format and add to list
  models <- c()
  for (combi in params_combi) {
    model <- paste("suspicion_rating ~", paste0(names(combi), collapse=' + '), "+ (1 +", paste0(names(combi), collapse=' + '), "| PID)")
    models <- c(models, model)
  }
  
  return(models)
}



 lmer_fit <- function(data, models) {
  # prepare empty data tables to append to
  fit_estimates_std <- data.table()
  fit_result <- data.table()
  
  for (i in 1:length(models)){
    model <- models[i] # get model formula from list of models
    print(paste0(i, " ", model)) # to check how far into the process we are when running the function
    
    tryCatch(
      {
        fit <- lmer(model, data = data, REML = FALSE, control=lmerControl(optimizer="bobyqa")) # fit linear mixed-effects model according to formula
      },
      warning=function(w) {
        print(w)
      }
    )
    
    # compute standardised parameter estimates
    sdy <- sd(attr(fit, "resp")$y)
    sdx <- apply(attr(fit, "pp")$X, 2, sd)
    sc <- fixef(fit)*sdx/sdy
    std_estimates <- data.frame(t(sc))
    
    fit_estimates_std <- rbindlist(list(fit_estimates_std, std_estimates), fill = TRUE) # add the std. estimates to the data.table
    
    
    # get overall fit indicators
    r <- data.table(model=model,
                    BIC=BIC(fit),
                    AIC=AIC(fit))
    
    fit_result <- rbindlist(list(fit_result, r))
  }
  
  return(cbind(fit_result, fit_estimates_std)) # combine all output before returning
}


BIC_weights <- function(BIC_array) {
  tmp_delta_bics <- abs(BIC_array - min(BIC_array))
  
  return(exp(-.5*tmp_delta_bics)/sum(exp(-.5*tmp_delta_bics)))
}


bayesian_lmer_95ci <- function(data, models, param_names){
  n_models_w_parameter <- round(length(models) / 2)
  
  lmer_fits <- lmer_fit(data, models)
  lmer_fits$BIC_weights <- BIC_weights(lmer_fits$BIC)

  # BIC MODEL PROBABILITY WEGIHTS #
  tmp_BICweighted_params <- lmer_fits[, ..param_names] * lmer_fits$BIC_weights
  weighted_factors <- apply(tmp_BICweighted_params, MARGIN = 2, function(x) { sum(x, na.rm = TRUE)}) 
  weighted_factors_sd <- apply(tmp_BICweighted_params, MARGIN = 2, function(x) { sd(x, na.rm = TRUE)}) 
  
  # 95%-CIs
  ci_ub <- weighted_factors + 1.96 * (weighted_factors_sd / sqrt(n_models_w_parameter))
  ci_lb <- weighted_factors - 1.96 * (weighted_factors_sd / sqrt(n_models_w_parameter))
  
  human_CIs <- data.frame(mean=weighted_factors, sd=weighted_factors_sd, upper=ci_ub, lower=ci_lb)
  # human_CIs$param <- c("signed expectation violation", "unsigned expectation violation", "lying oneself", "losing")
  # human_CIs$param <- factor(human_CIs$param)
  
  return(human_CIs)
}


lm_fit <- function(data, models) {
  # prepare empty data tables to append to
  fit_estimates_std <- data.table()
  fit_result <- data.table()
  
  for (i in 1:length(models)){
    model <- models[i] # get model formula from list of models
    print(paste0(i, " ", model)) # to check how far into the process we are when running the function
    
    tryCatch(
      {
        fit <- lm(model, data = data)
      }, warning=function(w) {
        print(w)
      }
    )
    # get standardised beta coefficients)
    coefs <- t(data.frame(lm.beta(fit)$standardized.coefficients)) 
    fit_estimates_std <- rbindlist(list(fit_estimates_std, data.table(coefs)), fill = T)

    # get overall fit indicators
    r <- data.table(model=model,
                    BIC=BIC(fit),
                    AIC=AIC(fit),
                    R2=summary(fit)$r.squared)
    fit_result <- rbindlist(list(fit_result, r))
  }
  return(cbind(fit_result, fit_estimates_std)) # combine all output before returning ; fit_estimates_std ; fit_estimates_std
}


bayesian_lm_95ci <- function(data, models, param_names, n_models_w_parameter){
  lm_fits <- lm_fit(data, models)
  lm_fits$BIC_weights <- BIC_weights(lm_fits$BIC)

  # BIC MODEL PROBABILITY WEGIHTS #
  tmp_BICweighted_params <- lm_fits[, ..param_names] * lm_fits$BIC_weights
  weighted_factors <- apply(tmp_BICweighted_params, MARGIN = 2, function(x) { sum(x, na.rm = TRUE)}) 
  weighted_factors_sd <- apply(tmp_BICweighted_params, MARGIN = 2, function(x) { sd(x, na.rm = TRUE)}) 
  weighted_factors_se <- weighted_factors_sd / sqrt(n_models_w_parameter)
  
  # 95%-CIs
  ci_ub <- weighted_factors + 1.96 * weighted_factors_se
  ci_lb <- weighted_factors - 1.96 * weighted_factors_se
  
  CIs <- data.frame(mean=weighted_factors, sd=weighted_factors_sd, se=weighted_factors_se, upper=ci_ub, lower=ci_lb)
  # CIs$param <- c("signed expectation violation", "unsigned expectation violation", "lying oneself", "losing")
  # CIs$param <- factor(CIs$param)
  
  return(CIs)
}


lm_fit_indiv_std <- function(data, uuids) {
  winning_formula <- "suspicion_rating ~ normed_signed_e_v + normed_unsigned_e_v + subject_lied + subject_lost"
  winning_formula_noloss <- "suspicion_rating ~ normed_signed_e_v + normed_unsigned_e_v + subject_lied"
  winning_formula_nolies <- "suspicion_rating ~ normed_signed_e_v + normed_unsigned_e_v + subject_lost"
  
  result <- data.table()
  
  for (i in 1:length(uuids)){
    uuid <- uuids[i]
    
    print(paste(i, uuid))
    
    data_subj <- data[uuid]
    
    formula <- ifelse(sum(data_subj$subject_lied) == 0, winning_formula_nolies, 
                      ifelse(sum(data_subj$subject_lost) == 0, winning_formula_noloss, winning_formula))
    
    data_subj$subject_lost <- as.factor(data_subj$subject_lost)
    data_subj$subject_lied <- as.factor(data_subj$subject_lied)
    
    fit <- lm(formula, data = data_subj) # fit winning model
    coefs <- t(data.frame(lm.beta(fit)$standardized.coefficients)) # get standardised beta coefficients)
    coefs <- data.table(coefs)
    coefs$PID <- uuid
    result <- rbindlist(list(result, coefs), fill = TRUE) # add to list of individual fit results
  }
  return(result)
}


cv_bayesian_avg_lmer <- function(data, uuids, models, n_models_w_parameter){
  cv_bay_model_ci <- data.table()
  
  for (i in 1:length(uuids)){
    print(i)
    uuid <- uuids[i]
    tmp <- data[data$PID != uuid, ]
    
    lmer_fits <- lmer_fit(tmp, models)
    lmer_fits$BIC_weights <- BIC_weights(lmer_fits$BIC)
    
    # BIC MODEL PROBABILITY WEGIHTS #
    params <- c("normed_signed_e_v", "normed_unsigned_e_v", "subject_liedTRUE", "subject_lostTRUE")
    weighted_params <- lmer_fits[, ..params] * lmer_fits$BIC_weights
    weighted_factors_avg <- apply(weighted_params, MARGIN = 2, function(x) { sum(x, na.rm = TRUE)}) 
    weighted_factors_sd <- apply(weighted_params, MARGIN = 2, function(x) { sd(x, na.rm = TRUE)}) 
    
    # 95%-CIs
    ci_ub <- weighted_factors_avg + 1.96 * (weighted_factors_sd / sqrt(n_models_w_parameter))
    ci_lb <- weighted_factors_avg - 1.96 * (weighted_factors_sd / sqrt(n_models_w_parameter))
    
    human_CIs <- data.frame(excluded=uuid, mean=weighted_factors_avg, upper=ci_ub, lower=ci_lb)
    human_CIs$param <- c("signed expectation violation", "unsigned expectation violation", "lying oneself", "losing")
    human_CIs$param <- factor(human_CIs$param)
    
    cv_bay_model_ci <- rbindlist(list(human_CIs, cv_bay_model_ci))
    
    print("========================================================================")
  }
  
  return(cv_bay_model_ci)
}


cv_bayesian_avg_perfect <- function(data, uuids, models, n_models_w_parameter){
  cv_bay_model_ci_perfect <- data.table()
    
  for (i in 1:length(uuids)){
    print(i)
    uuid <- uuids[i]
    tmp <- data[data$PID != uuid, ]
    
    perfect_detector_fit <- lm_fit(tmp, models)
    perfect_detector_fit$BIC_weights <- BIC_weights(perfect_detector_fit$BIC)
    
    params <- c("normed_signed_e_v", "normed_unsigned_e_v", "subject_liedTRUE", "subject_lostTRUE")
    perfect_detector_weighted_factors <- perfect_detector_fit[, ..params] * perfect_detector_fit$BIC_weights
    perfect_detector_weighted_factors_avg <- apply(perfect_detector_weighted_factors, MARGIN = 2, function(x) {sum(x, na.rm=T)})
    perfect_detector_weighted_factors_sd <- apply(perfect_detector_weighted_factors, MARGIN = 2, function(x) {sd(x, na.rm=T)})
    
    # 95%-CI perfect detector
    perfect_ci_ub <- perfect_detector_weighted_factors_avg + 1.96 * (perfect_detector_weighted_factors_sd / sqrt(n_models_w_parameter))
    perfect_ci_lb <- perfect_detector_weighted_factors_avg - 1.96 * (perfect_detector_weighted_factors_sd / sqrt(n_models_w_parameter))
    
    perfect_CIs <- data.frame(excluded=uuid, mean=perfect_detector_weighted_factors_avg, upper=perfect_ci_ub, lower=perfect_ci_lb)
    perfect_CIs$param <- c("signed expectation violation", "unsigned expectation violation", "lying oneself", "losing")
    perfect_CIs$param <- factor(perfect_CIs$param)
    
    cv_bay_model_ci_perfect <- rbindlist(list(perfect_CIs, cv_bay_model_ci_perfect))
    
    print("========================================================================")
  }
  
  return(cv_bay_model_ci_perfect)
}


compute_sdt_metrics <- function(data, uuids){
  df_sdt <- data.table()
  
  for (uuid in uuids){
    tmp <- data[uuid]
    
    n_hit <- sum((tmp$suspicion_rating > .5) & (tmp$pp_lied == TRUE))
    n_targets <- sum(tmp$pp_lied == TRUE)
    hit_rate <- n_hit / n_targets
    
    n_fa <- sum((tmp$suspicion_rating > .5) & (tmp$pp_lied == FALSE))
    n_nolie <- sum(tmp$pp_lied == FALSE)
    fa_rate <- n_fa / n_nolie
    
    # dprime
    dprime <- qnorm(hit_rate) - qnorm(fa_rate)
    
    # beta
    zhr <- qnorm(hit_rate)
    zfar <- qnorm(fa_rate)
    beta <- exp(-zhr * zhr / 2 + zfar * zfar / 2)
    
    n_lied_self <- sum(tmp$subject_lied)
    
    # overall accuracy
    tmp$y_pred <- ifelse(tmp$suspicion_rating > .5, TRUE, FALSE)
    acc <- sum(tmp$y_pred == tmp$pp_lied)/nrow(tmp)
    
    sdt <- data.frame(uuid=uuid, dprime=dprime, beta=beta, hitrate=hit_rate, farate=fa_rate, acc=acc, 
                      n_lied_self=n_lied_self)
    
    df_sdt <- rbindlist(list(sdt, df_sdt))
  }
  setkey(df_sdt, 'uuid')
  
  return(df_sdt)
}


perfect_model <- function(x, experiment_n){
  if (experiment_n == '3') {
    return(0.305 * x$normed_signed_e_v + 0.336 * x$normed_unsigned_e_v + 0.025 * x$subject_lost)
  }
  if (experiment_n == '2') {
    return(0.329 * x$normed_signed_e_v + 0.351 * x$normed_unsigned_e_v)
  }
  else {
    return(0.295 * x$normed_signed_e_v + 0.313 * x$normed_unsigned_e_v)
  }
}


compute_perfect_sdt_metrics <- function(data, uuids, experiment_n){
  df_sdt <- data.table()
  
  for (uuid in uuids){
    tmp <- data[uuid]
    
    tmp$y_pred <- ifelse(perfect_model(tmp, experiment_n) >= .5, TRUE, FALSE)

    n_hit <- sum((tmp$y_pred == TRUE) & (tmp$pp_lied == TRUE))
    n_targets <- sum(tmp$pp_lied == TRUE)
    hit_rate <- n_hit / n_targets
    
    n_fa <- sum((tmp$y_pred == TRUE) & (tmp$pp_lied == FALSE))
    n_nolie <- sum(tmp$pp_lied == FALSE)
    fa_rate <- n_fa / n_nolie
    
    # dprime
    dprime <- qnorm(hit_rate) - qnorm(fa_rate)
    
    # beta
    zhr <- qnorm(hit_rate)
    zfar <- qnorm(fa_rate)
    beta <- exp(-zhr * zhr / 2 + zfar * zfar / 2)
    
    # overall accuracy
    acc <- sum(tmp$y_pred == tmp$pp_lied)/nrow(tmp)
    
    sdt <- data.frame(uuid=uuid, dprime_art=dprime, beta_art=beta, hitrate_art=hit_rate, farate_art=fa_rate, acc_art=acc)
    
    df_sdt <- rbindlist(list(sdt, df_sdt))
  }
  setkey(df_sdt, 'uuid')
  
  return(df_sdt)
}


plot_partial <- function(lm1, lm2, lm1_dep, lm2_dep) {
  resid.1 <- resid(lm1)
  resid.2 <- resid(lm2)
  
  tmp <- data.frame(resid.1, resid.2)
  
  plot <- ggplot(data=tmp, aes(x=resid.2, y=resid.1)) +
    geom_point() + 
    geom_smooth(method=lm, color="darkgoldenrod") +
    theme_bw() + theme(text = element_text(size=28), panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
    xlab(lm1_dep) +
    ylab(lm2_dep)
  return(plot)
}


avg_corr_matrix <- function(data, uuids, param_combis){
  master_corrs <- data.table()
  
  for (uuid in uuids) {
    data_single <- data[uuid]
    data_single$subject_lied <- as.numeric(data_single$subject_lied)
    data_single$subject_lost <- as.numeric(data_single$subject_lost)
    
    corrs <- c()
    for (combi in param_combis) {
      subset <- data_single[, ..combi]
      corr <- cor.test(subset[,1][[1]], subset[,2][[1]])$estimate
      corrs <- c(corrs, corr)
    }
    corrs <- data.frame(t(corrs))
    corrs$PID <- uuid
    
    master_corrs <- rbindlist(list(corrs, master_corrs))
  }
  return(master_corrs)
}

avg_corr_matrix_p <- function(data, uuids, param_combis){
  master_corrs <- data.table()
  
  for (uuid in uuids) {
    data_single <- data[uuid]
    data_single$subject_lied <- as.numeric(data_single$subject_lied)
    data_single$subject_lost <- as.numeric(data_single$subject_lost)
    
    corrs <- c()
    for (combi in param_combis) {
      subset <- data_single[, ..combi]
      corr <- cor.test(subset[,1][[1]], subset[,2][[1]])$p.value
      corrs <- c(corrs, corr)
    }
    corrs <- data.frame(t(corrs))
    corrs$PID <- uuid
    
    master_corrs <- rbindlist(list(corrs, master_corrs))
  }
  return(master_corrs)
}
