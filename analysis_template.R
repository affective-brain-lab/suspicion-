# LOAD DATA SETS
## aggregate data, e.g. demographics
data_meta <- read.csv("./exp1_meta_4param.csv") # adapt path as needed; fr = study 1
data_meta <- read.csv("./exp2_meta_4param.csv") # adapt path as needed; rs = study 2
data_meta <- read.csv("./exp3_meta_4param.csv") # study 3
data_meta <- setDT(data_meta) # make sure the data are of data.table type
setkey(data_meta, "PID")

## trials data
data <- read.csv("./fr_topup_clean_long.csv") # adapt path as needed; fr = study 1
data <- read.csv("./rs_clean_long.csv") # adapt path as needed; rs = study 2
data <- read.csv("C:/Users/Sarah Zheng/dev/simpy-suspicion-model/exp3_data_trials.csv") # study 3
data <- setDT(data) # make sure the data are of data.table type
setkey(data, "PID")

## save participant IDs
unique(data$PID) == unique(data_meta$PID) # SANITY CHECK
uuids <- unique(data$PID)


## PRE-PROCESS DATA
data$n_blue <- as.numeric(data$n_blue)
data$ia_sl_nblu <- data$subject_lied * data$n_blue

data$subject_lost <- ifelse(data$win_lose_tie == "loss", TRUE, FALSE) # convert "win lose tie" to boolean factor variable "subject_lost"
data$outcome_blue <- ifelse(data$outcome == 1, TRUE, FALSE) # convert "outcome" to boolean factor variable indicating whether other participant reported blue, i.e., lying motivation

# ONLY IF USING STUDY 3 DATA:
# data$subject_lost <- ifelse(data$subject_lost == "True", TRUE, FALSE) # ONLY applies to experiment 3 data
# data$subject_lied <- ifelse(data$subject_lied == "True", TRUE, FALSE) # ONLY applies to experiment 3 data
# data$pp_lied <- ifelse(data$pp_lied == "True", TRUE, FALSE) # ONLY applies to experiment 3 data


# DEFINE MODELS TO FIT
params <- c("normed_signed_e_v", 
            "normed_unsigned_e_v",
            "subject_lied", 
            "subject_lost")
models <- define_models(params)
length(models)


# BAYESIAN MODEL AVERAGING PROCEDURE
human_CIs <- bayesian_lmer_95ci(data, models, PARAM_NAMES)
human_CIs$entity <- 'human'
round(human_CIs[, c('mean', 'upper', 'lower', 'sd')], 3)

perfect_CIs <- bayesian_lm_95ci(data, MODELS_PERFECT, PARAM_NAMES)
perfect_CIs$entity <- 'accurate lie detector'
round(perfect_CIs[, c('mean', 'upper', 'lower', 'sd')], 3)

CIs <- rbind(human_CIs, perfect_CIs)

plot_cis <- ggplot(CIs, aes(param, mean, color=entity)) + 
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0) +
  xlab("") +
  ylab(expression(paste("weighted standardised", beta))) +
  # ggtitle("cues driving human vs artificial detector's suspicion (95%-CIs)") +
  scale_x_discrete(labels= c("losing",
                             "lying\n oneself",
                             "signed\n expectation\n violation",
                             "unsigned\n expectation\n violation")) +
  labs(color="") +
  theme_classic() +
  theme(text = element_text(size = 18), legend.position = c(.1, .9))
plot_cis


# COMPUTE SDT METRICS
human_sdt <- compute_sdt_metrics(data, uuids)


# COMPUTE PERFECT DETECTOR'S SDT METRICS
## the following perfect detector estimates are used to construct the perfect_model function
## ONLY add the significant parameters to the model!
round(perfect_CIs[, c('mean', 'upper', 'lower')], 3)

# experiment_n <- 1 # MAKE SURE TO CHANGE THIS TO THE APPROPRIATE EXPERIMENT NUMBER!!!
perfect_sdt <- compute_perfect_sdt_metrics(data, uuids, experiment_n)


# W/LEAVE 1 OUT CROSS VALIDATION
cv_human_CIs <- cv_bayesian_avg_lmer(data, uuids, models, 8)

round(mean(cv_human_CIs[param == 'Signed expectation violation']$mean), 3)
round(mean(cv_human_CIs[param == 'Signed expectation violation']$lower), 3)
round(mean(cv_human_CIs[param == 'Signed expectation violation']$upper), 3)

round(mean(cv_human_CIs[param == 'Unsigned expectation violation']$mean), 3)
round(mean(cv_human_CIs[param == 'Unsigned expectation violation']$lower), 3)
round(mean(cv_human_CIs[param == 'Unsigned expectation violation']$upper), 3)

round(mean(cv_human_CIs[param == 'Losing']$mean), 3)
round(mean(cv_human_CIs[param == 'Losing']$lower), 3)
round(mean(cv_human_CIs[param == 'Losing']$upper), 3)

round(mean(cv_human_CIs[param == 'Lying oneself']$mean), 3)
round(mean(cv_human_CIs[param == 'Lying oneself']$lower), 3)
round(mean(cv_human_CIs[param == 'Lying oneself']$upper), 3)


cv_perfect_CIs <- cv_bayesian_avg_perfect(data, uuids, MODELS_PERFECT, 8)

round(mean(cv_perfect_CIs[param == 'Signed expectation violation']$mean), 3)
round(mean(cv_perfect_CIs[param == 'Signed expectation violation']$lower), 3)
round(mean(cv_perfect_CIs[param == 'Signed expectation violation']$upper), 3)

round(mean(cv_perfect_CIs[param == 'Unsigned expectation violation']$mean), 3)
round(mean(cv_perfect_CIs[param == 'Unsigned expectation violation']$lower), 3)
round(mean(cv_perfect_CIs[param == 'Unsigned expectation violation']$upper), 3)

round(mean(cv_perfect_CIs[param == 'Losing']$mean), 3)
round(mean(cv_perfect_CIs[param == 'Losing']$lower), 3)
round(mean(cv_perfect_CIs[param == 'Losing']$upper), 3)

round(mean(cv_perfect_CIs[param == 'Lying oneself']$mean), 3)
round(mean(cv_perfect_CIs[param == 'Lying oneself']$lower), 3)
round(mean(cv_perfect_CIs[param == 'Lying oneself']$upper), 3)


## COMPARE HUMAN VS. ARTIFICIAL LIE DETECTOR PERFORMANCE
dprime_long <- rbind(data.frame(type='human', dprime=human_sdt$dprime), data.frame(type='artificial', dprime=perfect_sdt$dprime))

plot_dprime_compare <- ggplot(dprime_long, aes(x=type, y=dprime)) +
  geom_boxplot() +
  xlab("") + 
  ylab("Lie detection d'-score") +
  scale_x_discrete(labels = c("Artificial lie detector", "Participants")) +
  theme_classic() +
  theme(text = element_text(size = 24))


# participants for whom each predictor is signifcant
human_fits <- data.table()

for (i in 1:length(uuids)){
  print(i)
  uuid <- uuids[i]
  d <- data[uuid]
  res <- bayesian_lm_95ci(d, MODELS_INDIV, PARAM_NAMES, 8)
  res$uuid <- uuid
  human_fits <- rbindlist(list(human_fits, res))
  print("======================================================")
}

human_fits$sig <- ifelse(((round(human_fits$lower, 3) < 0) & (round(human_fits$upper, 3) > 0)), NA, 1)
human_fits$mean_sigadj <- ifelse(human_fits$sig == 1, human_fits$mean, NA)
setDT(human_fits)

# table(ifelse(((human_fits[human_fits$param == "Lying oneself",]$lower < 0) & (human_fits[human_fits$param == "Lying oneself",]$upper > 0)), 0, 1))
# table(ifelse(((human_fits[human_fits$param == "Signed expectation violation",]$lower < 0) & (human_fits[human_fits$param == "Signed expectation violation",]$upper > 0)), 0, 1))
# table(ifelse(((human_fits[human_fits$param == "Unsigned expectation violation",]$lower < 0) & (human_fits[human_fits$param == "Unsigned expectation violation",]$upper > 0)), 0, 1))
# table(ifelse(((human_fits[human_fits$param == "Losing",]$lower < 0) & (human_fits[human_fits$param == "Losing",]$upper > 0)), 0, 1))


# INDIVIDUAL CUE ESTIMATES
human_betas_ba <- spread(human_fits[,c('uuid', 'param', 'mean_sigadj')], key = "param", value = "mean_sigadj")
setnames(human_betas_ba, c('uuid', 'subject_lostTRUE', 'subject_liedTRUE', 'normed_signed_e_v', 'normed_unsigned_e_v'))
human_betas_ba$entity <- 'human'


# rerun model fits after excluding participants who thought there was no other player
uuids_0others <- read.csv("~/exp123_0others_uuids.csv", sep="")
data <- data[!uuids_0others]
uuids <- unique(data$PID)

human_CIs <- bayesian_lmer_95ci(data, models, PARAM_NAMES, 8)
human_CIs$entity <- 'human'
round(human_CIs[, c('mean', 'upper', 'lower')], 3)


# NUMBER OF PEOPLE WHO NEVER LOST
i <- 0
for(uuid in uuids){
  d <- data[uuid]
  l <- sum(d$subject_lost)
  if (l == 0){
    i <- i + 1}
}

# NUMBER OF PEOPLE WHO NEVER LIED
i <- 0
for(uuid in uuids){
  d <- data[uuid]
  l <- sum(d$subject_lied)
  if (l == 0){
    i <- i + 1}
}

# compute number of true positives (hits) and false positives (false alarms) for overall feel of people's lie detection accuracy
tfp <- data.table()
for (uuid in uuids){
  tmp <- data[uuid]
  tp <- nrow(tmp[(tmp$pp_lied == TRUE) & (tmp$suspicion_rating > .5), ])
  fp <- nrow(tmp[(tmp$pp_lied == FALSE) & (tmp$suspicion_rating > .5), ])
  d <- data.frame(tp=tp, fp=fp, uuid=uuid)
  
  tfp <- rbindlist(list(tfp, d))
}


# TENDENCY TO LIE AS PROPORTION OF TRIALS WITH RANDOM RED CARD SELECTION
# FOR STUDY 1 AND 2:
lie_prop_redpicks <- data.table()
for (uuid in uuids){
  d <- data[uuid]
  lie_prop_redpick <- sum((d$col_picked == -1) & (d$col_reported == 1)) / sum(d$col_picked == -1)
  lie_prop_redpicks <- rbindlist(list(lie_prop_redpicks, data.table(PID=uuid, lie_prop_redpick=lie_prop_redpick)))
}

# FOR STUDY 3:
# data_meta$condition <- ifelse(data_meta$PID %in% uuids_HtL, 'hl', 'lh') ## from file exp3_uuids.R
# data_meta$ed_lev <- data_meta$edlev
# data_meta$gender <- ifelse(data_meta$gender == 1, 0, 1) # recode gender to be 1 = female, 0 = male in Exp 3
# data_meta$lie_prop <- (data_meta$lie_proportion_block_high_reward + data_meta$lie_proportion_block_low_reward)/2
# data_meta$mean_suspicion_rating <- (data_meta$mean_suspicion_rating_block_high_reward + data_meta$mean_suspicion_rating_block_low_reward)/2
# 
# lie_prop_redpicks <- data.table()
# for (uuid in uuids){
#   d <- data[uuid]
#   lie_prop_redpick <- sum((d$random_pick_col == "red") & (d$reported_col == 1)) / sum(d$random_pick_col == 'red')
#   
#   if (data_meta[uuid]$condition == 'hl'){
#     lie_prop_redpick_highstakes <- sum((d[1:30]$random_pick_col == "red") & (d[1:30]$reported_col == 1)) / sum(d[1:30]$random_pick_col == 'red')
#     lie_prop_redpick_lowstakes <- sum((d[31:60]$random_pick_col == "red") & (d[31:60]$reported_col == 1)) / sum(d[31:60]$random_pick_col == 'red')
#   } else {
#     lie_prop_redpick_lowstakes <- sum((d[1:30]$random_pick_col == "red") & (d[1:30]$reported_col == 1)) / sum(d[1:30]$random_pick_col == 'red')
#     lie_prop_redpick_highstakes <- sum((d[31:60]$random_pick_col == "red") & (d[31:60]$reported_col == 1)) / sum(d[31:60]$random_pick_col == 'red')
#     
#   }
#   
#   lie_prop_redpicks <- rbindlist(list(lie_prop_redpicks, data.table(PID=uuid,
#                                                                     lie_prop_redpick=lie_prop_redpick,
#                                                                     lie_prop_redpick_high=lie_prop_redpick_highstakes,
#                                                                     lie_prop_redpick_low=lie_prop_redpick_lowstakes)))
# }


## MASTER AGGREGATE DATA FILE
# data_meta <- data_meta[,c('crt', 'aq', 'rgpts', 'eq', 'PID', 'mean_suspicion_rating', 'age', 'gender', 'ed_lev')] # STUDY 1
# data_meta <- data_meta[,c('crt', 'PID', 'mean_suspicion_rating', 'age', 'gender', 'ed_lev')] # STUDY 2
# data_meta <- data_meta[,c('crt', 'PID', 'mean_suspicion_rating', 'mean_suspicion_rating_block_high_reward', 'mean_suspicion_rating_block_low_reward', 'age', 'gender', 'ed_lev')] # STUDY 3
data_meta <- merge(data_meta, lie_prop_redpicks)
data_meta <- merge(data_meta, human_sdt, by.x = 'PID', by.y = 'uuid')
data_meta <- merge(data_meta, tfp, by.x = 'PID', by.y = 'uuid')
data_meta <- merge(data_meta, human_betas_ba, by.x = 'PID', by.y = 'uuid')
data_meta <- merge(data_meta, perfect_sdt, by.x = 'PID', by.y = 'uuid')
data_meta[is.na(data_meta)] <- 0
data_meta[data_meta==-Inf] <- 0
data_meta[data_meta==Inf] <- 0
str(data_meta)
# write.csv(data_meta, 'exp1_meta_4param.csv')
# write.csv(data_meta, 'exp2_meta_4param.csv')
# write.csv(data_meta, 'exp3_meta_4param.csv')


# STUDY 3 MEDIATION ANALYSIS
# df3 <- data.table()
# for(uuid in uuids) {
#   d <- data[uuid]
#   
#   if (uuid %in% uuids_HtL){
#     d$stake <- c(rep('high', 30), rep('low', 30))
#   } else {
#     d$stake  <- c(rep('low', 30), rep('high', 30))
#   } 
#   
#   df3 <- rbindlist(list(df3, d[,c('PID', 'suspicion_rating', 'subject_lied', 'stake')]))
# }
# 
# summary(lm('suspicion_rating ~ stake', data=df3))
# summary(lm('subject_lied ~ stake', data=df3))
# summary(lm('suspicion_rating ~ stake + subject_lied', data=df3))
# 
# df3$stake1 <- ifelse(df3$stake == 'low', 0, 1)
# summary(lm('suspicion_rating ~ stake1', data=df3))
# summary(lm('subject_lied ~ stake1', data=df3))
# summary(lm('suspicion_rating ~ stake1 + subject_lied', data=df3))


# REGRESSIONS
## FOR STUDY 1 ADD TO ALL REGRESSION FORMULAS: eq+aq+rgpts+
summary(lm('mean_suspicion_rating ~ crt +  
           age + gender + ed_lev +
           lie_prop_redpick', data = data_meta))
lm.beta(lm('mean_suspicion_rating ~ crt +  eq+aq+rgpts+
           age + gender + ed_lev +
           lie_prop_redpick', data = data_meta))

summary(lm('dprime ~ subject_liedTRUE + subject_lostTRUE + normed_signed_e_v + normed_unsigned_e_v +
            crt + 
           age + gender + ed_lev +
           lie_prop_redpick', data = data_meta))
lm.beta(lm('dprime ~ subject_liedTRUE + subject_lostTRUE + normed_signed_e_v + normed_unsigned_e_v +
            crt + 
           age + gender + ed_lev +
           lie_prop_redpick', data = data_meta))

summary(lm('beta ~ subject_liedTRUE + subject_lostTRUE + normed_signed_e_v + normed_unsigned_e_v +
            crt + 
           age + gender + ed_lev +
           lie_prop_redpick', data = data_meta))
lm.beta(lm('beta ~subject_liedTRUE + subject_lostTRUE + normed_signed_e_v + normed_unsigned_e_v +
            crt + 
           age + gender + ed_lev +
           lie_prop_redpick', data = data_meta))

summary(lm('acc ~ subject_liedTRUE + subject_lostTRUE + normed_signed_e_v + normed_unsigned_e_v +
            crt + 
           age + gender + ed_lev +
           lie_prop_redpick', data = data_meta))
lm.beta(lm('acc ~ subject_liedTRUE + subject_lostTRUE + normed_signed_e_v + normed_unsigned_e_v +
            crt + 
           age + gender + ed_lev +
           lie_prop_redpick', data = data_meta))


## BRUNSWIK LENS MODEL CORRELATIONS
data$pp_lied <- as.numeric(data$pp_lied)
data$subject_lied <- as.numeric(data$subject_lied)
data$subject_lost <- as.numeric(data$subject_lost)
data$outcome_blue <- as.numeric(data$outcome_blue)

# ecology = artificial detector: ecological validities
cor.test(data$pp_lied, data$subject_lied)
cor.test(data$pp_lied, data$normed_signed_e_v)
cor.test(data$pp_lied, data$normed_unsigned_e_v)
cor.test(data$pp_lied, data$subject_lost)
cor.test(data$pp_lied, data$outcome_blue)

# human judgement: cue utilisation
cor.test(data$suspicion_rating, data$subject_lied)
cor.test(data$suspicion_rating, data$normed_signed_e_v)
cor.test(data$suspicion_rating, data$normed_unsigned_e_v)
cor.test(data$suspicion_rating, data$subject_lost)
cor.test(data$suspicion_rating, data$outcome_blue)


# CORRELATION MATRIX
data$subject_lost <- ifelse(data$subject_lost == TRUE, 1, 0) 
data$outcome_blue <- ifelse(data$outcome == TRUE, 1, 0)

avg_r <- avg_corr_matrix(data, uuids, PARAM_BI_COMBIS)
colnames(avg_r)[1:6] <- COLNAMES_CORR_MATRIX
lapply(avg_r[,1:length(PARAM_BI_COMBIS)], function(x) {round(mean(x, na.rm=T), 3)})

avg_rp <- avg_corr_matrix_p(data, uuids, PARAM_BI_COMBIS)
colnames(avg_rp)[1:6] <- COLNAMES_CORR_MATRIX
lapply(avg_rp[,1:length(PARAM_BI_COMBIS)], function(x) {round(mean(x, na.rm=T), 3)})


# COMPARE EXP 1 VS. EXP 2 LYING AND SUSPICION
data_meta1 <- read.csv("./exp1_meta_4param.csv") # STUDY 1
data_meta2 <- read.csv("./exp2_meta_4param.csv") # STUDY 2

wilcox.test(data_meta1$lie_prop_redpick, data_meta2$lie_prop_redpick)
wilcox.test(data_meta1$mean_suspicion_rating, data_meta2$mean_suspicion_rating)


# CHECK IF PARTICIPANTS' BELIEF ON THE STUDY PURPOSE RELATES TO THEIR TASK BEHAVIOUR/PERFORMANCE
study_purpose <- read_excel("study_purpose.xlsx")
# FOR EXP 1 or 2:
# tmp <- merge(data_meta, study_purpose, by.x = 'PID', by.y = 'uuid')
# # EXP 3
# tmp <- cbind(data_meta, study_purpose[206:305, 'studypurpose'])

summary(aov(mean_suspicion_rating ~ studypurpose, tmp))
summary(aov(lie_prop_redpick ~ studypurpose, tmp))
summary(aov(dprime ~ studypurpose, tmp))
