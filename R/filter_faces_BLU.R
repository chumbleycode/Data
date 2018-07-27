# Item selection
# Stochastic optimization: minimum noise, maximum heterogeniety
# Descriptives: beware interpretation/fitting small n of subjects
# Few subjects for estimating these variance sources: MLM Bayesian
rm(list = ls())

library(tidyverse)
c("foreign",
  "readxl", 
  "GGally", 
  "psych",
  "ggformula",
  "tidybayes",
  "brms") %>%  
  map(library, 
      character.only = T)

d <- read_excel("CFD_Version_2.0.3/CFD_2.0.3_Norming_Data_and_Codebook.xlsx", 
                sheet = "CFD 2.0.3 Norming Data")
d =
  d %>% 
  rename(suit_aj = `Personal suit`) 

d %>%
  filter( Suitability > 0) %>%
  select(Suitability, suit_aj) %>% 
  mutate_all(as.numeric) %>% 
  gf_point(Suitability ~ suit_aj)

d %>%
  filter( suit_aj > 0) %>%
  select(Attractive, Happy, Angry, Dominant, Feminine, Masculine, Trustworthy, Gender) %>% 
  pairs.panels()

subs = 
  list.files("BLU-LAB_pilot_results/morph_raw/",
             full.names = T) %>% 
  str_subset(".csv") %>% 
  array_branch() %>% 
  map(~read.csv(.x ,sep = "\t")) %>% 
  map(as_tibble) #%>% 

# join all subs into one df
all_subs = 
  subs %>% 
  map2_dfr(1:length(subs), 
           ~ cbind(subj_id = .y, .x)) %>%
  as_tibble %>%   
  select(-X) %>%  # meaningless column
  rename(fed = Answer,
         rt = ReactionTime.s.,
         face_id = Identity) 

########################################################
# MARGINALS
########################################################

# OVER ALL SUBJS
all_subs %>% gf_histogram(~fed, bins = 1000) 
all_subs %>% gf_histogram(~rt, bins = 100) %>% gf_lims(x = c(0, 45))
# SUBJ-SPECIFIC
all_subs %>% ggplot(aes(fed)) + stat_ecdf(aes(fed, group = subj_id, color = factor(subj_id))) + ggtitle("Subject-specific choice distributions")
all_subs %>% ggplot(aes(rt))  + stat_ecdf(aes(rt, group = subj_id, color = factor(subj_id))) + ggtitle("Subject-specific RT distributions")

########################################################
# REMOVE SOME SUBJECTS
########################################################

removal = FALSE
if(removal){ 
  all_subs %>%
    filter(fed < 5 | fed > 95)
  
  all_subs =
    all_subs %>%
    filter(!(fed < 5 | fed > 95))  
}

########################################################
# SUMMARIES
########################################################

# subj specific averages
(mu =
   all_subs %>%
   group_by(subj_id) %>%
   summarise(mu_fed = mean(fed),
             mu_rt = mean(rt)))

# face-specific 
(mu_face = 
    all_subs %>% 
    group_by(face_id) %>% 
    summarise(mu_fed = mean(fed),
              mu_rt = mean(rt, na.omit = 1)))

########################################################
# GRAPH FACE-SPECIFIC SUMMARIES
########################################################

# fed response
all_subs %>% 
  gf_violin(fed~face_id) %>% 
  gf_labs(title ="Raw fed for each face, ignoring subjects")


all_subs %>% 
  group_by(face_id, subj_id) %>%
  summarise(mu = mean(fed), 
            sd = sqrt(var(fed)),
            ranger = max(fed) - min(fed) ) %>% 
  ungroup %>% 
  mutate(subj_id = factor(subj_id)) %>% 
  # gf_boxplot(mu~face_id) %>%  gf_jitter(width = .4)
  gf_point(mu~sd, color =~ subj_id) %>% 
  gf_facet_wrap(~face_id) %>%  # %>% 
  gf_lims(x = c(1,25),
          y = c(25,75)) %>% 
  gf_labs(title = "Face and subject specific means and spreads")

# Rank reliable faces (average precision over subjects) AND discriminating
# Y_[sfr] = X_s + Y_f + e_[sfr]
all_subs  =
  all_subs %>% 
  group_by(face_id, subj_id) %>%
  mutate(sd_within_face_subj = sd(fed),
         mu_within_face_subj = mean(fed))   %>% 
  ungroup %>% 
  group_by(face_id) %>% 
  mutate(mean_sd_over_subjs_within_face = mean(sd_within_face_subj),
         sd_mean_over_subjs_within_face = sd(mu_within_face_subj)) %>% # sd of within face variation in mu over subjects
  ungroup %>% 
  arrange(-sd_mean_over_subjs_within_face, mean_sd_over_subjs_within_face ) %>% 
  mutate(reliable = mean_sd_over_subjs_within_face < mean(mean_sd_over_subjs_within_face),
         discriminating = sd_mean_over_subjs_within_face > mean(sd_mean_over_subjs_within_face))

all_subs %>% 
  gf_point(sd_mean_over_subjs_within_face~mean_sd_over_subjs_within_face, 
           shape =~ reliable, 
           size =~ discriminating,
           color =~ face_id)  %>% 
  gf_labs(title = "reliable and discrimating faces")

all_subs %>% 
  mutate(subj_id = factor(subj_id)) %>% 
  mutate(logrt = log(rt)) %>% 
  filter(logrt > 0) %>% # remove outliers
  gf_point(logrt~fed, fill = ~subj_id, color =~ subj_id) %>% 
  gf_density_2d(logrt~fed, fill = ~subj_id, color =~ subj_id) %>% 
  gf_vline(xintercept = mu$mu_fed) %>% 
  gf_hline(yintercept = log(mu$mu_rt)) %>% 
  gf_labs(title = str_c("10 secs = exp(2.3)  RT "))

if(1){
  # analyse with all data, first run, and cherrypicked faces (beware overfitting)
  source("R/brms_with_different_data_subsets.R")
  # Ex. call: different_data_subsets(dater = all_subs)
  # An using purrr
  inn = 
    c(dater = list(all_subs %>% filter(Trial %in% 1:44),
                   all_subs %>% filter(Trial %in% 1:88),
                   all_subs %>% filter(Trial %in% 1:132),
                   all_subs,
                   all_subs %>% filter(reliable == TRUE, discriminating == TRUE)),
      tittle = list( "run 1", "runs 1-2", "runs 1-3", "runs 1-4", "cherry")) 
  out1 = 
    inn[1] %>% 
    map(brms_with_different_data_subsets)
  out2 =
    inn[2] %>% 
    map(brms_with_different_data_subsets)
  out3 = 
    inn[3] %>% 
    map(brms_with_different_data_subsets)
  out4 =
    inn[4] %>% 
    map(brms_with_different_data_subsets)
  out5 = 
    inn[5] %>% 
    map(brms_with_different_data_subsets)
  
  save.image("dat/output_blu.Rdata")
}
load("dat/output1_blu.Rdata")

# out1 = out1 %>% array_branch()
out1$dater1$figs
out2$dater2$figs
out3$dater3$figs
out4$dater4$figs
out5$dater5$figs

########################################################
# READ SUBJECT-LEVEL DATA (ALEX) AND JOIN WITH SUBJECT FED TASK SUMMARIES
########################################################

quest = read_excel("BLU-LAB_pilot_results/BLU_P.xlsx", sheet = 3)
merged = merge(mu, quest, by.x = "subj_id", by.y = "ID")

merged %>% gf_point(mu_fed~ STAI)
cor.test(merged$mu_fed, merged$STAI,    method = "pearson" )

########################################################
# JOIN SUBJECT-LEVEL DATA TO TRIAL-LEVEL DATA
########################################################

questions = read_excel("BLU-LAB_pilot_results/BLU_P.xlsx", sheet = 3) %>% rename(subj_id = ID) 
all_subs =  all_subs %>% left_join(questions, by = "subj_id")

########################################################
# MLM WITH SUBJECT-LEVEL COVARIATES
# BASED ON: source("R/brms_with_different_data_subsets.R")
########################################################

n_iter = 1000 
# GENERAL MODEL
m <- brm(bf(fed ~ -1 + intercept + (1 + STAI |subj_id) + (1| face_id),
            sigma ~ (1|face_id)),
         data = all_subs, 
         family = gaussian(),
         # prior = prior_dep, 
         sample_prior = TRUE, 
         iter = n_iter,
         control = list(adapt_delta = 0.95))
# RESTRICTED MODEL
m_no_subj_variation <- update(m, formula. = ~ . - (1 | subj_id))

# LOO COMPARISON: When comparing models fitted to the same data, the smaller the LOO, the better the fit
loo_evaluating_subj_variation = LOO(m, m_no_subj_variation)

########################################################
# SAME MODEL DIFFERENT PARAMETERIZATION
########################################################

########################################################
# MLM WITH SUBJECT-LEVEL COVARIATES
# BASED ON: source("R/brms_with_different_data_subsets.R")
########################################################

n_iter = 1000 
# GENERAL MODEL
m <- brm(bf(fed ~ -1 + intercept + (1 + STAI |subj_id) + (1| face_id),
            sigma ~ (1|face_id)),
         data = all_subs, 
         family = gaussian(),
         # prior = prior_dep, 
         sample_prior = TRUE, 
         iter = n_iter,
         control = list(adapt_delta = 0.95))
# RESTRICTED MODEL: NO STAI, NO SUBJECT-INTERCEPT?
m_no_subj_STAI <- update(m, formula. = ~ . - (STAI | subj_id))

# LOO COMPARISON: When comparing models fitted to the same data, the smaller the LOO, the better the fit
loo_evaluating_subj_STAI = LOO(m, m_no_subj_variation)

########################################################
# SAME MODEL DIFFERENT PARAMETERIZATION
########################################################

########################################################
# MLM WITH SUBJECT-LEVEL COVARIATES
# BASED ON: source("R/brms_with_different_data_subsets.R")
########################################################

n_iter = 1000 
# GENERAL MODEL
m <- brm(bf(fed ~ (1 + STAI |subj_id) + (1| face_id),
            sigma ~ (1|face_id)),
         data = all_subs, 
         family = gaussian(),
         # prior = prior_dep, 
         sample_prior = TRUE,
         iter = n_iter,
         control = list(adapt_delta = 0.95))
# RESTRICTED MODEL
m_no_subj_variation <- update(m, formula. = ~ . - (1 | subj_id))

# LOO COMPARISON: When comparing models fitted to the same data, the smaller the LOO, the better the fit
loo_evaluating_subj_variation = LOO(m, m_no_subj_variation)
