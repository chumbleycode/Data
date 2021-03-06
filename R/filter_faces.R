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
  array_branch() %>% 
  map(library, 
      character.only = T)

d <- read_excel("CFD Version 2.0.3/CFD 2.0.3 Norming Data and Codebook.xlsx", 
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
  list.files("Pretest_results/",
             full.names = T) %>% 
  str_subset("pretest00") %>% 
  array_branch() %>% 
  map(read.csv) %>% 
  map(as_tibble) #%>% 

# join all subs into one df
all_subs = 
  subs %>% 
  map2_dfr(1:length(subs), 
           ~ cbind(subj_id = .y, .x)) %>%
  as_tibble %>%   
  rename(fed = Answer,
         rt = ReactionTime.s.,
         face_id = Identity) %>% 
  filter( rt > 1 | is.na(rt)) %>% # too quick response (and outlier), but don't exclude those withour RT measurement
  filter(fed > 5, fed < 95)

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

if(0){
# analyse with all data, first run, and cherrypicked faces (beware overfitting)
source("brms_with_different_data_subsets.R")
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

save.image("output.Rdata")
}
load("output.Rdata")

# out1 = out1 %>% array_branch()
out1$dater1$figs
out2$dater2$figs
out3$dater3$figs
out4$dater4$figs
out5$dater5$figs


