brms_with_different_data_subsets = function(dater, tittle = "waa"){
  # inference
 # browser() 
  # prior
  n_iter = 10000 
  # n_iter = 1000
  # update
  m <- brm(bf(fed ~ -1 + intercept + (1 |subj_id) + (1| face_id),
              sigma ~ (1|face_id)),
           data = dater, 
           family = gaussian(),
           # prior = prior_dep, 
           sample_prior = TRUE, 
           iter = n_iter,
           control = list(adapt_delta = 0.95))
  
  m_no_subj_variation <- update(m, formula. = ~ . - (1 | subj_id))
  
  ########################################################
  # LOO
  # When comparing models fitted to the same data, the smaller the LOO, the better the fit
  ########################################################
  
  loo_evaluating_subj_variation = LOO(m, m_no_subj_variation)
  
  # m %>% parameters()
  
  tidied = 
    m %>% 
    spread_samples(r_subj_id[subj,],
                   r_face_id[face_id,],
                   b_intercept) %>% 
    ungroup
 
  # Plot the group-level parameters: subject and face 
  a = 
    tidied %>% 
    mutate(subj_total = r_subj_id + b_intercept,
           subj = factor(subj)) %>% 
    gf_density(~subj_total, fill =~subj) %>% 
    gf_vline(xintercept = mu$mu_fed) %>% 
    gf_labs(title = str_c("Shrunk subject effects: " , tittle ))
  
  b = 
    tidied %>% 
    mutate(total_r_face_id = r_face_id + b_intercept) %>% 
    gf_density(~total_r_face_id, fill =~ face_id) %>% 
    # gf_vline(xintercept = mu_face$mu_fed) %>% 
    gf_labs(title = str_c("Shrunk face effects: " , tittle ))
  
  out = list(figs = ggmatrix(list(a,b), 2,1, title = tittle), 
             m = m,
             loo_evaluating_subj_variation = loo_evaluating_subj_variation)
}
