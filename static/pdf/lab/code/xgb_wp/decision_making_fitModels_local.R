
source("decision_making_fitModels_main.R")

#################################################################################
### Fit all models, inlcuding the bootstrapped models, to get Standard Errors ###
#################################################################################

for (b in 0:B) {
  print(paste0("boostrap b = ", b, " of B = ", B))
  set.seed(b + 10000)
  
  if (b == 0) { ### fit models on fully observed data
    fg_df_b = fg_df
    punt_df_b = punt_df
    go_df_b = go_df
    all_fourth_downs_b = all_fourth_downs
    # V1_dataset_EP_b = V1_dataset_EP
    V1_dataset_WP_b = V1_dataset_WP
    
    # ### fit "OG" models
    # save_lm(Burke_model_fit, paste0("fitted_models/", "Burke_model_fit_og", ".rds"))
    save_lm(fit_fgp_model_og(fg_df_b), paste0("fitted_models/", "fg_model", "_b", b, "_og", ".rds"))
    save_lm(fit_punt_eny_model_og(punt_df_b), paste0("fitted_models/", "punt_model", "_b", b, "_og", ".rds"))
    save_lm(fit_convp_model_og(go_df_b), paste0("fitted_models/", "go_model", "_b", b, "_og", ".rds"))

    ### fit coachs' baseline decision frequency models
    coach_model_b0 = fit_coach_model_best(all_fourth_downs_b)
    xgb.save(coach_model_b0, paste0("fitted_models/", "coach_model", "_b", b, ".rds"))
    ##### save_lm(fit_coach_fg_model_best(all_fourth_downs_b), paste0("fitted_models/", "coach_fg_model", "_b", b, ".rds"))
    ##### save_lm(fit_coach_punt_model_best(all_fourth_downs_b), paste0("fitted_models/", "coach_punt_model", "_b", b, ".rds"))
    
  } else {
    fg_df_b = get_iid_bootstrap_dataset(fg_df) ### bootstrap FG dataset ###
    punt_df_b = get_iid_bootstrap_dataset(punt_df) ### bootstrap Punt dataset ###
    go_df_b = get_iid_bootstrap_dataset(go_df) ### bootstrap Convert dataset ###
    # V1_dataset_EP_b = get_randomized_clustered_bootstrap_dataset(V1_dataset_EP, wp=FALSE) ### epoch randomized clustered bootstrap
    V1_dataset_WP_b = get_randomized_clustered_bootstrap_dataset(V1_dataset_WP, wp=TRUE) ### game randomized clustered bootstrap
    
  }
  
  ### FIT bootstrap V1 WP model ###
  model_fit_b = fit_V1_model_best(V1_model_name_WP, V1_model_type_WP, V1_dataset_WP_b)
  model_wp_filename = paste0("fitted_models/", V1_model_name_WP, "_b", b, ".rds")
  xgb.save(model_fit_b, model_wp_filename) 
  
  # ### FIT bootstrap V1 EP model ###
  # model_fit_b = fit_V1_model_best(V1_model_name_EP, V1_model_type_EP, V1_dataset_EP_b)
  # model_ep_filename = paste0("fitted_models/", V1_model_name_EP, "_b", b, ".rds")
  # if (V1_model_type_EP == "MLR") {
  #   save_lm(model_fit_b, model_ep_filename)
  # } else if (V1_model_type_EP == "XGB") {
  #   xgb.save(model_fit_b, model_ep_filename)
  # }

  # ### FIT bootstrap FG Model ###
  # fg_model_fit_b = fit_fgp_model_best(fg_df_b)
  # save_lm(fg_model_fit_b, paste0("fitted_models/", "fg_model", "_b", b, ".rds"))
  # 
  # ### FIT bootstrap Punt Model ###
  # punt_model_fit_b = fit_punt_eny_model_best(punt_df_b)
  # save_lm(punt_model_fit_b, paste0("fitted_models/", "punt_model", "_b", b, ".rds"))
  # 
  # ### FIT bootstrap Convert Model ###
  # go_model_fit_b = fit_convp_model_best(go_df_b)
  # save_lm(go_model_fit_b, paste0("fitted_models/", "go_model", "_b", b, ".rds"))
}

#######################################################################################################3

# ######################################
# ### Plot Bootstrap Standard Errors ###
# ######################################
# 
# source("decision_making_loadModels.R")
# 
# #####################################
# ### Plot FG, PUNT, CONVERT Models ###
# #####################################
# 
# plot_fg_model = plot_fg_prob_by_kq(fg_model_obs)
# ggsave("plot_SE/plot_fg_model.png", plot_fg_model, width=8, height=6)
# 
# plot_punt_model = plot_punt_eny_by_pq(punt_model_obs)
# ggsave("plot_SE/plot_punt_model.png", plot_punt_model, width=8, height=6)

