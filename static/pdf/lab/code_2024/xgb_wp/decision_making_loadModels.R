
### make sure to, before loading this file, specify the number B of bootstrap models to load

WILL_EVAL_SE_TODAY = TRUE

V1_model_fitList_boot <- list()
V1_wp_model_fitList_boot <- list()
fg_model_fitList_boot <- list()
punt_model_fitList_boot <- list()
go_model_fitList_boot <- list()
for (b in 0:B) {
  if (WILL_EVAL_SE_TODAY | b == 0) {
    print(paste0("loading boostrap b = ", b, " of B = ", B))
    
    # ### bootstrapped EP models
    # if (V1_model_type_EP == "MLR") {
    #   V1_model_fitList_boot[[b+1]] = load_lm(paste0("fitted_models/", paste0(V1_model_name_EP, "_b", b), ".rds")) 
    # } else if (V1_model_type_EP == "XGB") {
    #   V1_model_fitList_boot[[b+1]] = xgb.load(paste0("fitted_models/", paste0(V1_model_name_EP, "_b", b), ".rds"))
    # } 
    ### bootstrapped WP models
    V1_wp_model_fitList_boot[[b+1]] = xgb.load(paste0("fitted_models/", paste0(V1_model_name_WP, "_b", b), ".rds"))
    
    # ### bootstrapped FG,Go,Punt models
    # fg_model_fitList_boot[[b+1]] = load_lm(paste0("fitted_models/", paste0("fg_model_b",b), ".rds")) 
    # punt_model_fitList_boot[[b+1]] = load_lm(paste0("fitted_models/", paste0("punt_model_b",b), ".rds")) 
    # go_model_fitList_boot[[b+1]] = load_lm(paste0("fitted_models/", paste0("go_model_b",b), ".rds")) 
    
    ### don't bootstrap the FG,Go,Punt models
    fg_model_fitList_boot[[b+1]] = load_lm(paste0("fitted_models/", paste0("fg_model_b",0), ".rds")) 
    punt_model_fitList_boot[[b+1]] = load_lm(paste0("fitted_models/", paste0("punt_model_b",0), ".rds")) 
    go_model_fitList_boot[[b+1]] = load_lm(paste0("fitted_models/", paste0("go_model_b",0), ".rds")) 
  }
}
# V1_model_obs = V1_model_fitList_boot[[1]]
V1_wp_model_obs = V1_wp_model_fitList_boot[[1]]
fg_model_obs = fg_model_fitList_boot[[1]]
punt_model_obs = punt_model_fitList_boot[[1]]
go_model_obs = go_model_fitList_boot[[1]]

# V1_model_og = load_lm(paste0("fitted_models/", "Burke_model_fit_og", ".rds")) 
# fg_model_og = load_lm(paste0("fitted_models/", "fg_model_b0", ".rds")) 
# punt_model_og = load_lm(paste0("fitted_models/", "punt_model_b0", ".rds")) 
# go_model_og = load_lm(paste0("fitted_models/", "go_model_b0", ".rds")) 
coach_model = xgb.load(paste0("fitted_models/", "coach_model_b0.rds"))
# # coach_fg_model = load_lm(paste0("fitted_models/", "coach_fg_model_b0.rds"))
# # coach_punt_model = load_lm(paste0("fitted_models/", "coach_punt_model_b0.rds"))


