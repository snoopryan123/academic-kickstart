
##########################################
### number of bootstrap samples
B = 25

### load MLR and XGBoost models
source("models.R")
source("coach_decision_models.R")

# ### our V1 EP  model
# V1_model_name_EP = "xgb_C_110_s_1_catalytic"
# V1_model_type_EP = ifelse(str_detect(V1_model_name_EP, "mlr"), "MLR", ifelse(str_detect(V1_model_name_EP, "xgb"), "XGB", NA))

### our V1 WP model
# V1_model_name_WP = "xgb_wp_110_6"
V1_model_name_WP = "xgb_wp_110_6_catalytic"
V1_model_type_WP = ifelse(str_detect(V1_model_name_WP, "gam"), "GAM", ifelse(str_detect(V1_model_name_WP, "xgb"), "XGB", NA))
