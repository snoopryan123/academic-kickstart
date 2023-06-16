
##################
WP = TRUE
source("1_train_test_split.R") 
source("models.R")
##################

##########################################################
### Fit the Tree Classification Win Probability Models ### 
##########################################################

#######################################################
print(""); print(xgb_wp_110_6_model_name); print("");
xgb_wp_110_6 = train_xgb(xgb_wp_110_6_features, train_set_110, xgb_wp_110_6_params, xgb_wp_110_6_nrounds, watchSet=test_set_110, catalytic=xgb_wp_110_6_catalytic, wp=TRUE)
logloss_xgb_wp_110_6 = last(xgb_wp_110_6$evaluation_log$validation_logloss)
grouped_loss_xgb_wp_110_6 = compute_loss_by_time_bin(xgb_wp_110_6, test_set_110, xgb_wp_110_6_model_name, xgb_features=xgb_wp_110_6_features, xgb_wp=TRUE)

print(""); print(xgb_wp_Baldwin_model_name); print("");
xgb_wp_Baldwin = train_xgb(xgb_wp_Baldwin_features, train_set, xgb_wp_Baldwin_params, xgb_wp_Baldwin_nrounds, watchSet=test_set_110, catalytic=xgb_wp_Baldwin_catalytic, wp=TRUE)
logloss_xgb_wp_Baldwin = last(xgb_wp_Baldwin$evaluation_log$validation_logloss)

########################################################################################

results_wp = tibble(
  model = c(
    xgb_wp_110_6_model_name,
    xgb_wp_Baldwin_model_name
  ),
  logloss = c(
    logloss_xgb_wp_110_6,
    logloss_xgb_wp_Baldwin
  )
) %>% arrange(logloss)
print(data.frame(results_wp))

# write_csv(results_wp, paste0("test_results_wp.csv"))
