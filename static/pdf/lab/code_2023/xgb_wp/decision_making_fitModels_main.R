
source("00_main.R")
source("decision_making_bootstrap_main.R")
# V1_dataset_EP = if (str_detect(V1_model_name_EP, "110")) data_full_110_EP else data_full_EP
V1_dataset_WP = if (str_detect(V1_model_name_WP, "110")) data_full_110_WP else data_full_WP
# ### original model: Burke 2009
# source("../3_model_selection/ols_110/models.R")
# Burke_model_fit = get(paste0("fit_", "lm_Burke_paper_110_1"))(data_full_110_EP)

##############
### Models ###
##############

### BEST VALUE OF A FIRST DOWN MODEL
fit_V1_model_best <- function(model_name, model_type, dataset) {
  if (model_type == "MLR") {
    model_fit = clean_lm(get(paste0("fit_", model_name))(dataset))
  } else if (model_type == "XGB") {
    model_fit = train_xgb(
      xgb_features = get(paste0(model_name, "_features")), 
      train_set = dataset, 
      params = get(paste0(model_name, "_params")), 
      nrounds = get(paste0(model_name, "_nrounds")),  
      catalytic= get(paste0(model_name, "_catalytic")), 
      Regression=str_detect(model_name, "_R_"), 
      BoundedRegression=str_detect(model_name, "_BR_"), 
      wp=str_detect(model_name, "_wp_"), 
      catalytic_model_name = get(paste0(model_name, "_prior_name")) 
    )
  } else {
    stop("need to implement...")
  }
  return(model_fit)
}

### BEST FIELD GOAL PROBABILITY MODEL
fit_fgp_model_best <- function(fg_data) {
  ### impute fake missed field goals for yardlines beyond 50 (which has never been made before)
  fg_data_1 = fg_data %>% bind_rows(tibble(
    fg_made = 0, kq_0_sum_std = 0,
    yardline_100 = sample(51:99, size=500, replace=TRUE)
  ))
  
  fit = glm(fg_made ~  bs(yardline_100, df=5) + kq_0_sum_std, data = fg_data_1, family="binomial")
  clean_lm(fit)
}

### ORIGINAL FIELD GOAL PROBABILITY MODEL
fit_fgp_model_og <- function(fg_data) {
  fit = glm(fg_made ~  bs(yardline_100, df=5), data = fg_data, family="binomial")
  # fit = glm(fg_made ~ bs(yardline_100, df=6), data = fg_data, family="binomial")
  clean_lm(fit)
}

### BEST PUNT EXPECTED NEXT YARDLINE MODEL
fit_punt_eny_model_best <- function(punt_data) {
  fit = lm(next_ydl ~ bs(yardline_100, df=4) + pq_0_sum_std + pq_0_sum_std:yardline_100, data = punt_data)
  clean_lm(fit)
}

### ORIGINAL PUNT EXPECTED NEXT YARDLINE MODEL
fit_punt_eny_model_og <- function(punt_data) {
  fit = lm(next_ydl ~ bs(yardline_100, df=4), data = punt_data)
  # fit = lm(next_ydl ~ bs(yardline_100, df=3), data = punt_data)
  clean_lm(fit)
}

### BEST CONVERSION LOGISTIC REGRESSION MODEL
fit_convp_model_best <- function(go_dataset) {
  fit = glm(convert ~ 
              (down==4):bs(log(ydstogo),4,intercept = FALSE) +
              qbq_ot_0_sum + oq_rot_0_total_sum + dq_dt_0_againstPass_sum + dq_dt_0_againstRun_sum
            ,data=go_dataset, family="binomial") 
  
  # fit = glm(convert ~ 
  #             (down==4):bs(log(ydstogo),4,intercept = FALSE) +
  #             1 +
  #             I( (20-yardline_100)^2 ):as.numeric(yardline_100 <= 20) +
  #             I( (85-yardline_100)^2 ):as.numeric(yardline_100 >= 85) +
  #             qbq_ot_0_sum + oq_rot_0_total_sum + dq_dt_0_againstPass_sum + dq_dt_0_againstRun_sum
  #           ,data=go_dataset, family="binomial") 
  clean_lm(fit)
}

### ORIGINAL CONVERSION LOGISTIC REGRESSION MODEL
fit_convp_model_og <- function(go_dataset) {
  fit = glm(convert ~ 
              (down==4):bs(log(ydstogo),4,intercept = FALSE) +
              1 +
              I( (20-yardline_100)^2 ):as.numeric(yardline_100 <= 20) +
              I( (85-yardline_100)^2 ):as.numeric(yardline_100 >= 85)
            ,data=go_dataset, family="binomial") 
  # fit = glm(convert ~ bs(log(ydstogo),6), data=go_dataset, family="binomial") 
  clean_lm(fit)
}

### COACHS' BASELINE DECISION MODEL
fit_coach_model_best <- function(fourth_down_dataset) {
  fit_xgb_coach(fourth_down_dataset, params_xgb_coach) 
}

###########################
### Bootstrap Functions ###
###########################

get_randomized_clustered_bootstrap_dataset <- function(dataset, wp=TRUE, brp=0.5) {
  ### randomized cluster bootstrap
  
  ### group_by index: `game_id` for WP, `epoch` for EP
  group_name = if (wp) "game_id" else "epoch"
  
  ### sample clusters (given by `group_name`) with replacement
  all_group_ids = sort(unique(dataset[[group_name]]))
  num_resample_cb = round(length(all_group_ids)*brp)
  group_ids_boot = tibble(
    g = sort(sample(all_group_ids, size=num_resample_cb, replace=TRUE))
  ) %>% mutate(ii = 1:n()) 
  group_ids_boot[[group_name]] = group_ids_boot$g
  group_ids_boot = group_ids_boot %>% select(-g)
  df_cb = left_join(group_ids_boot, dataset)
  
  ### within each cluster, sample rows with replacement
  df_rcb = df_cb %>% select(all_of(group_name), ii, row_idx)
  df_rcb_1 = df_rcb %>%
    group_by(ii) %>%
    sample_n(size = n(), replace = TRUE) %>%
    arrange(all_of(group_name),ii,row_idx) %>%
    ungroup() %>%
    left_join(dataset) 
  
  return(df_rcb_2)
}

get_iid_bootstrap_dataset <- function(dataset) {
  ### standard iid bootstrap
  dataset[sort(sample(1:nrow(dataset), replace = TRUE)), ]
}


