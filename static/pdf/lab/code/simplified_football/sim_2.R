
source("0_clean_lm.R")

args <- commandArgs(trailingOnly = TRUE)
SIM_NUM = as.numeric(args[1])
HYPERPARAM_COMBO_IDX = as.numeric(args[2])
set.seed(23748 + SIM_NUM*143)
grid_size = 40 ### num param combos to try for XGBoost tuning
RETUNE_XGB = FALSE
DO_BOOTSTRAP_ANALYSIS = TRUE #FIXME
NUM_WP_ACTUAL_BINS = 50 #10
# B=10; grid_size=2; #FIXME # for testing
#### B = if (exists(args[2])) as.numeric(args[2]) else 100 ### default 100 bootstrap samples

#############################
### Simulation Parameters ###
#############################

# length(unique(data_full_110_WP$game_id)) # actual num. games == 4101
# nrow(data_full_110_WP) / length(unique(data_full_110_WP$game_id)) # actual number of first down plays per game == 53
# nrow(data_full_WP) / length(unique(data_full_WP$game_id)) # actual number of plays per game == 125
# data_full_110_WP %>% group_by(game_id, home_team, drive) %>% summarise() %>% summarise(count = n(), .groups="drop") %>% summarise(avg_drives_per_game = mean(count)) ### 21.69 drives (possessions) per game
# data_full_110_WP %>% group_by(game_id, epoch) %>% summarise() %>% summarise(count = n(), .groups="drop") %>% summarise(avg_epochs_per_game = mean(count)) ### 11.6 epochs per game
# data_full_WP %>% group_by(game_id, home_team, drive) %>% summarise(num_plays_per_poss = n(), .groups="drop") %>% summarise(num_plays_per_poss = mean(num_plays_per_poss)) ### 5.6 number of plays per possession

######################################
### G == num games
### N == num plays per game
### K == num plays to keep per game
######################################

source("sim_2_main.R")

sim_str = paste0("sim2_idx", SIM_NUM, "_G", G, "_N", N, "_K", K, "_L", L)
print(sim_str)
print(paste0("***** sim #", SIM_NUM, " and B = ", B, " bootstrap samples *****"))

############################
### True Win Probability ###
############################

WP_true <- array(dim=c(1+N+1,L-1,MAX_TD_SURPLUS*2+1))
dimnames(WP_true)[[1]] <- paste0("n=", 0:(N+1))
dimnames(WP_true)[[2]] <- paste0("x=", 1:(L-1))
dimnames(WP_true)[[3]] <- paste0("s=", SI*(-MAX_TD_SURPLUS:MAX_TD_SURPLUS))

get_WP_true <- function(n,x,s) {
  if (s < -MAX_TD_SURPLUS*SI) {
    s_ = -MAX_TD_SURPLUS*SI
  } else if (s > MAX_TD_SURPLUS*SI) {
    s_ = MAX_TD_SURPLUS*SI
  } else {
    s_ = s
  }

  WP_true[paste0("n=",n), paste0("x=",x), paste0("s=",s_)]
}

######################################
### fill the `WP_true` array and save it
set_WP_true <- function(value, n,x,s) {
  WP_true[paste0("n=",n), paste0("x=",x), paste0("s=",s)] <<- value
}
fill_WP_true_array <- function() {
  for (n in (N+1):0) {
    for (x in 1:(L-1)) {
      for (s in SI*(-MAX_TD_SURPLUS:MAX_TD_SURPLUS)) {
        # print(paste0("computing WP true for ", "n=", n, ", x=", x, ", s=", s))
        
        if (n == N+1) { ### base case
          if (s > 0) {
            wp = 1
          } else if (s == 0) {
            wp = 1/2
          } else { # s < 0
            wp = 0
          }
          set_WP_true(wp, n,x,s)
        } else { ### recursive case
          if (x == L-1) {
            wp_if_1 = get_WP_true(n+1, MIDFIELD, s-SI) # opposing team touchdown
            wp_if_n1 = get_WP_true(n+1, x-1, s)
          } else if (x == 1) {
            wp_if_1 = get_WP_true(n+1, x+1, s)
            wp_if_n1 = get_WP_true(n+1, MIDFIELD, s+SI) # touchdown
          } else { # 1 < x < L-1
            wp_if_1 = get_WP_true(n+1, x+1, s)
            wp_if_n1 = get_WP_true(n+1, x-1, s)
          }
          wp = 1/2*wp_if_1 + 1/2*wp_if_n1
          set_WP_true(wp, n,x,s) 
        }
      }
    }
  }
}

wp_true_filename = paste0("job_output/", sim_str, "_WP_true.rds")
if (!file.exists(wp_true_filename)) {
  fill_WP_true_array()
  saveRDS(WP_true, wp_true_filename)
} else {
  ### load the `WP_true` array 
  WP_true = readRDS(wp_true_filename)
}

######################################
### visualize `WP_true` 

### WP vs. time for various score differential values, from midfield
{
  df_visualize_wp = tibble()
  for (x in c(1, MIDFIELD, L-1)) {
    for (s in -9:9) {
      df_visualize_wp = bind_rows(df_visualize_wp, tibble(n=1:N, x=x, s=s))
    }
  }
  df_visualize_wp = df_visualize_wp %>%
    rowwise() %>%
    mutate(x_ = case_when(
      x==MIDFIELD ~ paste0("x = ", MIDFIELD, " (midfield)"),
      x==1 ~ paste0("x = ", 1, " (about to score)"),
      x==L-1 ~ paste0("x = ", L-1, " (opponent about to score)"),
      TRUE ~ ""
    )) %>%
    mutate(
      s_ = paste0("score diff = ", s)
    ) %>%
    ungroup()
}

visualize_wp <- function(wp_true=TRUE, wp_xgb_model=NULL, wp_boot_mat=NULL, boot_method="rcb", brp_=1, option=1) {
  my_palette <- c(
    brewer.pal(name="Blues",n=9)[4:9],
    rev(brewer.pal(name="Purples",n=9)[6:8]),
    "magenta", "black",
    rev(brewer.pal(name="Greens",n=9)[2:9])
  )

  plot_df1 = tibble()
  plot_df2 = tibble()
  plot_df3 = tibble()
  if (wp_true) {
    ### plot WP true 
    plot_df1 = df_visualize_wp %>% 
      rowwise() %>% 
      mutate(wp = get_WP_true(n,x,s), wp_type="true") %>% 
      ungroup()
  } 
  if (!is.null(wp_xgb_model)) {
    ### plot WP pred 
    plot_df2 = df_visualize_wp %>% 
      mutate(
        wp = predict(wp_xgb_model, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp %>% select(all_of(xgb_features))) )),
        wp_type = "XGBoost"
      )
  }
  if (!is.null(wp_boot_mat)) {
    wp_boot_mat_L = wp_boot_mat %>%
      filter(brp == brp_) %>%
      rename(wp = all_of(paste0("wp_pred_", boot_method, "_L"))) %>%
      mutate(wp_type = "lower boot.") %>%
      select(wp, wp_type)
    wp_boot_mat_U = wp_boot_mat %>%
      filter(brp == brp_) %>%
      rename(wp = all_of(paste0("wp_pred_", boot_method, "_U"))) %>%
      mutate(wp_type = "upper boot.") %>%
      select(wp, wp_type)
    plot_df3 = df_visualize_wp %>% bind_cols(wp_boot_mat_L)
    plot_df3 = bind_rows(plot_df3, df_visualize_wp%>% bind_cols(wp_boot_mat_U))
  }
  
  plot_df_A = bind_rows(plot_df1, plot_df2, plot_df3) %>%
    mutate(wp_type = factor(wp_type, levels=c("true", sort(setdiff(wp_type,"true")) )))
  if (option == 1) {
    plot_df_A %>% 
      ggplot(aes(x=n, y=wp, color=factor(s))) +
      facet_wrap(~x_) +
      geom_line(aes(linetype=wp_type), size=1) +
      scale_y_continuous(breaks=seq(0,1,by=0.1), name = "win probability") +
      scale_x_continuous(breaks=seq(0,N,by=25), name="play number n") +
      guides(color=guide_legend(title=" score\n differential\n s")) +
      guides(linetype=guide_legend(title="WP")) +
      scale_color_manual(values = my_palette)
  } else if (option == 2) {
    plot_df_A %>% 
      filter(s %in% -4:4) %>%
      ggplot(aes(x=n, y=wp, color=factor(x))) +
      facet_wrap(~fct_reorder(s_,s) ) +
      geom_line(aes(linetype=wp_type), size=1) +
      scale_y_continuous(breaks=seq(0,1,by=0.1), name = "win probability") +
      scale_x_continuous(breaks=seq(0,N,by=25), name="play number n") +
      guides(color=guide_legend(title=" field\n position\n x")) +
      guides(linetype=guide_legend(title="WP")) +
      scale_color_manual(values = c("firebrick", "black", "dodgerblue2"))
  }
}

plot_wp_true_vs_time = visualize_wp(wp_true=TRUE)
plot_wp_true_vs_time_2 = visualize_wp(wp_true=TRUE, option=2)
# plot_wp_true_vs_time
# ggsave(paste0("job_output/", sim_str, "_plot_wp_true_vs_time.png"), plot_wp_true_vs_time, width=24, height=8)
# ggsave(paste0("job_output/", sim_str, "_plot_wp_true_vs_time_2.png"), plot_wp_true_vs_time_2, width=16, height=12)

###########################################
### Simulated Football Dataframes (X,y) ###
###########################################

sample_Rademacher <- function(size=1) {
  sample(c(1,-1), size=size, replace=TRUE)
}

simulate_football_season <- function() {
  XI = sample_Rademacher(size=I) ### simulated outcome of this play
  x = numeric(I) ### field position prior to this play
  s = numeric(I) ### score differential prior to this play
  TD = numeric(I) ### td on this play
  TD_OPP = numeric(I) ### opp td on this play
  for (g in 1:G) {
    for (n in 1:N) {
      if (n == 0 & G %% 100 == 0) print(paste0("simulating play n = ", n, " of N = ", N, " in game g = ", g, " of G = ", G))
      
      play_idx = n + (g-1)*N
      
      ########################################### next field position
      if (n == 1) { ### starting play of the game
        x[play_idx] = MIDFIELD 
        s[play_idx] = 0
        TD[play_idx] = FALSE
        TD_OPP[play_idx] = FALSE
      } else {
        if (TD[play_idx-1]) { ### TD on previous play
          x[play_idx] = MIDFIELD # back to midfield
          s[play_idx] = s[play_idx-1] + SI # score a TD
        } else if (TD_OPP[play_idx-1]) { ### opp TD on previous play
          x[play_idx] = MIDFIELD # back to midfield
          s[play_idx] = s[play_idx-1] - SI # opp scores a TD
        } else {
          x[play_idx] = x[play_idx-1] + XI[play_idx-1] # advance field position
          s[play_idx] = s[play_idx-1] # no score
        }
        TD[play_idx] = x[play_idx] + XI[play_idx] == 0
        TD_OPP[play_idx] = x[play_idx] + XI[play_idx] == L
        
      }
    }
  }
  
  df = tibble(
      i = 1:I,
      g = 1+floor((i-1)/N),
      n = i - (g-1)*N,
      x = x,
      s = s,
      XI = XI,
      TD = TD,
      TD_OPP = TD_OPP,
    ) %>%
    group_by(g) %>%
    mutate(
      s_final = case_when(
        TD[n()] == 1 ~ s[n()] + SI,
        TD_OPP[n()] == 1 ~ s[n()] - SI,
        TRUE ~ s[n()]
      ),
      y = case_when(
        s_final > 0 ~ 1,
        s_final < 0 ~ 0,
        TRUE ~ as.numeric(rbernoulli(1, p=1/2)) # if tied at end of regulation, win prob is 1/2, so use a coin flip
      )
    ) %>% 
    ungroup()
  
  if (K == N) {
    df1 = df
  } else if (K == 1) {
    df1 = df %>% filter( n-1 == ((g-1)%%N) )
  } else {
    stop("haven't implemented K where 1<K<N")
  }
  # df2 = df1 %>%
  #   group_by(g) %>%
  #   filter(row_number() == sort(sample(1:n(), size=K, replace=FALSE))) %>%
  #   ungroup() 
  
  df2 = df1 %>%
    rowwise() %>%
    mutate(
      wp_actual = get_WP_true(n, x, s)
    ) %>%
    ungroup()
  
  df2
}
# simulate_football_season()
# View(simulate_football_season())

#####################################################
### check number of epochs, num plays per epoch   ###
### are consistent with observed football dataset ###
#####################################################

{
  # sfs1 = simulate_football_season()
  # sfs1a = sfs1 %>%
  #   mutate(
  #     score = as.numeric(TD==1 | TD_OPP==1),
  #     just_scored = lag(score, default=0),
  #     epoch = 1+cumsum(just_scored),
  #   ) %>%
  #   group_by(epoch) %>%
  #   summarise(
  #     num_plays = n()
  #   ) %>%
  #   summarise(
  #     mean_num_plays_per_epoch = mean(num_plays),
  #     num_epochs = n()
  #   )
  # sfs1a
  # ### in simulated football data: mean_num_plays_per_epoch == 4.096,  num_epochs == 53069
  # 
  # ################
  # # filewd = getwd()
  # # setwd("..")
  # # source("00_main.R")
  # # setwd(filewd)
  # ################
  # # data_full_WP %>%
  # data_full_110_WP %>%
  #   distinct(game_id, epoch, play_id) %>%
  #   group_by(game_id, epoch) %>%
  #   summarise(num_plays = n(), .groups="drop") %>%
  #   summarise(
  #     mean_num_plays_per_epoch = mean(num_plays),
  #     num_epochs = n()
  #   )
  # ### in observed football data:  mean_num_plays_per_epoch == 4.549,  num_epochs == 47515
}

##########################
### Simulation XGBoost ###
##########################

### generate simulation df
df = simulate_football_season()
print(df)
### hold-out half of the games for testing
all_game_idxs = 1:G
test_game_idxs = sort(sample(all_game_idxs, size=round(G*0.5), replace=FALSE))
train_game_idxs_OG = setdiff(all_game_idxs, test_game_idxs)
test_df = df %>% filter(g %in% test_game_idxs)
train_df_OG = df %>% filter(g %in% train_game_idxs_OG)
### hold-out half of the training games for validation
val_game_idxs = sort(sample(train_game_idxs_OG, size=round(length(train_game_idxs_OG)*0.5), replace=FALSE))
val_df = train_df_OG %>% filter(g %in% val_game_idxs)
train_game_idxs = setdiff(train_game_idxs_OG, val_game_idxs)
train_df = train_df_OG %>% filter(g %in% train_game_idxs)
### check
length(unique(test_df$g))
length(unique(train_df$g))
length(unique(val_df$g))

################################
### XGBoost Parameter Tuning ###
################################

xgb_features = c("n","x","s")
fit_xgb <- function(params, train_df, val_df=NULL, nrounds=NULL) {
  ### fit the XGBoost model
  train_set_xgbDM = xgb.DMatrix(
    model.matrix(~ . + 0, data = train_df %>% select(all_of(xgb_features))),
    label = train_df$y
  )
  if (is.null(val_df)) {
    watchlist <- list(train=train_set_xgbDM)
  } else {
    val_set_xgbDM = xgb.DMatrix(
      model.matrix(~ . + 0, data = val_df %>% select(all_of(xgb_features))),
      label = val_df$y
    )
    watchlist <- list(train=train_set_xgbDM, validation=val_set_xgbDM)
  }
  ### train XGBoost
  xgb <- xgb.train( 
    data = train_set_xgbDM, 
    watchlist = watchlist,
    params = params, 
    nrounds = if (is.null(nrounds)) 15000 else nrounds,
    early_stopping_rounds = if (is.null(nrounds)) 50 else Inf,
    print_every_n = 50,
    verbose = 2
  )
  return(xgb)
}

################################################################################

params_filename = paste0("job_output/", sim_str, "_params.yaml")
if (!RETUNE_XGB & file.exists(params_filename)) {
  print(paste0("loading XGBoost params in Sim2 #", SIM_NUM))
  
  params = list.load(params_filename)
} else {
  print(paste0("tuning XGBoost in Sim2 #", SIM_NUM))
  ### Baldwin's XGBoost tuning https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
  
  ### parameter tuning grid
  get_param_grid <- function() {
    dials::grid_latin_hypercube(
      dials::finalize(dials::mtry(), train_df),
      dials::min_n(),
      dials::tree_depth(range=c(length(xgb_features), length(xgb_features))),
      dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
      dials::loss_reduction(),
      sample_size = dials::sample_prop(),
      size = grid_size
    ) %>%
      dplyr::mutate(
        mtry = mtry / length(train_df),
        monotone_constraints = "(0,-1,1)"
      ) %>%
      # make these the right names for xgb
      dplyr::rename(
        eta = learn_rate,
        gamma = loss_reduction,
        subsample = sample_size,
        colsample_bytree = mtry,
        max_depth = tree_depth,
        min_child_weight = min_n
      )
  }
  # get_param_grid()
  
  # function to perform xgb.cv for a given row in a hyperparameter grid
  get_row <- function(row) {
    params <-
      list(
        booster = "gbtree",
        objective = "binary:logistic",
        eval_metric = c("logloss"),
        eta = row$eta,
        gamma = row$gamma,
        subsample = row$subsample,
        colsample_bytree = row$colsample_bytree,
        max_depth = row$max_depth,
        min_child_weight = row$min_child_weight,
        monotone_constraints = row$monotone_constraints
      )
    
    ### fit the XGBoost model
    xgb = fit_xgb(params, train_df, val_df)
  
    # bundle up the results together for returning
    output <- params
    output$iter <- xgb$best_iteration
    output$logloss <- xgb$evaluation_log[output$iter]$validation_logloss
    
    row_result <- bind_rows(output)
    return(row_result)
  }
  
  # get results
  results <- purrr::map_df(1:grid_size, function(i) {
    print(paste0("tuning row ", i, " of ", grid_size))
    get_row(get_param_grid() %>% filter(row_number() == i)  )
  })
  
  # plot tuning results
  # {
  #   results %>%
  #     dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
  #     tidyr::pivot_longer(
  #       eta:min_child_weight,
  #       values_to = "value",
  #       names_to = "parameter"
  #     ) %>%
  #     ggplot(aes(value, logloss, color = parameter)) +
  #     geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
  #     facet_wrap(~parameter, scales = "free_x") +
  #     labs(x = NULL, y = "logloss") +
  #     theme_minimal()
  # }
  
  ### best XGBoost model
  best_model <- results %>% arrange(logloss) %>% slice_head(n=1)
  print(best_model)
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = best_model$eta,
      gamma = best_model$gamma,
      subsample = best_model$subsample,
      colsample_bytree = best_model$colsample_bytree,
      max_depth = best_model$max_depth,
      min_child_weight = best_model$min_child_weight,
      monotone_constraints = best_model$monotone_constraints,
      nrounds = best_model$iter
    )

  ### save xgboost params
  list.save(params, params_filename)
} 

###########
### GAM ###
###########

# fit_lr_1 <- function(dataset) {
#   fit = glm(
#     y ~ x + I(s/(N-n+1)),
#     data = train_df_OG,
#     family = "binomial"
#   )
#   clean_lm(fit)
# }

#################################
### XGBoost Point Predictions ###
#################################

########## fit the XGBoost model ########## 
xgb_pp <- fit_xgb(params, train_df_OG, test_df, params$nrounds) 

########## visualize the XGBoost WP predictions ########## 
# plot_wp_xgb_pp_vs_time = visualize_wp(wp_true=FALSE, wp_xgb_model=xgb_pp)
# ggsave(paste0("job_output/", sim_str, "_plot_wp_xgb_pp_vs_time.png"), plot_wp_xgb_pp_vs_time, width=24, height=8)
# 
# plot_wp_xgb_pp_vs_time_2 = visualize_wp(wp_true=FALSE, wp_xgb_model=xgb_pp, option=2)
# ggsave(paste0("job_output/", sim_str, "_plot_wp_xgb_pp_vs_time.png"), plot_wp_xgb_pp_vs_time_2, width=16, height=12)
# 
# plot_wp_both_vs_time = visualize_wp(wp_true=TRUE, wp_xgb_model=xgb_pp)
# ggsave(paste0("job_output/", sim_str, "_plot_wp_both_vs_time.png"), plot_wp_both_vs_time, width=24, height=8)
# 
# plot_wp_both_vs_time_2 = visualize_wp(wp_true=TRUE, wp_xgb_model=xgb_pp, option=2)
# ggsave(paste0("job_output/", sim_str, "_plot_wp_both_vs_time_2.png"), plot_wp_both_vs_time_2, width=16, height=12)

########## Save Results: Losses ########## 
loss_results_df = tibble(
  y = test_df$y,
  wp_actual = test_df$wp_actual,
)
loss_results_df = bind_rows(
  bind_cols(
    loss_results_df,
    tibble(
      wp_pred = predict(xgb_pp, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) )),
      model_type = "xgb"
    )
  ),
  # bind_cols(
  #   loss_results_df,
  #   tibble(
  #     wp_pred = predict(lr1_pp, test_df, type = "response"),
  #     model_type = "lr"
  #   )
  # )
)
loss_results_df = loss_results_df %>%
  mutate(
    logloss_wp_y = -y*log(wp_pred) - (1-y)*log(1-wp_pred),
    brier_wp_y = (y - wp_pred)^2,
    brier_wp_pred = (wp_actual - wp_pred)^2,
    mae_wp_pred = abs(wp_actual - wp_pred)
  )
loss_results_df 

### loss
loss_df = loss_results_df %>%
  group_by(model_type) %>%
  summarise(
    logloss_wp_y = mean(logloss_wp_y),
    brier_wp_y = mean(brier_wp_y),
    brier_wp_pred = mean(brier_wp_pred),
    mae_wp_pred = mean(mae_wp_pred)
  ) %>% mutate(sim_num = SIM_NUM) %>% arrange(mae_wp_pred)
print(loss_df)
write_csv(loss_df, paste0("job_output/", sim_str, "_loss_df.csv"))
# gtsave(gt(loss_df), paste0("job_output/", sim_str, "_loss_df.png"))

 ### write_csv(loss_results_df, paste0("job_output/", "sim2_", SIM_NUM, "_loss_results_df.csv"))
### bias as a function of actual WP
loss_results_df_binned = loss_results_df %>%
  mutate(wp_actual_bin = cut(wp_actual, NUM_WP_ACTUAL_BINS)) %>%
  group_by(model_type, wp_actual_bin) %>%
  summarise(
    logloss_wp_y = mean(logloss_wp_y),
    brier_wp_y = mean(brier_wp_y),
    brier_wp_pred = mean(brier_wp_pred),
    mae_wp_pred = mean(mae_wp_pred)
  ) %>% mutate(sim_num = SIM_NUM)
print(loss_results_df_binned)
write_csv(loss_results_df_binned, paste0("job_output/", sim_str, "_loss_results_df_binned.csv"))

##############################################################
### XGBoost predictions and Bootstrap Confidence Intervals ###
##############################################################

if (DO_BOOTSTRAP_ANALYSIS) {
  
  # boot_resample_props = c(1, 0.5, 0.25)
  boot_resample_props = c(1, 0.5)
  BRP = length(boot_resample_props)
  wp_preds_iidb = array(dim=c(nrow(test_df), B+1, BRP))
  wp_preds_cb = array(dim=c(nrow(test_df), B+1, BRP))
  wp_preds_rcb = array(dim=c(nrow(test_df), B+1, BRP))
  wp_preds_iidb_viz = array(dim=c(nrow(df_visualize_wp), B+1, BRP))
  wp_preds_cb_viz = array(dim=c(nrow(df_visualize_wp), B+1, BRP))
  wp_preds_rcb_viz = array(dim=c(nrow(df_visualize_wp), B+1, BRP))
  # wp_preds_rcb2 = array(dim=c(nrow(test_df), B+1, BRP))
  dimnames(wp_preds_iidb) = list( paste0("i=",1:nrow(test_df)), paste0("b=",0:B), paste0("brp=",boot_resample_props) )
  dimnames(wp_preds_cb) = list( paste0("i=",1:nrow(test_df)), paste0("b=",0:B), paste0("brp=",boot_resample_props) )
  dimnames(wp_preds_rcb) = list( paste0("i=",1:nrow(test_df)), paste0("b=",0:B), paste0("brp=",boot_resample_props) )
  # dimnames(wp_preds_rcb2) = list( paste0("i=",1:nrow(test_df)), paste0("b=",0:B), paste0("brp=",boot_resample_props) )
  
  for (j in 1:BRP) {
    brp = boot_resample_props[j]
    
    for (b in 0:B) {
      # if (b %% 5 == 0)
      print(""); print(paste0("*** bootstrap sample b = ", b, " of B = ", B, " with brp ", brp, " idx ", j, " of ", BRP, " ***")); print(""); 
      if (b==0) { ### actual data
        train_df_iidb = train_df_OG
        train_df_cb = train_df_OG
        train_df_rcb = train_df_OG
        # train_df_rcb2 = train_df_OG
      } else {
        ### iid bootstrap sample
        num_resample_iidb = round(nrow(train_df_OG)*brp)
        train_game_idxs_iidb = sort(sample(1:nrow(train_df_OG), size=num_resample_iidb, replace=TRUE))
        train_df_iidb = train_df_OG[train_game_idxs_iidb,]
        ### clustered bootstrap sample
        num_resample_cb = round(length(train_game_idxs_OG)*brp)
        train_df_cb = tibble(
          g = sort(sample(train_game_idxs_OG, size=num_resample_cb, replace=TRUE))
        ) %>% mutate(ii = 1:n())
        train_df_cb = left_join(train_df_cb, train_df_OG)
        ### randomized clustered bootstrap sample
        train_df_rcb = train_df_cb %>% select(g, ii, i)
        train_df_rcb = train_df_rcb %>% 
          group_by(ii) %>%
          sample_n(size = n(), replace = TRUE) %>%
          ungroup() %>%
          left_join(train_df_OG) %>%
          arrange(g,ii,n)
        # ### randomized clustered bootstrap sample with smaller proportion of rows resampled
        # train_df_rcb2 = train_df_cb %>% select(g, ii, i)
        # train_df_rcb2 = train_df_rcb2 %>% 
        #   group_by(ii) %>%
        #   mutate(i2 = sort(sample(i, size=n(), replace=TRUE)) ) %>%
        #   filter(row_number() <= round(n()*brp)) %>%
        #   ungroup() %>%
        #   select(g, ii, i2) %>%
        #   rename(i = i2) %>%
        #   left_join(train_df_OG)
      }
      ### iid bootstrap model
      # print(paste0("fit iid bootstrap model b = ", b, " of B = ", B))
      xgb_iidb <- fit_xgb(params, train_df_iidb, test_df, params$nrounds) 
      preds_iidb = predict(xgb_iidb, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) ))
      preds_iidb_viz = predict(xgb_iidb, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp %>% select(all_of(xgb_features))) ))
      ### clustered bootstrap model
      # print(paste0("fit clustered bootstrap model b = ", b, " of B = ", B))
      xgb_cb <- fit_xgb(params, train_df_cb, test_df, params$nrounds) 
      preds_cb = predict(xgb_cb, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) ))
      preds_cb_viz = predict(xgb_cb, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp %>% select(all_of(xgb_features))) ))
      ### randomized clustered bootstrap model
      # print(paste0("fit randomized clustered bootstrap model b = ", b, " of B = ", B))
      xgb_rcb <- fit_xgb(params, train_df_rcb, test_df, params$nrounds) 
      preds_rcb = predict(xgb_rcb, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) ))
      preds_rcb_viz = predict(xgb_rcb, xgb.DMatrix(model.matrix(~ . + 0, data = df_visualize_wp %>% select(all_of(xgb_features))) ))
      # ### randomized clustered bootstrap version 2 model
      # xgb_rcb2 <- fit_xgb(params, train_df_rcb2, test_df, params$nrounds) 
      # preds_rcb2 = predict(xgb_rcb2, xgb.DMatrix(model.matrix(~ . + 0, data = test_df %>% select(all_of(xgb_features))) ))
      ###
      wp_preds_iidb[,b+1,j] = preds_iidb
      wp_preds_cb[,b+1,j] = preds_cb
      wp_preds_rcb[,b+1,j] = preds_rcb
      wp_preds_iidb_viz[,b+1,j] = preds_iidb_viz
      wp_preds_cb_viz[,b+1,j] = preds_cb_viz
      wp_preds_rcb_viz[,b+1,j] = preds_rcb_viz
      # wp_preds_rcb2[,b+1,j] = preds_rcb2
    }
  }
  
  #################################
  ### Visualize Bootstrap Dists ###
  #################################
  
  alpha = 0.025
  viz_df_boot = tibble()
  for (j in 1:BRP) {
    viz_df_boot_j = tibble(
      wp_pred_iidb_L = apply(wp_preds_iidb_viz[,1:(B+1),j], 1, function(x) quantile(x, alpha) ),
      wp_pred_iidb_U = apply(wp_preds_iidb_viz[,1:(B+1),j], 1, function(x) quantile(x, 1-alpha) ),
      wp_pred_cb_L = apply(wp_preds_cb_viz[,1:(B+1),j], 1, function(x) quantile(x, alpha) ),
      wp_pred_cb_U = apply(wp_preds_cb_viz[,1:(B+1),j], 1, function(x) quantile(x, 1-alpha) ),
      wp_pred_rcb_L = apply(wp_preds_rcb_viz[,1:(B+1),j], 1, function(x) quantile(x, alpha) ),
      wp_pred_rcb_U = apply(wp_preds_rcb_viz[,1:(B+1),j], 1, function(x) quantile(x, 1-alpha) ),
      brp = boot_resample_props[j]
    )
    viz_df_boot = bind_rows(viz_df_boot, viz_df_boot_j)
  }

  # ####################
  # plot_wp_both_boot_iidb_vs_time = visualize_wp(wp_true=TRUE, wp_boot_mat=viz_df_boot, boot_method="iidb", brp_=1)
  # ggsave(paste0("job_output/", sim_str, "_plot_wp_both_boot_iidb_vs_time.png"), plot_wp_both_boot_iidb_vs_time, width=24, height=8)
  # 
  # plot_wp_both_boot_iidb_vs_time_2 = visualize_wp(wp_true=TRUE, wp_boot_mat=viz_df_boot, boot_method="iidb", brp_=1, option=2)
  # ggsave(paste0("job_output/", sim_str, "_plot_wp_both_boot_iidb_vs_time_2.png"), plot_wp_both_boot_iidb_vs_time_2, width=16, height=12)
  # 
  # 
  # plot_wp_both_boot_rcb_vs_time = visualize_wp(wp_true=TRUE, wp_boot_mat=viz_df_boot, boot_method="rcb", brp_=0.5)
  # ggsave(paste0("job_output/", sim_str, "_plot_wp_both_boot_rcb_vs_time.png"), plot_wp_both_boot_rcb_vs_time, width=24, height=8)
  # 
  # plot_wp_both_boot_rcb_vs_time_2 = visualize_wp(wp_true=TRUE, wp_boot_mat=viz_df_boot, boot_method="rcb", brp_=0.5, option=2)
  # ggsave(paste0("job_output/", sim_str, "_plot_wp_both_boot_rcb_vs_time_2.png"), plot_wp_both_boot_rcb_vs_time_2, width=16, height=12)
  
  ########################
  ### Save Results: CI ###
  ########################
  
  ##### coverages ##### 
  get_covg_df <- function(B, alpha = 0.025) {
    boot_results_df = tibble()
    for (j in 1:BRP) {
      boot_results_df_j = tibble(
        wp_actual = test_df$wp_actual,
        wp_pred_iidb_L = apply(wp_preds_iidb[,1:(B+1),j], 1, function(x) quantile(x, alpha) ),
        wp_pred_iidb_M = apply(wp_preds_iidb[,1:(B+1),j], 1, function(x) quantile(x, 0.5) ),
        wp_pred_iidb_U = apply(wp_preds_iidb[,1:(B+1),j], 1, function(x) quantile(x, 1-alpha) ),
        wp_pred_cb_L = apply(wp_preds_cb[,1:(B+1),j], 1, function(x) quantile(x, alpha) ),
        wp_pred_cb_M = apply(wp_preds_cb[,1:(B+1),j], 1, function(x) quantile(x, 0.5) ),
        wp_pred_cb_U = apply(wp_preds_cb[,1:(B+1),j], 1, function(x) quantile(x, 1-alpha) ),
        wp_pred_rcb_L = apply(wp_preds_rcb[,1:(B+1),j], 1, function(x) quantile(x, alpha) ),
        wp_pred_rcb_M = apply(wp_preds_rcb[,1:(B+1),j], 1, function(x) quantile(x, 0.5) ),
        wp_pred_rcb_U = apply(wp_preds_rcb[,1:(B+1),j], 1, function(x) quantile(x, 1-alpha) ),
        # wp_pred_rcb2_L = apply(wp_preds_rcb2[,1:(B+1),j], 1, function(x) quantile(x, alpha) ),
        # wp_pred_rcb2_U = apply(wp_preds_rcb2[,1:(B+1),j], 1, function(x) quantile(x, 1-alpha) ),
        brp = boot_resample_props[j]
      )
      ### widen CI for WP near 0 and 1
      boot_results_df_j = boot_results_df_j %>% mutate(
        wp_pred_iidb_2_L = ifelse(wp_pred_iidb_M <= 0.02, 0, wp_pred_iidb_L),
        wp_pred_cb_2_L = ifelse(wp_pred_cb_M <= 0.02, 0, wp_pred_cb_L),
        wp_pred_rcb_2_L = ifelse(wp_pred_rcb_M <= 0.02, 0, wp_pred_rcb_L),
        wp_pred_iidb_2_U = ifelse(wp_pred_iidb_M >= 0.98, 1, wp_pred_iidb_U),
        wp_pred_cb_2_U = ifelse(wp_pred_cb_M >= 0.98, 1, wp_pred_cb_U),
        wp_pred_rcb_2_U = ifelse(wp_pred_rcb_M >= 0.98, 1, wp_pred_rcb_U),
      )
      boot_results_df = bind_rows(boot_results_df, boot_results_df_j)
    }
    ##### coverages ##### 
    covg_results_df = boot_results_df %>%
      mutate(
        covered_iidb = as.numeric(wp_pred_iidb_L <= wp_actual & wp_actual <= wp_pred_iidb_U),
        covered_cb = as.numeric(wp_pred_cb_L <= wp_actual & wp_actual <= wp_pred_cb_U),
        covered_rcb = as.numeric(wp_pred_rcb_L <= wp_actual & wp_actual <= wp_pred_rcb_U),
        covered_iidb_L = as.numeric(wp_pred_iidb_L <= wp_actual),
        covered_cb_L = as.numeric(wp_pred_cb_L <= wp_actual),
        covered_rcb_L = as.numeric(wp_pred_rcb_L <= wp_actual),
        covered_iidb_U = as.numeric(wp_actual <= wp_pred_iidb_U),
        covered_cb_U = as.numeric(wp_actual <= wp_pred_cb_U),
        covered_rcb_U = as.numeric(wp_actual <= wp_pred_rcb_U),
        
        covered_iidb_2 = as.numeric(wp_pred_iidb_2_L <= wp_actual & wp_actual <= wp_pred_iidb_2_U),
        covered_cb_2 = as.numeric(wp_pred_cb_2_L <= wp_actual & wp_actual <= wp_pred_cb_2_U),
        covered_rcb_2 = as.numeric(wp_pred_rcb_2_L <= wp_actual & wp_actual <= wp_pred_rcb_2_U),
        covered_iidb_2_L = as.numeric(wp_pred_iidb_2_L <= wp_actual),
        covered_cb_2_L = as.numeric(wp_pred_cb_2_L <= wp_actual),
        covered_rcb_2_L = as.numeric(wp_pred_rcb_2_L <= wp_actual),
        covered_iidb_2_U = as.numeric(wp_actual <= wp_pred_iidb_2_U),
        covered_cb_2_U = as.numeric(wp_actual <= wp_pred_cb_2_U),
        covered_rcb_2_U = as.numeric(wp_actual <= wp_pred_rcb_2_U),
        # covered_rcb2 = as.numeric(wp_pred_rcb2_L <= wp_actual & wp_actual <= wp_pred_rcb2_U),
        length_iidb = wp_pred_iidb_U - wp_pred_iidb_L,
        length_cb = wp_pred_cb_U - wp_pred_cb_L,
        length_rcb = wp_pred_rcb_U - wp_pred_rcb_L,
        
        length_iidb_2 = wp_pred_iidb_2_U - wp_pred_iidb_2_L,
        length_cb_2 = wp_pred_cb_2_U - wp_pred_cb_2_L,
        length_rcb_2 = wp_pred_rcb_2_U - wp_pred_rcb_2_L,
        # length_rcb2 = wp_pred_rcb2_U - wp_pred_rcb2_L,
      )
      
    return(covg_results_df)
  }
  # get_covg_df(B=1)
  
  # B_list = c(10,25,50,100,150,200,250,500,1000)
  B_list = c(25)
  COVG_DF = tibble()
  COVG_DF_BINNED = tibble()
  for (BB in B_list) {
    if (BB <= B) {
      covg_results_df_BB = get_covg_df(B=BB)
      
      covg_df_BB = covg_results_df_BB %>%
        group_by(brp) %>%
        summarise(
          sim_num = SIM_NUM,
          B = B,
          covered_iidb = mean(covered_iidb),
          covered_cb = mean(covered_cb),
          covered_rcb = mean(covered_rcb),
          covered_iidb_L = mean(covered_iidb_L),
          covered_cb_L = mean(covered_cb_L),
          covered_rcb_L = mean(covered_rcb_L),
          covered_iidb_U = mean(covered_iidb_U),
          covered_cb_U = mean(covered_cb_U),
          covered_rcb_U = mean(covered_rcb_U),
          covered_iidb_2 = mean(covered_iidb_2),
          covered_cb_2 = mean(covered_cb_2),
          covered_rcb_2 = mean(covered_rcb_2),
          covered_iidb_2_L = mean(covered_iidb_2_L),
          covered_cb_2_L = mean(covered_cb_2_L),
          covered_rcb_2_L = mean(covered_rcb_2_L),
          covered_iidb_2_U = mean(covered_iidb_2_U),
          covered_cb_2_U = mean(covered_cb_2_U),
          covered_rcb_2_U = mean(covered_rcb_2_U),
          length_iidb = mean(length_iidb),
          length_cb = mean(length_cb),
          length_rcb = mean(length_rcb),
          length_iidb_2 = mean(length_iidb_2),
          length_cb_2 = mean(length_cb_2),
          length_rcb_2 = mean(length_rcb_2),
        )
      
      covg_df_BB_binned = covg_results_df_BB %>%
        mutate(wp_actual_bin = cut(wp_actual, NUM_WP_ACTUAL_BINS)) %>%
        group_by(brp, wp_actual_bin) %>%
        summarise(
          sim_num = SIM_NUM,
          B = B,
          covered_iidb = mean(covered_iidb),
          covered_cb = mean(covered_cb),
          covered_rcb = mean(covered_rcb),
          covered_iidb_L = mean(covered_iidb_L),
          covered_cb_L = mean(covered_cb_L),
          covered_rcb_L = mean(covered_rcb_L),
          covered_iidb_U = mean(covered_iidb_U),
          covered_cb_U = mean(covered_cb_U),
          covered_rcb_U = mean(covered_rcb_U),
          length_iidb = mean(length_iidb),
          length_cb = mean(length_cb),
          length_rcb = mean(length_rcb),
          covered_iidb_2 = mean(covered_iidb_2),
          covered_cb_2 = mean(covered_cb_2),
          covered_rcb_2 = mean(covered_rcb_2),
          covered_iidb_2_L = mean(covered_iidb_2_L),
          covered_cb_2_L = mean(covered_cb_2_L),
          covered_rcb_2_L = mean(covered_rcb_2_L),
          covered_iidb_2_U = mean(covered_iidb_2_U),
          covered_cb_2_U = mean(covered_cb_2_U),
          covered_rcb_2_U = mean(covered_rcb_2_U),
          length_iidb_2 = mean(length_iidb_2),
          length_cb_2 = mean(length_cb_2),
          length_rcb_2 = mean(length_rcb_2),
        )
      
      COVG_DF = bind_rows(COVG_DF, covg_df_BB)
      COVG_DF_BINNED = bind_rows(COVG_DF_BINNED, covg_df_BB_binned)
      # print(data.frame(covg_df_BB))
      # write_csv(covg_df_BB, paste0("job_output/", "sim2_", SIM_NUM, "_covg_df_B", BB, ".csv"))
    }
  }
  print(data.frame(COVG_DF))
  write_csv(COVG_DF, paste0("job_output/", sim_str, "_covg_df.csv"))
  # gtsave(gt(COVG_DF), paste0("job_output/", sim_str, "_covg_df.png"))
  
  print(data.frame(COVG_DF_BINNED))
  write_csv(COVG_DF_BINNED, paste0("job_output/", sim_str, "_covg_df_binned.csv"))
}
