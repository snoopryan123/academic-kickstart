
### fitting the grid f = f(I,R)
### as part of: Rethinking WAR for Starting Pitchers
### Author: Ryan Brill

library(tidyverse)
library(xgboost)
library(skellam)
library(truncnorm)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

############
### Data ###
############

### load half-inning data from Retrosheet 
### (you can ask me how I got this data)
df_f_grid = read_csv("data/df_f_grid.csv")
df_f_grid
data.frame(df_f_grid %>% filter(GAME_ID == "ANA201004050")) ### Angels vs Twins on April 5 2010, Angels win 6-3

######################
### Empirical Grid ###
######################

### train empirical grid
train_empirical_f_grid = function(df) {
  # browser()
  f_grid_empirical = df %>%
    group_by(INNING,CUM_RUNS) %>%
    summarise(y_hat = mean(PIT_WINS), 
              .groups = "drop") %>%
    ungroup() 
  f_grid_empirical = reshape2::acast(f_grid_empirical, INNING~CUM_RUNS, value.var="y_hat")
  f_grid_empirical = ifelse(is.na(f_grid_empirical), 0, f_grid_empirical)
  rownames(f_grid_empirical) = paste0("INNING_",1:9)
  colnames(f_grid_empirical) = paste0("CUM_RUNS_",(0:(ncol(f_grid_empirical)-1)))
  f_grid_empirical
}

### function to plot f = f(I,R) grid
plot_grid_f_IR <- function(f_grid) {
  ### WP is a matrix with 9 rows (innings) and Rmax+1 columns (runs allowed)
  
  WPi = as_tibble(t(f_grid))
  colnames(WPi) = paste0("inn",1:9)
  WPii = stack(WPi) 
  WPii$runs = rep(0:(nrow(WPi)-1), 9)
  
  pWPiis = WPii %>% filter(runs <= 13) %>%
    mutate(inning=str_sub(ind,start=4)) %>%
    ggplot(aes(x=runs,y=values,color=inning)) + 
    geom_point() + 
    geom_line(linewidth=1) +
    labs(
      # title=TeX("smoothed $f(I,R)$ as a function of $R$, for each $I$"),
      y="context-neutral win probability",
      x="runs allowed through the end of the given inning") +
    scale_x_continuous(breaks=seq(0,30,by=2)) +
    scale_y_continuous(breaks=seq(0,1,by=0.1))
  pWPiis
}

### fit and plot the 2010-2019 empirical grid  
empirical_f_grid = train_empirical_f_grid(df_f_grid)
plot_grid_f_IR(empirical_f_grid)

### fit and plot the 2019 NL empirical grid  
empirical_f_grid_2019_NL = train_empirical_f_grid(
  df_f_grid %>% filter(YEAR == 2019, HOME_LEAGUE == "NL")
)
plot_grid_f_IR(empirical_f_grid_2019_NL)

##########################################
### XGBoost with Monotonic Constraints ###
##########################################

### xgboost model fitting functions
f_xgb_features = c("INNING", "CUM_RUNS")
# don't tune the XGBoost twice
params = if (file.exists("xgb_f_grid_params.yaml")) list.load("xgb_f_grid_params.yaml") else NULL

fit_xgb_f_grid <- function(params, data_train, data_test=NULL, w=FALSE, param_tuning=FALSE) {
  f_xgb_trainMat = xgb.DMatrix(
    model.matrix(~ . + 0, data = data_train %>% select(all_of(f_xgb_features))), 
    label = data_train$PIT_WINS,
    weight = if (w) data_train$w else rep(1, nrow(data_train))
  )
  
  if (!is.null(data_test)) {
    f_xgb_testMat = xgb.DMatrix(
      model.matrix(~ . + 0, data = data_test %>% select(all_of(f_xgb_features))), 
      label = data_test$PIT_WINS
    )
    watchlist = list(train=f_xgb_trainMat, test=f_xgb_testMat)
  } else  {
    watchlist = list(train=f_xgb_trainMat)
  }
  
  nrounds = params$nrounds
  params = within(params, rm(nrounds))
  
  if (!param_tuning) {
    f_xgb_model <- xgb.train( 
      data = f_xgb_trainMat, 
      watchlist = watchlist,
      params = params,
      nrounds = nrounds,
      print_every_n = 50
    )
  } else {
    f_xgb_model <- xgb.train( 
      data = f_xgb_trainMat, 
      watchlist = watchlist,
      params = params,
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = 50
    )
  }
  
  return(f_xgb_model)
}

pred_xgb_f_grid <- function(df, xgbm) {
  f_xgb_testMat = xgb.DMatrix(
    model.matrix(~ . + 0, data = df %>% select(all_of(f_xgb_features)))
  )
  predict(xgbm, f_xgb_testMat)
}

### xgboost parameter tuning setup
df_f_train = df_f_grid
f_xgb_trainMat = xgb.DMatrix(
  model.matrix(~ . + 0, data = df_f_train %>% select(all_of(f_xgb_features))), 
  label = df_f_train$PIT_WINS
)

### Ben Baldwin's param tuning from https://www.opensourcefootball.com/posts/2021-04-13-creating-a-model-from-scratch-using-xgboost-in-r/
{
  ###############################################################
  library(dials)
  set.seed(30) ### Todd
  grid_size = 40
  f_xgb_param_grid = grid_latin_hypercube(
    dials::loss_reduction(),
    #################
    dials::min_n(),
    dials::finalize(dials::mtry(), df_f_train), # this finalize thing is because mtry depends on # of columns in data
    dials::tree_depth(),
    dials::learn_rate(range = c(-1.5, -0.5), trans = scales::log10_trans()),
    sample_size = dials::sample_prop(),
    #################
    # dials::min_n(range=c(20,30)),
    # dials::mtry(range = c(round(length(df_f_train) * 0.8), length(df_f_train))),
    # dials::tree_depth(range=c(3,4)),
    # dials::learn_rate(range = c(-1.5, -1), trans = scales::log10_trans()),
    # sample_size = dials::sample_prop(range = c(0.8, 1)),
    #################
    size = grid_size
  ) %>% mutate(
    mtry = mtry / length(df_f_train),
    monotone_constraints = "(1, -1)"
  ) %>% rename(
    eta = learn_rate,
    gamma = loss_reduction,
    subsample = sample_size,
    colsample_bytree = mtry,
    max_depth = tree_depth,
    min_child_weight = min_n
  )
  f_xgb_param_grid
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
    
    fold1 = sort(sample(1:nrow(df_f_train), replace=FALSE, size=0.5*nrow(df_f_train)))
    fold2 = setdiff(1:nrow(df_f_train), fold1)
    folds = list(Fold1 = fold1, Fold2 = fold2)
    
    # do the cross validation
    wp_cv_model <- xgboost::xgb.cv(
      data = f_xgb_trainMat,
      params = params,
      folds = folds,
      metrics = list("logloss"),
      nrounds = 15000,
      early_stopping_rounds = 50,
      print_every_n = 50
    )
    
    # bundle up the results together for returning
    output <- params
    output$iter <- wp_cv_model$best_iteration
    output$logloss <- wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
    
    row_result <- bind_rows(output)
    
    return(row_result)
  }
  # get results
  results = map_df(1:nrow(f_xgb_param_grid), function(x) {
    print(paste0("row ", x)); return(get_row(f_xgb_param_grid %>% dplyr::slice(x)))
  })
  # visualize param tuning
  results %>%
    dplyr::select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
    tidyr::pivot_longer(
      eta:min_child_weight,
      values_to = "value",
      names_to = "parameter"
    ) %>%
    ggplot(aes(value, logloss, color = parameter)) +
    geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "logloss") +
    theme_minimal()
  # re-tune, with better param range based on these plots...
  # Collect best parameters
  results %>% arrange(logloss) %>% select(eta, subsample, colsample_bytree, max_depth, logloss, min_child_weight, iter)
  best_model <- results %>% arrange(logloss) %>% slice_head()
  best_model
  params <- list(
    booster = "gbtree",
    objective = "reg:logistic",
    # objective = "binary:logistic",
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
  params
  list.save(params, "data/xgb_f_grid_params.yaml")
  ###############################################################
}

### get parameters xgboost model
params = list.load("data/xgb_f_grid_params.yaml")

### XGBoost model to f_grid matrix
xgb_to_grid <- function(f_xgb) {
  max_r = 10
  f_xgb_grid = matrix(nrow = 9, ncol = max_r+1) ### WP matrix for XGBoost
  for (inn in 1:9) {
    test_df_inn = tibble(INNING = inn, CUM_RUNS = 0:(max_r))
    f_xgb_grid[inn,] = pred_xgb_f_grid(test_df_inn, f_xgb)
  }
  rownames(f_xgb_grid) = paste0("INNING_",1:9)
  colnames(f_xgb_grid) = paste0("CUM_RUNS_",(0:(ncol(f_xgb_grid)-1)))
  return(f_xgb_grid)
}

### fit and plot the 2010-2019 XGBoost grid  
xgb_f_grid_xgb = fit_xgb_f_grid(params, df_f_grid)
xgb_f_grid = xgb_to_grid(xgb_f_grid_xgb)
plot_grid_f_IR(xgb_f_grid)

### fit and plot the 2019 NL XGBoost grid  
xgb_f_grid_xgb_2019_NL = fit_xgb_f_grid(
  params, df_f_grid %>% filter(YEAR == 2019, HOME_LEAGUE == "NL")
) # technically supposed to retrain, but here it doesn't make a difference...
xgb_f_grid_2019_NL = xgb_to_grid(xgb_f_grid_xgb_2019_NL)
plot_grid_f_IR(xgb_f_grid_2019_NL)



plot_grid_f_IR(xgb_to_grid(fit_xgb_f_grid(
  params, df_f_grid %>% filter(YEAR == 2019, HOME_LEAGUE == "NL")
)))
plot_grid_f_IR(xgb_to_grid(fit_xgb_f_grid(
  params, df_f_grid %>% filter(YEAR == 2018, HOME_LEAGUE == "NL")
)))
plot_grid_f_IR(xgb_to_grid(fit_xgb_f_grid(
  params, df_f_grid %>% filter(YEAR == 2017, HOME_LEAGUE == "NL")
)))


##############################
### Simplest Poisson Model ###
##############################

df_lambda_yr_lg = df_f_grid %>%
  group_by(YEAR,HOME_LEAGUE,BAT_TEAM_ID) %>%
  summarise(lambda = mean(INN_RUNS)) %>%
  group_by(YEAR,HOME_LEAGUE) %>%
  summarise(lambda = mean(lambda)) 
df_lambda_yr_lg

### Skellam f grid
get_f_grid_Skellam <- function(lambda, max_r = 10) {
  f_grid = matrix(nrow = 9, ncol = max_r+1) ### WP matrix
  rownames(f_grid) = paste0("INNING_",1:9)
  colnames(f_grid) = paste0("CUM_RUNS_",(0:(ncol(f_grid)-1)))
  for (R in 0:max_r) {
    for (I in 1:9) {
      if (I < 9) {
        t1 = pskellam(R, lambda1 = 9*lambda, lambda2 = (9-I)*lambda, lower.tail = FALSE)
        ### pskellam means P(Skellam(9lambda, 9(9-I-1)) > R)
        t2 = dskellam(R, lambda1 = 9*lambda, lambda2 = (9-I)*lambda)
        ### dskellam means P(Skellam == R)
        f_grid[I,R+1] = t1 + 1/2*t2
      } else {
        t1 = ppois(R, lambda = 9*lambda, lower.tail = FALSE)
        t2 = dpois(R, lambda = 9*lambda)
        f_grid[I,R+1] = t1 + 1/2*t2
      }
    }
  }
  return(f_grid)
}

### fit Skellam f grids in each lg-szn
f_grids_Skellam = list()
for (yr in unique(df_f_grid$YEAR)) {
  for (lg in unique(df_f_grid$HOME_LEAGUE)) {
    lambda_yr_lg = (df_lambda_yr_lg %>% filter(YEAR == yr & HOME_LEAGUE == lg))$lambda
    f_grid_yr_lg = get_f_grid_Skellam(lambda = lambda_yr_lg, max_r = 10)
    # save Skellam grid
    f_grids_Skellam[[paste0("f_grid_",yr,"_",lg)]] = f_grid_yr_lg
  }
}

### visualize 2019 NL Skellam grid
plot_grid_f_IR(f_grids_Skellam[["f_grid_2019_NL"]])
plot_grid_f_IR(f_grids_Skellam[["f_grid_2019_AL"]])
plot_grid_f_IR(f_grids_Skellam[["f_grid_2018_NL"]])
plot_grid_f_IR(f_grids_Skellam[["f_grid_2018_AL"]])

################################################
### Poisson Model with Positive Normal Prior ###
################################################

df_lambda_yr_lg = df_f_grid %>%
  group_by(YEAR,HOME_LEAGUE,BAT_TEAM_ID) %>%
  summarise(
    lambda = mean(INN_RUNS),
    sigma = sd(INN_RUNS)
  ) %>%
  group_by(YEAR,HOME_LEAGUE) %>%
  summarise(
    lambda = mean(lambda),
    sigma = mean(sigma)
  ) 
df_lambda_yr_lg

### 
get_f_grid_Skellam_XY <- function(lambda_X, lambda_Y, max_r = 10) {
  f_grid = matrix(nrow = 9, ncol = max_r+1) ### WP matrix
  rownames(f_grid) = paste0("INNING_",1:9)
  colnames(f_grid) = paste0("CUM_RUNS_",(0:(ncol(f_grid)-1)))
  for (R in 0:max_r) {
    for (I in 1:9) {
      if (I < 9) {
        t1 = pskellam(R, lambda1 = 9*lambda_X, lambda2 = (9-I)*lambda_Y, lower.tail = FALSE)
        t2 = dskellam(R, lambda1 = 9*lambda_X, lambda2 = (9-I)*lambda_Y)
        f_grid[I,R+1] = t1 + 1/2*t2
      } else {
        t1 = ppois(R, lambda = 9*lambda_X, lower.tail = FALSE)
        t2 = dpois(R, lambda = 9*lambda_X)
        f_grid[I,R+1] = t1 + 1/2*t2
      }
    }
  }
  return(f_grid)
}

### Skellam f grid with positive Normal prior via Monte Carlo Simulation
monte_carlo_f_grid <- function(lambda_hat, sigma_hat, B=100) {
  f_grid = NULL
  for (b in 1:B) {
    # browser()
    ### sample lambda_X and lambda_Y
    lambda_X_b = truncnorm::rtruncnorm(n=1, a=0, mean=lambda_hat, sd=sigma_hat)
    lambda_Y_b = truncnorm::rtruncnorm(n=1, a=0, mean=lambda_hat, sd=sigma_hat)
    ### get f(I,R) grid for this league-season and monte carlo sample b
    f_grid_b = get_f_grid_Skellam_XY(lambda_X_b, lambda_Y_b, max_r = 10)
    if (b == 1) {
      ### initialize the grid f(I,R | yr,lg)
      f_grid = f_grid_b
    } else {
      ### running average of the grid
      f_grid = 1/b * f_grid_b + (b-1)/b * f_grid
    }
  }
  return(f_grid)
}

### fit Skellam f grids with positive Normal prior in each lg-szn
f_grids_Skellam_pN = list()
for (yr in unique(df_f_grid$YEAR)) {
  for (lg in unique(df_f_grid$HOME_LEAGUE)) {
    print(paste0("computing f(I,R) grid for lg ", lg, " and szn ", yr))
    lambda_yr_lg = (df_lambda_yr_lg %>% filter(YEAR == yr & HOME_LEAGUE == lg))$lambda
    sigma_yr_lg = (df_lambda_yr_lg %>% filter(YEAR == yr & HOME_LEAGUE == lg))$sigma
    f_grid_yr_lg = monte_carlo_f_grid(lambda_yr_lg, sigma_yr_lg)
    ### save Skellam grid
    f_grids_Skellam_pN[[paste0("f_grid_",yr,"_",lg)]] = f_grid_yr_lg
  }
}

### visualize 2019 NL Skellam grid with positive Normal prior 
plot_grid_f_IR(f_grids_Skellam_pN[["f_grid_2019_NL"]])
plot_grid_f_IR(f_grids_Skellam_pN[["f_grid_2019_AL"]])
plot_grid_f_IR(f_grids_Skellam_pN[["f_grid_2018_NL"]])
plot_grid_f_IR(f_grids_Skellam_pN[["f_grid_2018_AL"]])

#############################################################################
### Poisson Model with Positive Normal Prior and Overdispersion Parameter ###
#############################################################################

### tune k for the fit dispersed Skellam f(I,R)
lambdaF = mean(df_f_grid$INN_RUNS)
sigmaF = sd(df_f_grid$INN_RUNS)
# choose the k which minimizes the logloss of 
# f grid predictions and the actual win/loss column
set.seed(22) #Kershaw
# ks = seq(0.1,1,by=0.05)
ks = seq(0.2,0.3,by=0.01)
logLosses = numeric(length(ks))
for (i in 1:length(ks)) {
  k = ks[i]
  f_grid_k = monte_carlo_f_grid(lambdaF, sigmaF*k)
  f_grid_k1 = reshape2::melt(f_grid_k) %>%
    mutate(
      INNING = as.numeric(str_sub(Var1, start=8)),
      CUM_RUNS = as.numeric(str_sub(Var2, start=10)),
    ) %>%
    rename(wp_hat = value) %>%
    select(-c(Var1,Var2)) %>%
    as_tibble()
  eval_df_k = (df_f_grid %>%
                 left_join(f_grid_k1) %>%
                 summarise(logloss_ = logloss(PIT_WINS, wp_hat)))$logloss_
  logLosses[i] = eval_df_k
}
plot(logLosses)
ks[which(logLosses == min(logLosses))]

### fit Skellam f grids with positive Normal prior and tuned overdispersion in each lg-szn
f_grids_Skellam_pNk = list()
for (yr in unique(df_f_grid$YEAR)) {
  for (lg in unique(df_f_grid$HOME_LEAGUE)) {
    print(paste0("computing f(I,R) grid for lg ", lg, " and szn ", yr))
    lambda_yr_lg = (df_lambda_yr_lg %>% filter(YEAR == yr & HOME_LEAGUE == lg))$lambda
    sigma_yr_lg = (df_lambda_yr_lg %>% filter(YEAR == yr & HOME_LEAGUE == lg))$sigma
    k = 0.28 # overdispersion parameters
    f_grid_yr_lg = monte_carlo_f_grid(lambda_yr_lg, sigma_yr_lg*k)
    ### save Skellam grid
    f_grids_Skellam_pNk[[paste0("f_grid_",yr,"_",lg)]] = f_grid_yr_lg
  }
}

### visualize 2019 NL Skellam grid with positive Normal prior and tuned overdispersion
plot_grid_f_IR(f_grids_Skellam_pNk[["f_grid_2019_NL"]])
plot_grid_f_IR(f_grids_Skellam_pNk[["f_grid_2019_AL"]])
plot_grid_f_IR(f_grids_Skellam_pNk[["f_grid_2018_NL"]])
plot_grid_f_IR(f_grids_Skellam_pNk[["f_grid_2018_AL"]])

