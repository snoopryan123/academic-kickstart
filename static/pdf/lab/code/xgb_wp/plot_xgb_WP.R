
###################
WP = TRUE
source("00_main.R") 
###################

############################
### load data and models ###
############################

MODEL_TYPE = "XGB"
source("models.R")
### make sure to first train and save these XGB models in `param_tuning.R`
model_names_list_ = c("xgb_wp_110_6", "xgb_wp_Baldwin")

############################
### visualize the models ###
############################

# lm_ep00 <- load_lm("ep00.rds")

plot_s_vs_t <- function(model_fit, timeZone="mid", printNums=FALSE) {
  
  if (timeZone == "mid") {
    # ss = seq(-24,24,by=3)
    ss = seq(-14,14,by=1)
    ts = c( seq(3480,120,by=-240))
  } else if (timeZone == "end") {
    # ss = seq(-24,24,by=3)
    ss = seq(-14,14,by=1)
    ts = c( seq(60,5,by=-5) )
    # ts = c( seq(120,15,by=-15), 3)
  } else {
    stop("FIXME: please specify timeZone")
  }
  
  plot_set = tibble()
  for (s in ss) {
    for (t in ts) {
      df_st = tibble(
        score_differential = s, game_seconds_remaining = t,
        half_seconds_remaining = ifelse(game_seconds_remaining > 1800, game_seconds_remaining - 1800, game_seconds_remaining), 
        half = ifelse(game_seconds_remaining > 1800, 1, 2), era_A = 4,
        posteam_spread = 0, yardline_100 = rep(1:99,4), 
        receive_2h_ko = c(rep(0,99), rep(0,99), rep(1,99), rep(1,99)), home = c(rep(0,99), rep(1,99), rep(0,99), rep(1,99)), 
        posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3,
        down = 1, ydstogo = ifelse(yardline_100 < 10, yardline_100, 10),
        down1=1, down2=0, down3=0, down4=0, utm=as.numeric(half_seconds_remaining<=120),
        elapsed_share = (3600 - game_seconds_remaining) / 3600,
        spread_time = posteam_spread*exp(-4*elapsed_share), 
        Diff_Time_Ratio = score_differential / (exp(-4*elapsed_share))
      ) %>% mutate(
        # ep00 = predict(lm_ep00, .),
        # e_score_diff = ep00 + score_differential,
        # eScoreTimeRatio = (e_score_diff)/(game_seconds_remaining + 1),
        scoreTimeRatio = compute_scoreTimeRatio(score_differential, game_seconds_remaining),
      )
      plot_set = bind_rows(plot_set, df_st)
    }
  }
  
  if (MODEL_TYPE=="XGB") {
    pred_wp_0 = bind_cols(
      wp = predict_probs_xgb(model_fit, plot_set, xgb_features, wp=TRUE),
      plot_set
    )
  } else if (MODEL_TYPE=="GAM") {
    pred_wp_0 = bind_cols(
      wp = predict_gam(model_fit, plot_set),
      plot_set
    )
  }
  pred_wp = pred_wp_0 %>% group_by(score_differential, game_seconds_remaining) %>% summarise(wp = mean(wp), .groups = "drop")
  
  plot_ = pred_wp %>%
    mutate(wpr = round(wp,2)) %>%
    ggplot(aes(y = game_seconds_remaining, x = score_differential)) +
    geom_vline(aes(xintercept=0), color="black") +
    geom_tile(aes(fill=wp), color = "black", linetype = 1) +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"), breaks=seq(0,1,by=0.2), limits=c(0,1)) +
    scale_x_continuous(breaks=ss) +
    scale_y_continuous(breaks=ts) +
    labs(fill=" win\n probability") +
    xlab("score differential") + ylab("game seconds remaining")
  if (printNums) plot_ = plot_ + geom_text(aes(label=wpr)) 
  return(plot_)
}

plot_varyS <- function(model_fit) {
  ss = seq(-24,24,by=3)
  ts = c(3600-120, 1800-120, 480, 120)
  plot_set = tibble()
  for (s in ss) {
    for (t in ts) {
      df_st = tibble(
        score_differential = s, game_seconds_remaining = t,
        half_seconds_remaining = ifelse(game_seconds_remaining > 1800, game_seconds_remaining - 1800, game_seconds_remaining), 
        half = ifelse(game_seconds_remaining > 1800, 1, 2), era_A = 4,
        posteam_spread = 0, yardline_100 = rep(1:99,4), 
        receive_2h_ko = c(rep(0,99), rep(0,99), rep(1,99), rep(1,99)), home = c(rep(0,99), rep(1,99), rep(0,99), rep(1,99)), 
        posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3,
        down = 1, ydstogo = ifelse(yardline_100 < 10, yardline_100, 10),
        down1=1, down2=0, down3=0, down4=0, utm=as.numeric(half_seconds_remaining<=120),
        elapsed_share = (3600 - game_seconds_remaining) / 3600,
        spread_time = posteam_spread*exp(-4*elapsed_share), 
        Diff_Time_Ratio = score_differential / (exp(-4*elapsed_share))
      ) %>% mutate(
        # ep00 = predict(lm_ep00, .),
        # e_score_diff = ep00 + score_differential,
        # eScoreTimeRatio = (e_score_diff)/(game_seconds_remaining + 1),
        scoreTimeRatio = compute_scoreTimeRatio(score_differential, game_seconds_remaining),
      )
      plot_set = bind_rows(plot_set, df_st)
    }
  }
  
  if (MODEL_TYPE=="XGB") {
    pred_wp_0 = bind_cols(
      wp = predict_probs_xgb(model_fit, plot_set, xgb_features, wp=TRUE),
      plot_set
    )
  } else if (MODEL_TYPE=="GAM") {
    pred_wp_0 = bind_cols(
      wp = predict_gam(model_fit, plot_set),
      plot_set
    )
  }
  pred_wp = pred_wp_0 %>% 
    group_by(score_differential, yardline_100, game_seconds_remaining) %>% 
    summarise(wp = mean(wp), .groups = "drop") %>%
    arrange(score_differential, game_seconds_remaining)
  
  my_palette <- c(
    rev(brewer.pal(name="Greens",n=9)[3:9]),
    # brewer.pal(name="Purples",n=9)[5:9],
    brewer.pal(name="Blues",n=9)[4:9],
    rev(brewer.pal(name="Reds",n=9)[3:9])
    # rev(brewer.pal(name="BrBG",n=9)[7:9]),
    # rev(brewer.pal(name="BrBG",n=9)[1:3]),
    # rev(brewer.pal(name="YlOrBr",n=9)[4:8])
  )
  
  plot_ = pred_wp %>%
    mutate(score_differential = fct_reorder(factor(score_differential), -score_differential) ) %>%
    mutate(game_seconds_remaining = fct_reorder(factor(game_seconds_remaining), -game_seconds_remaining)) %>%
    ggplot(aes(x = yardline_100, y = wp, color=score_differential)) +
    facet_wrap(~game_seconds_remaining, nrow=1, labeller = as_labeller(
      c(
        "120" = "end (2 min. remaining)",
        "480" = "fourth quarter (8 min. remaining)",
        "1680" = "middle (28 min. remaining)",
        "3480" = "beginning (58 min. remaining)"
      )
    )) +
    geom_hline(yintercept=0.5, size=0.5, color="gray70") +
    geom_line(size=1) +
    scale_colour_manual(values = my_palette) +
    xlab("yardline") + ylab("win probability") +
    labs(color=" score\n differential") +
    scale_x_continuous(breaks=seq(0,100,by=10)) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))
  plot_
  
  return(plot_)
}

plot_varyT <- function(model_fit, N=16) {
  ss = c(-7,-3,3,7)
  # ts = c(seq(0,1-120/1800,length=N), 1-60/1800, 1-30/1800, 1-10/1800)
  ts = c(seq(3480,120,by=-240))
  plot_set = tibble()
  for (s in ss) {
    for (t in ts) {
      df_st = tibble(
        score_differential = s, game_seconds_remaining = t,
        half_seconds_remaining = ifelse(game_seconds_remaining > 1800, game_seconds_remaining - 1800, game_seconds_remaining), 
        half = ifelse(game_seconds_remaining > 1800, 1, 2), era_A = 4,
        posteam_spread = 0, yardline_100 = rep(1:99,4), 
        receive_2h_ko = c(rep(0,99), rep(0,99), rep(1,99), rep(1,99)), home = c(rep(0,99), rep(1,99), rep(0,99), rep(1,99)), 
        posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3,
        down = 1, ydstogo = ifelse(yardline_100 < 10, yardline_100, 10),
        down1=1, down2=0, down3=0, down4=0, utm=as.numeric(half_seconds_remaining<=120),
        elapsed_share = (3600 - game_seconds_remaining) / 3600,
        spread_time = posteam_spread*exp(-4*elapsed_share), 
        Diff_Time_Ratio = score_differential / (exp(-4*elapsed_share))
      ) %>% mutate(
        # ep00 = predict(lm_ep00, .),
        # e_score_diff = ep00 + score_differential,
        # eScoreTimeRatio = (e_score_diff)/(game_seconds_remaining + 1),
        scoreTimeRatio = compute_scoreTimeRatio(score_differential, game_seconds_remaining),
      )
      plot_set = bind_rows(plot_set, df_st)
    }
  }
  
  if (MODEL_TYPE=="XGB") {
    pred_wp_0 = bind_cols(
      wp = predict_probs_xgb(model_fit, plot_set, xgb_features, wp=TRUE),
      plot_set
    )
  } else if (MODEL_TYPE=="GAM") {
    pred_wp_0 = bind_cols(
      wp = predict_gam(model_fit, plot_set),
      plot_set
    )
  }
  pred_wp = pred_wp_0 %>% 
    group_by(score_differential, yardline_100, game_seconds_remaining) %>% 
    summarise(wp = mean(wp), .groups = "drop") %>%
    arrange(score_differential, game_seconds_remaining)  
  
  my_palette <- c(
    rev(brewer.pal(name="Greens",n=9)[3:9]),
    # brewer.pal(name="Purples",n=9)[5:9],
    brewer.pal(name="Blues",n=9)[4:9],
    rev(brewer.pal(name="Reds",n=9)[3:9])
    # rev(brewer.pal(name="BrBG",n=9)[7:9]),
    # rev(brewer.pal(name="BrBG",n=9)[1:3]),
    # rev(brewer.pal(name="YlOrBr",n=9)[4:8])
  )
  my_palette <- c(
    rev(brewer.pal(name="Blues",n=9)[3:9]),
    brewer.pal(name="Purples",n=9)[3:9],
    rev(brewer.pal(name="Reds",n=9)[3:9])
    # rev(brewer.pal(name="BrBG",n=9)[7:9]),
    # rev(brewer.pal(name="BrBG",n=9)[1:3]),
    # rev(brewer.pal(name="YlOrBr",n=9)[4:8])
  )
  
  plot_ = pred_wp %>%
    mutate(score_differential = fct_reorder(factor(score_differential), -score_differential) ) %>%
    mutate(game_seconds_remaining = fct_reorder(factor(game_seconds_remaining), -game_seconds_remaining)) %>%
    ggplot(aes(x = yardline_100, y = wp)) +
    facet_wrap(~score_differential, nrow=1, 
               labeller = as_labeller(
                 c(
                   "-7" = "score differential = -7",
                   "-3" = "score differential = -3",
                   "3" = "score differential = 3",
                   "7" = "score differential = 7"
                 )
               )
    ) +
    geom_hline(yintercept=0.5, size=0.5, color="gray70") +
    geom_line(size=1, aes(color=game_seconds_remaining)) +
    scale_colour_manual(values = my_palette) +
    xlab("yardline") + ylab("win probability") +
    labs(color=" game\n seconds\n remaining") +
    scale_x_continuous(breaks=seq(0,100,by=10)) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))
  plot_
  
  return(plot_)
}

plot_varySpread <- function(model_fit) {
  pspreads = -10:10
  time_rems = c(2700, 900)
  plot_set = tibble()
  for (ps in pspreads) {
    for (t in time_rems) {
      plot_set_ = tibble(
        game_seconds_remaining = t,
        posteam_spread = ps, 
        score_differential = 0, 
        half_seconds_remaining = ifelse(game_seconds_remaining > 1800, game_seconds_remaining - 1800, game_seconds_remaining), 
        half = ifelse(game_seconds_remaining > 1800, 1, 2), era_A = 4,
        yardline_100 = rep(1:99,4), 
        receive_2h_ko = c(rep(0,99), rep(0,99), rep(1,99), rep(1,99)), home = c(rep(0,99), rep(1,99), rep(0,99), rep(1,99)), 
        posteam_timeouts_remaining = 3, defteam_timeouts_remaining = 3,
        down = 1, ydstogo = ifelse(yardline_100 < 10, yardline_100, 10),
        down1=1, down2=0, down3=0, down4=0, utm=as.numeric(half_seconds_remaining<=120),
        elapsed_share = (3600 - game_seconds_remaining) / 3600,
        spread_time = posteam_spread*exp(-4*elapsed_share), 
        Diff_Time_Ratio = score_differential / (exp(-4*elapsed_share))
      ) %>% mutate(
        # ep00 = predict(lm_ep00, .),
        # e_score_diff = ep00 + score_differential,
        # eScoreTimeRatio = (e_score_diff)/(game_seconds_remaining + 1),
        scoreTimeRatio = compute_scoreTimeRatio(score_differential, game_seconds_remaining),
      )
      plot_set = bind_rows(plot_set, plot_set_)
    }
  }
  
  if (MODEL_TYPE=="XGB") {
    pred_wp_0 = bind_cols(
      wp = predict_probs_xgb(model_fit, plot_set, xgb_features, wp=TRUE),
      plot_set
    )
  } else if (MODEL_TYPE=="GAM") {
    pred_wp_0 = bind_cols(
      wp = predict_gam(model_fit, plot_set),
      plot_set
    )
  }
  
  pred_wp = pred_wp_0 %>% 
    group_by(posteam_spread, yardline_100, game_seconds_remaining) %>% 
    summarise(wp = mean(wp), .groups = "drop") %>%
    arrange(posteam_spread, game_seconds_remaining)

  my_palette <- c(
    brewer.pal(name="Purples",n=9)[3:9],
    rev(brewer.pal(name="Blues",n=9)[3:9]),
    brewer.pal(name="Reds",n=9)[3:9]
  )
  
  plot_ = pred_wp %>%
    mutate(game_sec_rem_str = paste0(game_seconds_remaining, " game sec. remaining")) %>%
    mutate(posteam_spread = fct_reorder(factor(posteam_spread), -posteam_spread) ) %>%
    mutate(game_seconds_remaining = fct_reorder(factor(game_sec_rem_str), -game_seconds_remaining)) %>%
    ggplot(aes(x = yardline_100, y = wp, color=posteam_spread)) +
    facet_wrap(~game_sec_rem_str, nrow=1, ) +
    geom_hline(yintercept=0.5, size=0.5, color="gray70") +
    geom_line(size=1) +
    scale_colour_manual(values = my_palette) +
    xlab("yardline") + ylab("win probability") +
    labs(color=" point\n spread") +
    scale_x_continuous(breaks=seq(0,100,by=10)) +
    scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.1))
  plot_
  
  return(plot_)
}



# {
# j=1
for (j in 1:length(model_names_list_)) {
  
  model_name = model_names_list_[j]
  if (MODEL_TYPE == "GAM") {
    train_data_ = if (str_detect(model_name, "110")) data_full_110_WP else data_full_WP
    model_fit = get(paste0("fit_", model_name))(train_data_)
  } else if (MODEL_TYPE == "XGB") {
    model_fit = xgb.load(paste0("",model_name,".xgb"))
    xgb_features = get(paste0(model_name, "_features"))
  }
  print(model_fit)
  
  plot_mid_svt = plot_s_vs_t(model_fit, timeZone="mid", printNums=TRUE)
  plot_mid_svt
  # plot_mid_svt1 = plot_s_vs_t(model_fit, timeZone="mid", printNums=FALSE)
  # plot_mid_svt
  
  plot_end_svt = plot_s_vs_t(model_fit, timeZone="end", printNums=TRUE)
  plot_end_svt
  # plot_end_svt1 = plot_s_vs_t(model_fit, timeZone="end", printNums=FALSE)
  # plot_end_svt
  
  ggsave(paste0("plot_svt_mid_",model_name,".png"), plot_mid_svt, width=19, height=6)
  ggsave(paste0("plot_svt_end_",model_name,".png"), plot_end_svt, width=19, height=6)
  
  
  plot_varyS_ = plot_varyS(model_fit)
  plot_varyS_
  
  plot_varyT_ = plot_varyT(model_fit)
  plot_varyT_
  
  plot_varySpread_ = plot_varySpread(model_fit)
  plot_varySpread_
  
  ggsave(paste0("plot_svt_mid_",model_name,".png"), plot_mid_svt, width=19, height=6)
  ggsave(paste0("plot_svt_end_",model_name,".png"), plot_end_svt, width=19, height=6)
  # ggsave(paste0("plot_svt_mid1_",model_name,".png"), plot_mid_svt1, width=9, height=6)
  # ggsave(paste0("plot_svt_end1_",model_name,".png"), plot_end_svt1, width=9, height=6)
  ggsave(paste0("plot_SvaryYdl_",model_name,".png"), plot_varyS_, width=21, height=6)
  ggsave(paste0("plot_STvaryYdl_",model_name,".png"), plot_varyT_, width=21, height=6)
  ggsave(paste0("plot_varySpread_",model_name,".png"), plot_varySpread_, width=12, height=6)
  
}


