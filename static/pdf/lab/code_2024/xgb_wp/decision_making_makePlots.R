
################
source("00_main.R")
################

options(warn=-1)
###options(warn=0)

### LOAD 4TH DOWN PLOTTING FUNCTIONS
source("decision_making_bootstrap_main.R")
source("decision_making_loadModels.R")
source("decision_making_functions.R")

#####################
# MAKE_GIFS_TOO = TRUE
MAKE_GIFS_TOO = FALSE

WILL_EVAL_OG_METHOD = FALSE
EVAL_WP = TRUE
EVAL_WP_ONLY = TRUE

# WILL_EVAL_OG_METHOD = FALSE
# EVAL_WP = TRUE
# EVAL_WP_ONLY = TRUE

# WILL_EVAL_SE_TODAY = TRUE
# WILL_EVAL_SE_ONLY__TODAY = TRUE
WILL_EVAL_SE_TODAY = TRUE
WILL_EVAL_SE_ONLY__TODAY = FALSE
# WILL_EVAL_SE_TODAY = FALSE
# WILL_EVAL_SE_ONLY__TODAY = FALSE
#####################

#####################
### Example Plays ###
#####################

# ### find some example plays
# ex_plays_full = data_full_0A %>% filter(down==4) %>%
#   filter(half_seconds_remaining >= 1300) %>%
#   filter(season >= 2012) %>%
#   select(row_idx, game_id, week, posteam, defteam, season, oq_ot_0_total_sum, dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum, oq_dt_0_sum, dq_ot_0_againstPass_sum dq_ot_0_againstRun_sum)
# data.frame(ex_plays_full %>% filter(oq_ot_0_total_sum <= -0.5 & dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum <= -0.5))
# data.frame(ex_plays_full %>% filter(oq_ot_0_total_sum >= 0.5 & dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum >= 0.5))
# data.frame(ex_plays_full %>% filter(oq_ot_0_total_sum >= 0.5 & dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum <= -0.5))
# data.frame(ex_plays_full %>% filter(oq_ot_0_total_sum <= -0.5 & dq_dt_0_againstPass_sum dq_dt_0_againstRun_sum >= 0.5))
# data.frame(ex_plays_full %>% filter(abs(oq_ot_0_total_sum) <= 0.2 & abs(oq_dt_0_sum) <= 0.2 & week >= 10))

# ###
# explays1 = data_full_0A %>% filter(down==4) %>% filter(season >= 2012) %>%
#   select(row_idx, game_id, week, posteam, defteam, season, posteam_spread, score_differential, game_seconds_remaining)
# View(explays1)

# play_idxs = c(531987, 660986, 767858, 775775, 765904, 710161, 760684, 721265, 733206, 643380)
# play_idxs = c(531987, 660986, 767858, 775775) #, 765904, 710161, 760684, 721265, 733206, 643380)
# play_idxs = c(531987, 660986) 
# play_idxs = c(531987, 660986, 767858, 775775, 709544, 684381, 833737, 796009, 855454, 858240, 788893)



# play_idxs = c(531987, 720008, 744330,    713189, 594436, 843142, 831546,    815661, 632706, 656302, 766604,    554262, 621001, 799918, 842155)
# play_idxs = c(885738, 877222, 875978, 895095) 
# play_idxs = c(876754, )
# play_idxs = c(867213, 859866, 888428, 877789)

play_idxs = c(877205, 877797, 878865, 883589, 888428, 888822)

# 888598 860533 880711 883540 865345 863419 892421 860973, 898019 878240 893775 865772)

plays = bind_rows(
  # data_full_0A %>% filter(row_idx %in% play_idxs) %>% mutate(preset_play=TRUE) %>% mutate(
  #   qbq_ot_0_sum  = 0, oq_rot_0_total_sum = 0, dq_dt_0_againstPass_sum = 0, dq_dt_0_againstRun_sum = 0,
  #   qbq_dt_0_sum = 0, oq_rdt_0_sum = 0, dq_ot_0_againstPass_sum = 0, dq_ot_0_againstRun_sum = 0,
  #   kq_0_sum_std = 0, pq_0_sum_std = 0, posteam_spread = 0, spread_time = 0,
  # ),
  data_full_0A %>% filter(row_idx %in% play_idxs) %>% mutate(preset_play=FALSE)
)
plays = plays %>% arrange(factor(row_idx, levels = play_idxs))
plays

#########################################
### create full decision making plots ###
#########################################

{
  # i = 3
  # og_method = FALSE
  # SE = FALSE
  
  grid_to_plot = expand_grid(
    tibble(i = 1:nrow(plays)),
    tibble(SE = c(TRUE,FALSE)),
    tibble(og_method = c(TRUE,FALSE)),
    wp = c(TRUE,FALSE)
  ) %>% filter(!(SE & og_method)) %>% filter(!(wp & og_method))
  grid_to_plot
  
  if (WILL_EVAL_SE_ONLY__TODAY) {
    grid_to_plot = grid_to_plot %>% filter(SE)
  } else if (!WILL_EVAL_SE_TODAY) {
    grid_to_plot = grid_to_plot %>% filter(SE == FALSE)
  }
  if (!WILL_EVAL_OG_METHOD) {
    grid_to_plot = grid_to_plot %>% filter(og_method == FALSE)
  }
  if (!EVAL_WP) {
    grid_to_plot = grid_to_plot %>% filter(!wp)
  }
  if (EVAL_WP_ONLY) {
    # grid_to_plot = grid_to_plot %>% filter(wp)
    grid_to_plot = grid_to_plot %>% filter(wp | i==2 | i==6)
  }
  grid_to_plot = grid_to_plot %>% group_by(i) %>% mutate(first_in_group = row_number() == 1) %>% ungroup()
}
grid_to_plot


####################################
# for (j in 1:nrow(grid_to_plot)) {
# for (j in 4:6) {
for (j in 1:nrow(grid_to_plot)) {
  
  ############## play description ############## 
  i = grid_to_plot[j,]$i
  og_method = grid_to_plot[j,]$og_method
  SE = grid_to_plot[j,]$SE
  wp = grid_to_plot[j,]$wp
  first_in_group = grid_to_plot[j,]$first_in_group
  print(paste0("iter ", j, " of ", nrow(grid_to_plot), "; i = ", i, ", og_method = ", og_method, ", SE = ", SE))
  play = plays[i,]
  preset_play_i = play$preset_play
  
  if (play$down == 4) { # actual 4th down 
    play = play %>% mutate(
      decision_actual = case_when(
        !is.na(kicker_player_name) ~ "FG",
        !is.na(punter_player_name) ~ "Punt",
        TRUE ~ "Go"
      )) 
  }
  
  confounders_dataset_i = play %>% select(
    season, posteam, home_team, roof, posteam_timeouts_remaining, defteam_timeouts_remaining,
    half_seconds_remaining, half_sec_rem_std, era_A, receive_2h_ko, spread_time, home, game_seconds_remaining, 
    Diff_Time_Ratio, score_differential, elapsed_share
  )
  
  qbqot_i = play$qbq_ot_0_sum
  oqrot_i = play$oq_rot_0_total_sum
  dqpdt_i = play$dq_dt_0_againstPass_sum 
  dqrdt_i = play$dq_dt_0_againstRun_sum 
  qbqdt_i = play$qbq_dt_0_sum
  oqrdt_i = play$oq_rdt_0_sum
  dqpot_i = play$dq_ot_0_againstPass_sum 
  dqrot_i = play$dq_ot_0_againstRun_sum 
  ps_i = play$posteam_spread
  kicker_i = play$kicker_name
  kq_i = play$kq_0_sum_std
  punter_i = play$punter_name
  pq_i = play$pq_0_sum_std
  qb_i = play$qb_name
  
  
  play_desc_i = paste0(play$down, "th down and ", play$ydstogo, " yards to go at the ", play$yardline_100, " yardline \n",
                       "this play features ", play$posteam, " on offense vs. ", play$defteam, 
                       " \n the spread is ", ps_i,
                       ". \n The ", play$posteam, " kicker ", kicker_i, " has a kicker quality of ", round(kq_i,3),
                       ". \n The ", play$posteam, " punter ", punter_i, " has a punter quality of ", round(pq_i,3),
                       
                       ". \n ", qb_i, " has a QB quality of ", round(qbqot_i, 3),
                       ", \n ", play$posteam, " has an offensive non-QB quality of ", round(oqrot_i,3),
                       ", \n a def. quality against the pass of ", round(dqpot_i,3),
                       ", \n and a def. quality against the run of ", round(dqrot_i,3), ". ",
                       
                       ", \n The defensive team is ", play$defteam, 
                       ", \n their QB is ", play$qb_name_dt, " who has a QB quality of ", round(qbqdt_i,3),
                       ", \n a non-QB off. quality of ", round(oqrdt_i,3),
                       " \n  a def. quality against the pass of ", round(dqpdt_i,3), 
                       " \n and a def. quality against the run of ", round(dqrdt_i,3), ". \n",
                       " \n game_id ", play$game_id,
                       " \n row_idx ", play$row_idx 
                       )
  print(play_desc_i)
  print(confounders_dataset_i)
  
  play_str = paste(play_desc_i, "\n ", paste0(colnames(confounders_dataset_i)," ",as.character(confounders_dataset_i[1,]), collapse="\n"))
  if (first_in_group) {
    textPlot(paste0("", "plot_decisions_play_", i, "desc"), play_str)
  }
  
  ############## make plots ##############
  ddf_i = get_full_decision_making(play_df=play, wp=wp, og_method=og_method, SE=SE, coachBaseline=play$down==4)
  # ddf_i = get_full_decision_making(kq=kq_i, pq=pq_i, pspread=ps_i,
  #                                  qbqot=qbqot_i, oqrot=oqrot_i, dqpdt=dqpdt_i, dqrdt=dqrdt_i,
  #                                  qbqdt=qbqdt_i, oqrdt=oqrdt_i, dqpot=dqpot_i, dqrot=dqrot_i,
  #                                  confounders_dataset_i, wp=wp, og_method=og_method, SE=SE)
  
  plot_prefix = paste0("plot_decisions_play_", i, "_wp", wp, "_og", og_method, "_se", SE, "_")
  if (!SE) {
    heatmap_i = plot_4thDownHeatmap(ddf_i, wp=wp, og_method=og_method, SE=SE, ydl=play$yardline_100, ytg=play$ydstogo)
    ggsave(paste0("", plot_prefix, ".png"), heatmap_i, width=9, height=7)
  } else {
    heatmap_i = plot_4thDownHeatmap(ddf_i, wp=wp, og_method=og_method, SE=SE, ydl=play$yardline_100, ytg=play$ydstogo)
    ggsave(paste0("", plot_prefix, ".png"), heatmap_i, width=9, height=7)
  }

  ### 
  if (SE) {
    # browser()
    decision_df_i = get_decision(play$yardline_100, play$ydstogo, ddf_i, include_uncertainty=TRUE)
    aaa = plot_gt_4th_down_summary(play, ddf_i, decision_df=decision_df_i, SE=SE, wp=wp)
    # browser()
    gtsave(aaa,  paste0("plots/", plot_prefix, "summary", ".png"))
    # browser()
    # ### combined data frame
    # library(htmlTable)
    # library(kableExtra)
    # plot_combined_0 = concatHtmlTables(
    #   list(
    #     htmlTable(a1),
    #     htmlTable(a2),
    #     htmlTable(a3)
    #   ), headers = ""
    #   # list(htmlTable(a3)),
    # )
    # # plot_combined_0
    # save_kable(plot_combined_0, file = paste0("plots/", plot_prefix, "summary", ".png"), zoom=2)
  }
  


  if (SE) {
    # plot_ytg1_i = plot_Vs(ddf_i, ytg=1, SE=SE, wp=wp)
    plot_ytg4_i = plot_Vs(ddf_i, ytg=4, SE=SE, wp=wp)
    # plot_ytg7_i = plot_Vs(ddf_i, ytg=7, SE=SE, wp=wp)
    plot_ytg10_i = plot_Vs(ddf_i, ytg=10, SE=SE, wp=wp)
    
    # ggsave(paste0("plots_ytg/", plot_prefix, "_ytg", 1, ".png"), plot_ytg1_i, width=8, height=6)
    ggsave(paste0("plots_ytg/", plot_prefix, "_ytg", 4, ".png"), plot_ytg4_i, width=8, height=6)
    # ggsave(paste0("plots_ytg/", plot_prefix, "_ytg", 7, ".png"), plot_ytg7_i, width=8, height=6)
    ggsave(paste0("plots_ytg/", plot_prefix, "_ytg", 10, ".png"), plot_ytg10_i, width=8, height=6)
  }

  if (!SE & MAKE_GIFS_TOO & !og_method) {
    ### make gifs

    time_breaks = seq(0,1,length=21)
    for (t in 1:length(time_breaks)) {
      half_sec_rem_std = time_breaks[t]
      half_seconds_remaining = round((1-half_sec_rem_std)*1800)
      confounders_dataset_i$half_sec_rem_std = half_sec_rem_std
      confounders_dataset_i$half_seconds_remaining = half_seconds_remaining

      ddf_it = get_full_decision_making(kq=kq_i, pq=pq_i, pspread=ps_i,
                                        qbqot=qbqot_i, oqrot=oqrot_i, dqpdt=dqpdt_i, dqrdt=dqrdt_i,
                                        qbqdt=qbqdt_i, oqrdt=oqrdt_i, dqpot=dqpot_i, dqrot=dqrot_i,
                                        confounders_dataset_i, og_method=og_method, SE=SE)
      plot_prefix_it = paste0(plot_prefix, "t", t)
      plot_title_it = paste0("half seconds remaining = ", half_seconds_remaining)
      if (!SE) {
        heatmap_i = plot_4thDownHeatmap(ddf_it, wp=wp, og_method=og_method, SE=SE) + labs(title=plot_title_it)
        ggsave(paste0("plots/", plot_prefix_it, ".png"), heatmap_i, width=8, height=6)
      } else {
        heatmap_i = plot_4thDownHeatmap(ddf_it, wp=wp, og_method=og_method, SE=SE) + labs(title=plot_title_it)
        ggsave(paste0("plots/", plot_prefix, ".png"), heatmap_i, width=8, height=11)
      }
    }

    ## library(magick)
    ## can't install magick on HPCC...
    filenames <- paste0("plots/", plot_prefix, "t", 1:length(time_breaks), ".png")
    m <- magick::image_read(filenames[1])
    for (i in 2:length(time_breaks)) {
      m <- c(m, magick::image_read(filenames[i]))
    }
    m <- magick::image_animate(m, fps = 2, loop = 1, dispose = "previous")
    magick::image_write(m, paste0("plots/", plot_prefix, ".gif"))
    file.remove(filenames)
  }

  
}









