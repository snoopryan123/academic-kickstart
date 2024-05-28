
# Adapted from:
# Nylon Calculus: Grouping players by offensive role, again | Todd Whitehead | May 29, 2019
# https://fansided.com/2019/05/29/nylon-calculus-grouping-players-offensive-role-again/

### packages
library(tidyverse)
library(hoopR)
library(flexclust)
library(assertthat)
library(RColorBrewer)
library(latex2exp)
library(cluster)
library(fpc)
library(factoextra)

### plotting pre-sets
theme_set(theme_bw())
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=20),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20)
) 

####################
### Clustering 1 ###
####################

### additional columns relevant for clustering
df_off = read_csv("data/data_synergyPlayType.csv", show_col_types = F)
df_off
names(df_off)
dim(df_off)

### create new covariates for 1st clustering
df_off = 
  df_off %>%
  mutate(
    FGA_freq_PRRollMan_X =  FGA_freq_PRRollMan_OG,
    FGA_freq_Postup_X =  FGA_freq_Postup_OG,
    FGA_freq_Cut_X =  FGA_freq_Cut_OG,
    FGA_freq_OffRebound_X =  FGA_freq_OffRebound_OG,
    FGA_freq_PRBallHandler_X =  FGA_freq_PRBallHandler_OG,
    FGA_freq_Isolation_X =  FGA_freq_Isolation_OG,
    FGA_freq_SpotupOffScreenOrHandoff_X =  FGA_freq_Spotup_OG + FGA_freq_OffScreenOrHandoff_OG,
  ) 

### training set 
df_off_train = df_off 

### matrix for K means; train only on TRAINING SEASONS
col_idxs_fga_freq = which(startsWith(names(df_off_train), "FGA_freq") & endsWith(names(df_off_train), "_X"))
print(names(df_off_train[,col_idxs_fga_freq]))
mat_train = data.matrix(df_off_train[,col_idxs_fga_freq])
rownames(mat_train) = df_off_train$player_szn
mat_total = data.matrix(df_off[,col_idxs_fga_freq])
rownames(mat_total) = df_off$player_szn

### choose number of clusters for K means
set.seed(387397)
fviz_nbclust(mat_train, FUNcluster = kmeans, method="wss")
K1 = 3 ### num clusters via elbow method

### Hard Clustering: K means
set.seed(387397)
km1 <- flexclust::kcca(mat_train, K1)
print(km1)

### names of the offensive clusters
df_off_clust_names = 
  df_off %>%
  mutate(cluster1 = predict(km1, mat_total)) %>%
  group_by(cluster1) %>%
  summarise(
    mean_freq_spotup = mean(FGA_freq_Spotup_OG),
    mean_freq_PRBallHandler = mean(FGA_freq_PRBallHandler_OG),
  ) %>%
  mutate(
    cluster1s = case_when(
      mean_freq_PRBallHandler == max(mean_freq_PRBallHandler) ~ "ball-handler",
      mean_freq_spotup == max(mean_freq_spotup) ~ "wing",
      TRUE ~ "big"
    )
  )
df_off_clust_names

### offensive clusters 
df_off_clust = 
  df_off %>%
  mutate(cluster1 = predict(km1, mat_total)) %>%
  left_join(df_off_clust_names %>% select(-c(all_of(starts_with("mean_freq"))))) 

### endOfSznClusterGroup
df_off_clust = df_off_clust %>% mutate(endOfSznClusterGroup = factor(cluster1s, levels = c("ball-handler", "wing", "big")))

# View(df_off_clust %>% arrange(cluster1))
# View(df_off_clust %>% select(PLAYER_NAME, szn, cluster1) %>% arrange(cluster1))

### visualize cluster 1
plot_off_clust1 = 
  df_off_clust %>%
  pivot_longer(cols = starts_with("FGA_freq") & !ends_with("_X"), names_to = "play_type", values_to = "FGA_freq") %>%
  mutate(play_type = str_remove_all(play_type, "FGA_freq_")) %>%
  group_by(cluster1s, play_type) %>%
  summarise(mean_FGA_freq = mean(FGA_freq)) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_col(aes(x = play_type, y = mean_FGA_freq, fill=factor(cluster1s)), 
           position = "dodge", color="black", width = 0.65) +
  scale_fill_brewer(name = "initial\noffensive\nclustering", palette = "Spectral") +
  xlab("scoring attempt play type") +
  ylab("mean FGA frequency") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=15))
# plot_off_clust1
ggsave("plots/plot_offClustGroups.png", plot_off_clust1, width=8, height = 5)

###############################################################
### Visualize the end-of-season clustering: frequency plots ###
###############################################################

### Final end-of-season offensive cluster dataframe for visualization
df_off_clust_F = df_off_clust
df_off_clust_F = df_off_clust_F %>% mutate(endOfSznCluster = endOfSznClusterGroup) #FIXME

plotszns = 2022
# plotszns = c(2013, 2017, 2022)
for (plotszn in plotszns) {
  ex_player_names = c(
    "Chris Paul", "VanVleet", "Brunson", "Haliburton", "Cunningham", "Jordan Poole",
    "Durant", "Westbrook", "Kawhi", "LeBron James", "Shai", "Luka", "Gobert", "Embiid",
    "Hield", "Harden","Eric Gordon","Bullock","Crowder","Finney","Thybulle","Jokic",
    "Nikola Vucevic","Draymond","Anthony Davis","DeAngelo","Conley","Bullock","Jamal Murray",
    "Klay Thompson", "Curry", "Austin Reaves", "Darius Garland", "Ja Morant",
    "Bam Adebayo","Capela","Beal","Bertans","Jokic","Tatum", "Korver", "Danny Green", "Mikal"
  )
  
  ### visualize example players area charts
  plot_off_clust_exs0 = 
    df_off_clust_F %>%
    filter(szn == plotszn) %>%
    mutate(
      marked = str_detect(PLAYER_NAME,paste0(ex_player_names, collapse = "|"))
    ) %>%
    group_by(endOfSznCluster) %>%
    filter(!(PLAYER_NAME %in% c(
      "Otto Porter Jr.", "Markelle Fultz"
    ))) %>%
    arrange(-marked) %>%
    slice_head(n = 25) %>%
    ungroup() 
  plot_off_clust_exs0

  plot_off_clust_exs = 
    plot_off_clust_exs0 %>%
    pivot_longer(
      cols = starts_with("FGA_freq") & ends_with("_OG")
      ,names_to = "play_type", values_to = "FGA_freq") %>%
    mutate(
      play_type = str_remove_all(play_type, "FGA_freq_"),
      play_type = str_remove_all(play_type, "_OG"),
      play_type = factor(play_type, levels = rev(c(
        "Cut", "PRRollMan", "OffRebound", "Postup", 
        "Spotup", "OffScreenOrHandoff", "Isolation", "PRBallHandler"
      )))
    ) %>%
    ggplot(aes()) +
    facet_wrap(~ endOfSznCluster + PLAYER_NAME , ncol = 5) +
    geom_col(aes(x = "", y = FGA_freq, fill=play_type), color = "black") +
    scale_fill_brewer(name = "scoring\nattempt\nplay\ntype", palette = "Spectral", direction=-1) +
    coord_polar(theta="y", start=0) +
    xlab("area is proportional to the frequency of a scoring attempt play type") +
    coord_flip() +
    labs(title = hoopR::year_to_season(plotszn)) +
    theme(
      # strip.background = element_rect(fill=plot_off_clust_exs0$facet_color),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
    )
  # plot_off_clust_exs
  ggsave(paste0("plots/plot_off_clust_exs_",plotszn,".png"), plot_off_clust_exs, 
         width=20, height = 30)
}

########################################################
### Visualize the end-of-season clustering over time ###
########################################################

plot_df_playerVsTime1A = 
  df_off_clust_F %>%
  group_by(PLAYER_ID) %>%
  mutate(num_szns = n()) %>%
  group_by(PLAYER_ID, endOfSznCluster) %>%
  mutate(num_szns_inCluster = n()) %>%
  ungroup() %>%
  distinct(endOfSznClusterGroup, endOfSznCluster, PLAYER_ID, PLAYER_NAME, num_szns, num_szns_inCluster) %>%
  group_by(PLAYER_ID) %>%
  mutate(clust_variability = max(num_szns_inCluster/num_szns)) %>%
  ungroup() %>%
  arrange(endOfSznClusterGroup, -num_szns, PLAYER_ID) 
plot_df_playerVsTime1A
plot_df_playerVsTime1B = 
  plot_df_playerVsTime1A %>%
  group_by(PLAYER_ID) %>%
  filter(num_szns_inCluster == max(num_szns_inCluster)) %>%
  slice_head(n=1) %>%
  ungroup() 
plot_df_playerVsTime1B
player_df_leastVariable = 
  plot_df_playerVsTime1B %>%
  # arrange(endOfSznClusterGroup, -num_szns, -clust_variability) %>%
  arrange(endOfSznCluster, -num_szns, -clust_variability) %>%
  filter(num_szns >= 8) %>%
  arrange(endOfSznCluster, -clust_variability) %>%
  # filter(clust_variability >= 1) %>%
  group_by(endOfSznCluster) %>%
  slice_head(n=15) 
player_df_leastVariable
player_df_mostVariable = 
  plot_df_playerVsTime1B %>%
  filter(clust_variability < 0.65) %>%
  arrange(endOfSznCluster, -num_szns, clust_variability) %>%
  # arrange(endOfSznClusterGroup, -num_szns, clust_variability) %>%
  group_by(endOfSznCluster) %>%
  slice_head(n=15) 
player_df_mostVariable
player_names_custom = c(
  "LeBron James", "Kevin Durant",
  "Luka Doncic", "James Harden",
  "Stephen Curry", "Kyle Lowry",
  "Chris Paul", "Mike Conley",
  "Rajon Rondo", "Jalen Brunson",
  "Rudy Gobert", "Clint Capela",
  "Al Horford", "Brook Lopez",
  "Joel Embiid", "Nikola Jokic",
  "Paul Millsap", "Andre Drummond",
  "Kyle Korver", "Harrison Barnes",
  "Danny Green", "Trevor Ariza",
  "Kyle Kuzma", "Jae Crowder",
  "OG Anunoby", "Mikal Bridges"
)

###
ex_player_names_1 = player_names_custom
ex_player_names_2 = player_df_mostVariable$PLAYER_NAME
ex_player_names_3 = player_df_leastVariable$PLAYER_NAME

# PLOTIDX = 1
# for (PLOTIDX in 1:3) {
# for (PLOTIDX in 2) {
for (PLOTIDX in 2:3) {
    plot_off_endOfSznClusterExs_vs_time = 
    df_off_clust_F %>%
    mutate(
      marked = str_detect(PLAYER_NAME,paste0(get(paste0("ex_player_names_",PLOTIDX)), collapse = "|")),
    ) %>%
    filter(marked) %>%
    ungroup() %>%
    mutate(
      PLAYER_NAME = factor(PLAYER_NAME, levels=get(paste0("ex_player_names_",PLOTIDX))),
      season = year_to_season(szn)
    ) %>%
    arrange(PLAYER_ID, szn) %>%
    ggplot(aes(
       x = season, y = endOfSznCluster, 
       color=endOfSznCluster,
       # color=endOfSznClusterSubgroup, 
       shape=endOfSznClusterGroup)
    ) +
    facet_wrap(~PLAYER_NAME, ncol=5)+
    guides(shape=guide_legend(title="end-of-season group")) +
    scale_color_manual(
      # name = "end-of-season role",
      # name = "end-of-season subgroup",
      values = c(
        "firebrick", "dodgerblue2", "forestgreen"
      )
      # values = c(
      #   rev(brewer.pal(n = 9, name = "Blues")[5:7]),
      #   "gray80",
      #   brewer.pal(n = 9, name = "Reds")[5:7]
      # )
      # values = c(
      #   rev(brewer.pal(n = 9, name = "Blues")[4:7]),
      #   "gray80",
      #   brewer.pal(n = 9, name = "Reds")[4:7]
      # )
      # values = c(
      #   rev(brewer.pal(n = 9, name = "Purples")[4:7]),
      #   rev(brewer.pal(n = 9, name = "Oranges")[5:7]),
      #   rev(brewer.pal(n = 9, name = "Greens")[5:7])
      # )
    ) +  
    geom_point(size=5) +
    ylab("end-of-season role") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(axis.title=element_text(size=25), axis.text=element_text(size=15))
  # plot_off_endOfSznClusterExs_vs_time
  ggsave(paste0("plots/plot_off_endOfSznClusterExs",PLOTIDX,"_vs_time.png"), 
         plot_off_endOfSznClusterExs_vs_time, width=23, height=23)
}

