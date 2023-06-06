
### multivariable linear regression
### Author: Ryan Brill

### packages
library(tidyverse)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

##########################################
### NCAA Mens Basketball Power Ratings ###
##########################################

### get data
# https://www.kaggle.com/competitions/march-machine-learning-mania-2023/data?select=MTeams.csv
df_ncaamb = read_csv("data/MRegularSeasonCompactResults.csv")
df_ncaamb_teams = read_csv("data/MTeams.csv") %>% select(TeamID, TeamName)
df_ncaamb
df_ncaamb_teams
df_ncaamb = df_ncaamb %>%
  left_join(df_ncaamb_teams %>% rename(WTeamID = TeamID, WTeamName = TeamName)) %>%
  relocate(WTeamName, .after=WTeamID) %>%
  left_join(df_ncaamb_teams %>% rename(LTeamID = TeamID, LTeamName = TeamName)) %>%
  relocate(LTeamName, .after=LTeamID) %>%
  mutate(ScoreDiff = ifelse(WLoc=="H", WScore-LScore, LScore-WScore))
df_ncaamb1 = df_ncaamb %>% filter(Season == 2023)
df_ncaamb1
# df_ncaamb2 = df_ncaamb1 %>% select(Season, WTeamName, LTeamName,WScore, LScore, WLoc, WTeamID, LTeamID, ScoreDiff,  )
df_ncaamb2 = df_ncaamb1 %>% select(Season, WTeamName, LTeamName,WScore, LScore, WLoc, ScoreDiff,  )
df_ncaamb2[25:30,]

### create scheduling matrix X 
teams = sort(unique(c(df_ncaamb2$WTeamName, df_ncaamb2$LTeamName)))
X = matrix(0, nrow = nrow(df_ncaamb2), ncol = length(teams) + 1)
colnames(X) = c("(Intercept)", teams)
X[,1] = 1 ### intercept column
for (i in 1:nrow(df_ncaamb2)) {
  game_i = df_ncaamb2[i,]
  HTeam_i = ifelse(game_i$WLoc == "H", game_i$WTeamName, game_i$LTeamName)
  ATeam_i = ifelse(game_i$WLoc != "H", game_i$WTeamName, game_i$LTeamName)
  HTeamID_i = which(teams == HTeam_i)
  ATeamID_i = which(teams == ATeam_i)
  X[i,HTeamID_i+1] = 1
  X[i,ATeamID_i+1] = -1
}
df_ncaamb2[1:5,]
X[1:5,c(1:5,131)]

### get power ratings using multivariable linear regression
power_ratings_model = lm(df_ncaamb2$ScoreDiff ~ X + 0)
power_ratings = power_ratings_model$coefficients
power_ratings

### visualize the power ratings
plot_ncaamb_power_ratings = 
  tibble(teams=colnames(X), power_ratings=unname(power_ratings)) %>%
  ggplot(aes(y=fct_reorder(teams, power_ratings), x=power_ratings)) +
  # ggplot(aes(y=teams, x=power_ratings)) +
  geom_point() +
  theme(axis.text.y = element_text(size=8)) +
  xlab("power rating") + ylab("team") 
plot_ncaamb_power_ratings
# ggsave("plots/plot_ncaamb_power_ratings.png", width=20, height=20)

### visualize the power ratings
set.seed(25)
plot_ncaamb_power_ratings_1 = 
  tibble(teams=colnames(X), power_ratings=unname(power_ratings)) %>%
  slice_sample(n=25) %>%
  ggplot(aes(y=fct_reorder(teams, power_ratings), x=power_ratings)) +
  # ggplot(aes(y=teams, x=power_ratings)) +
  geom_point() +
  theme(axis.text.y = element_text(size=12)) +
  xlab("power rating") + ylab("team") 
plot_ncaamb_power_ratings_1
# ggsave("plots/plot_ncaamb_power_ratings_1.png", width=8, height=7)

###
tibble(teams=colnames(X), power_ratings=unname(power_ratings)) %>%
  drop_na() %>%
  arrange(power_ratings) %>%
  head(5)
tibble(teams=colnames(X), power_ratings=unname(power_ratings)) %>%
  drop_na() %>%
  arrange(-power_ratings) %>%
  head(5)

### sigma^2 estimate
sigma(power_ratings_model)


#######################################
### Expected points of a first down ###
#######################################

### get data
filename_ep = "data/data_ep_2015_2019.csv"
if (file.exists(filename_ep)) {
  D3a = read_csv(filename_ep)
} else { ### get data from Ben Baldwin's R archive
  # some helper files are in these
  source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R")
  source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R")
  source("https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_cp_cpoe.R")
  
  # from remote
  pbp_data <- readRDS(url("https://github.com/guga31bb/nflfastR-data/blob/master/models/cal_data.rds?raw=true"))
  
  # from local
  # pbp_data <- readRDS('../../nflfastR-data/models/cal_data.rds')
  
  D3 <- pbp_data %>%
    # in 'R/helper_add_nflscrapr_mutations.R'
    make_model_mutations() %>%
    mutate(
      label = case_when(
        Next_Score_Half == "Touchdown" ~ 0,
        Next_Score_Half == "Opp_Touchdown" ~ 1,
        Next_Score_Half == "Field_Goal" ~ 2,
        Next_Score_Half == "Opp_Field_Goal" ~ 3,
        Next_Score_Half == "Safety" ~ 4,
        Next_Score_Half == "Opp_Safety" ~ 5,
        Next_Score_Half == "No_Score" ~ 6
      ),
      label = as.factor(label),
      # use nflscrapR weights
      Drive_Score_Dist = Drive_Score_Half - drive,
      Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
        (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
      ScoreDiff_W = (max(abs(score_differential), na.rm = T) - abs(score_differential)) /
        (max(abs(score_differential), na.rm = T) - min(abs(score_differential), na.rm = T)),
      Total_W = Drive_Score_Dist_W + ScoreDiff_W,
      Total_W_Scaled = (Total_W - min(Total_W, na.rm = T)) /
        (max(Total_W, na.rm = T) - min(Total_W, na.rm = T))
    ) %>%
    filter(
      !is.na(defteam_timeouts_remaining), !is.na(posteam_timeouts_remaining),
      !is.na(yardline_100)
    ) %>%
    select(
      label,
      season,
      half_seconds_remaining,
      yardline_100,
      home,
      retractable,
      dome,
      outdoors,
      ydstogo,
      era0, era1, era2, era3, era4,
      down1, down2, down3, down4,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining,
      Total_W_Scaled
    ) %>%
    mutate(pts_next_score = case_when(
      label == 0 ~ 7,
      label == 1 ~ -7,
      label == 2 ~ 3,
      label == 3 ~ -3,
      label == 4 ~ 2,
      label == 5 ~ -2,
      label == 6 ~ 0,
    ))
  
  # idk why this is all necessary for xgb but it is
  D3 <- D3 %>%
    mutate(
      label = as.numeric(label),
      label = label - 1
    )
  
  rm(pbp_data)
  
  D3a = D3 %>% filter(season%in%2015:2019) %>% filter(down1==1) 
  write_csv(D3a, filename_ep)
}
D3a
D3b = D3a %>% filter(season==2019)
D3b

### visualize points of the next score vs. yardline
plot_mean_pts_redzone = D3a %>%
  filter(yardline_100 <= 20) %>%
  select(yardline_100, pts_next_score) %>%
  group_by(yardline_100) %>%
  summarise(ep_mean = mean(pts_next_score)) %>%
  ggplot(aes(x=yardline_100, y=ep_mean)) +
  ylab("mean points of the next score") + xlab("yardline") +
  geom_point()
plot_mean_pts_redzone
# ggsave("plots/plot_mean_pts_redzone.png", width=7, height=6)

### linear model
D3r = D3a %>% filter(yardline_100 <= 20) %>% select(yardline_100, pts_next_score)
m_ep_linear = lm(data=D3r, pts_next_score ~ yardline_100)
m_ep_linear

### quadratic model
m_ep_quad = lm(data=D3r, pts_next_score ~ yardline_100 + I(yardline_100^2))
m_ep_quad

### visualize linear vs quadratic model
plot_mean_pts_redzone_1 = D3r %>%
  mutate(pred_linear = predict(m_ep_linear, .),
         pred_quad = predict(m_ep_quad, .)) %>%
  group_by(yardline_100) %>%
  summarise(ep_mean = mean(pts_next_score),
            pred_linear = pred_linear[1],
            pred_quad = pred_quad[1]) %>%
  ggplot(aes(x=yardline_100, y=ep_mean)) +
  ylab("mean points of the next score") + xlab("yardline") +
  geom_point(size=2) +
  geom_line(aes(y=pred_linear), color="firebrick", linewidth=1) +
  geom_line(aes(y=pred_quad), color="dodgerblue2", linewidth=1)
plot_mean_pts_redzone_1
# ggsave("plots/plot_mean_pts_redzone_1.png", width=7, height=6)




