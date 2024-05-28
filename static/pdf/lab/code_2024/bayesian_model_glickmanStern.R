
### logistic regression
### Author: Ryan Brill

### packages
library(tidyverse)
library(nflfastR)
library(nflplotR)
library(rstan)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

#################################
### Scrape data from NFLFastR ###
#################################

# SCRAPE_DATA = TRUE
SCRAPE_DATA = FALSE
filename = "data/data_bayesian_model_glickmanStern.csv"

if (!SCRAPE_DATA & file.exists(filename)) {
  df0 = read_csv(filename, show_col_types = F)
} else {
  ### scrape the data from nflfastR
  d1 = load_pbp(2018:2023)
  d2 = d1 %>%
    distinct(game_id, season, season_type, week, home_team, away_team, total_home_score, total_away_score) %>%
    group_by(game_id) %>%
    slice_tail() %>%
    ungroup() %>%
    mutate(pts_H_minus_A = total_home_score - total_away_score)
  d2
  df0 = d2
  write_csv(df0, filename)
}

df0

########################################################
### Create variables that comply with the Stan model ###
########################################################

### create variables S, H, A, y
map_team_to_idx = 
  tibble(
    team = sort(unique(c(df0$home_team, df0$away_team)))
  ) %>%
  mutate(idx = 1:n())
data.frame(map_team_to_idx)
df1 = 
  df0 %>%
  mutate(
    S = season - min(season) + 1
  ) %>%
  left_join(
    map_team_to_idx %>% rename(home_team = team, H = idx)
  ) %>%
  left_join(
    map_team_to_idx %>% rename(away_team = team, A = idx)
  ) %>%
  mutate(
    y = pts_H_minus_A
  )
df1

### checks
data.frame(df1 %>% distinct(season, S) %>% arrange(S))
data.frame(df1 %>% distinct(home_team, H) %>% arrange(H))
data.frame(df1 %>% distinct(away_team, A) %>% arrange(A))

##########################
### Fit the Stan model ###
##########################

### load stan model
MODEL <- stan_model(file = "bayesian_model_glickmanStern.stan", model_name = "glickmanSternModel")
MODEL

### create list of data compliant with the Stan model
data_train <- list(
  N_games = nrow(df1),
  N_teams = nrow(map_team_to_idx),
  N_seasons = length(unique(df1$season)),
  y = df1$y, 
  H = df1$H, 
  A = df1$A, 
  S = df1$S
)
data_train

# Train the model
fit <- sampling(
  MODEL, data = data_train, iter = 1500, chains = 1, seed = 12345,
)
fit

### diagnostic check for convergence: max Rhat value is smaller than 1.1
max(summary(fit)$summary[,"Rhat"])

##################################
### Visualize the coefficients ###
##################################

df_params_0 = summary(fit)$c_summary[,,1]
df_params_0
df_params_1 = 
  tibble(
    param = rownames(df_params_0),
    # post_lower = df_params_0[,"2.5%"],
    post_lower = df_params_0[,"25%"],
    post_med = df_params_0[,"50%"],
    post_upper = df_params_0[,"75%"],
    # post_upper = df_params_0[,"97.5%"],
  )
df_params_1

df_params_2 = 
  df_params_1 %>%
  filter(!str_detect(param, "betas") & !str_detect(param, "lp__"))
df_params_2

map_team_to_div = tibble(
  team = map_team_to_idx$team,
  div = c(
    "NFC West", "NFC South", "AFC North", "AFC East",
    "NFC South", "NFC North", "AFC North", "AFC North",
    "NFC East", "AFC West", "NFC North", "NFC North",
    "AFC South", "AFC South", "AFC South", "AFC West",
    "NFC West", "AFC West", "AFC West", "AFC East",
    "NFC North", "AFC East", "NFC South", "NFC East",
    "AFC East", "NFC East", "AFC North", "NFC West",
    "NFC West", "NFC South", "AFC South", "NFC East"
  )
)
data.frame(map_team_to_div %>% arrange(div, team))

df_betas_0 = 
  df_params_1 %>%
  filter(str_detect(param, "betas")) %>%
  mutate(
    idxs = str_remove(param, "betas"),
    idxs = str_remove(idxs, "\\["),
    idxs = str_remove(idxs, "\\]"),
  ) %>%
  rowwise() %>%
  mutate(
    team_idx = as.numeric(str_split(idxs, ",")[[1]][1]),
    S = as.numeric(str_split(idxs, ",")[[1]][2])
  ) %>%
  ungroup() %>%
  left_join(
    map_team_to_idx %>% rename(team_idx = idx),
  ) %>%
  mutate(season = min(df0$season) + S - 1) %>%
  left_join(map_team_to_div)
df_betas_0

# df_betas_1 = 
#   df_betas_0 # %>% pivot_longer(c("post_lower", "post_med", "post_upper"))
# df_betas_1

### plot
plot_betas = 
  df_betas_0 %>%
  ggplot(aes(x = season, color=team, fill=team)) +
  facet_wrap(~ div, nrow=2) +
  geom_line(aes(y = post_med), linewidth=1) +
  geom_line(aes(y = post_lower), linewidth=0.5, linetype="dotted") +
  geom_line(aes(y = post_upper), linewidth=0.5, linetype="dotted") +
  geom_ribbon(aes(ymin=post_lower, ymax=post_upper), alpha=0.2) +
  geom_nfl_logos(aes(team_abbr = team, y = post_med, width = 0.05), alpha=0.5) + ###
  scale_color_nfl(type = "primary") +
  scale_fill_nfl(type = "primary") +
  theme(panel.spacing = unit(2, "lines")) +
  labs(subtitle="posterior median team ratings, with ribbons for 50% credible intervals") +
  ylab("posterior team rating\n from the Glickman Stern model")
# plot_betas
ggsave("plots/plot_betas_glickmanStern.png", plot_betas,
       width=20, height=8)


