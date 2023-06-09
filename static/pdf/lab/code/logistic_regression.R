
### logistic regression
### Author: Ryan Brill

### packages
library(tidyverse)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

######################################################
### Predict Golf First-Putt_Sink Probability Data ###
######################################################

### putting data, from
# https://statmodeling.stat.columbia.edu/2019/03/21/new-golf-putting-data-and-a-new-golf-putting-model/
putt_df = tibble(
  dist_to_hole = 2:20, # in feet
  num_putts = c(1443,694,455,353,272,256,240,217,200,237,202,192,174,167,201,195,191,147,152),
  num_successes = c(1346,577,337,208,149,136,111,69,67,75,52,46,54,28,27,31,33,20,24)
)
putt_df

### visualize
plot_putts = putt_df %>%
  mutate(p = num_successes/num_putts) %>%
  ggplot(aes(x=dist_to_hole, y= p)) +
  geom_point(size=2) +
  ylab("proportion of successful first putt") +
  xlab("distance to hole")
plot_putts
# ggsave("plots/plot_putts.png", width=7, height=6)

### get the dataframe into a form acceptable for logistic regression
putt_df_1 = tibble()
for (i in 1:nrow(putt_df)) {
  putt_df_1i = tibble(
    dist_to_hole = putt_df$dist_to_hole[i],
    y = c(rep(1, putt_df$num_successes[i]), rep(0, putt_df$num_putts[i] - putt_df$num_successes[i]))
  )
  putt_df_1 = bind_rows(putt_df_1, putt_df_1i)
}
putt_df_1

### linear regression
m1 = lm(y ~ dist_to_hole, data=putt_df_1)
m1
### visualize
plot_putts1 = putt_df %>%
  mutate(p = num_successes/num_putts) %>%
  mutate(pred = predict(m1,.)) %>%
  ggplot(aes(x=dist_to_hole)) +
  geom_point(aes(y=p), linewidth=2) +
  geom_line(aes(y=pred), linewidth=2) +
  ylab("proportion of successful first putt") +
  xlab("distance to hole")
plot_putts1
# ggsave("plots/plot_putts1.png", width=7, height=6)

### linear regression with cubic term
m2 = lm(y ~ dist_to_hole + I(dist_to_hole^2) + I(dist_to_hole^3), data=putt_df_1)
m2
### visualize
plot_putts2 = putt_df %>%
  mutate(p = num_successes/num_putts) %>%
  mutate(pred = predict(m2,.)) %>%
  ggplot(aes(x=dist_to_hole)) +
  geom_point(aes(y=p), size=2) +
  geom_line(aes(y=pred), size=2) +
  ylab("proportion of successful first putt") +
  xlab("distance to hole")
plot_putts2
# ggsave("plots/plot_putts2.png", width=7, height=6)

### linear regression with cubic term, but extrapolated
m2 = lm(y ~ dist_to_hole + I(dist_to_hole^2) + I(dist_to_hole^3), data=putt_df_1)
m2
### visualize
plot_putts2b = putt_df %>%
  bind_rows(tibble(dist_to_hole=21:50,num_putts=0,num_successes=0)) %>%
  mutate(p = num_successes/num_putts) %>%
  mutate(pred = predict(m2,.)) %>%
  ggplot(aes(x=dist_to_hole)) +
  geom_point(aes(y=p), size=2) +
  geom_line(aes(y=pred)) +
  ylab("proportion of successful first putt") +
  xlab("distance to hole")
plot_putts2b
# ggsave("plots/plot_putts2b.png", width=7, height=6)

### logit function
logit <- function(p) { log(p/(1-p)) }
plot_logit = tibble(p=seq(0.001,0.999,by=0.001)) %>%
  mutate(logit_p = logit(p)) %>%
  ggplot(aes(x=p,y=logit_p)) +
  geom_line() +
  ylab("logit(p)")
plot_logit
# ggsave("plots/plot_logit.png", width=7, height=6)

### logistic regression
m3 = glm(y ~ dist_to_hole, data=putt_df_1, family="binomial")
m3
### visualize
plot_putts3 = putt_df %>%
  mutate(p = num_successes/num_putts) %>%
  mutate(pred = predict(m3,.,type="response")) %>%
  ggplot(aes(x=dist_to_hole)) +
  geom_point(aes(y=p), size=2) +
  geom_line(aes(y=pred), size=2) +
  ylab("proportion of successful first putt") +
  xlab("distance to hole")
plot_putts3
# ggsave("plots/plot_putts3.png", width=7, height=6)
### visualize, extrapolated
plot_putts3a = putt_df %>%
  bind_rows(tibble(dist_to_hole=21:50,num_putts=0,num_successes=0)) %>%
  mutate(p = num_successes/num_putts) %>%
  mutate(pred = predict(m3,.,type="response")) %>%
  ggplot(aes(x=dist_to_hole)) +
  geom_point(aes(y=p), size=2) +
  geom_line(aes(y=pred)) +
  ylab("proportion of successful first putt") +
  xlab("distance to hole")
plot_putts3a
# ggsave("plots/plot_putts3a.png", width=7, height=6)

### logistic regression with 
m4 = glm(y ~ poly(dist_to_hole,3), data=putt_df_1, family="binomial")
# m4 = glm(y ~ poly(dist_to_hole,5), data=putt_df_1, family="binomial")
# m4 = glm(y ~ splines::bs(dist_to_hole,knots=c(10)), data=putt_df_1, family="binomial")
m4
### visualize
plot_putts4 = putt_df %>%
  mutate(p = num_successes/num_putts) %>%
  mutate(pred = predict(m4,.,type="response")) %>%
  ggplot(aes(x=dist_to_hole)) +
  geom_point(aes(y=p), size=2) +
  geom_line(aes(y=pred), size=2) +
  ylab("proportion of successful first putt") +
  xlab("distance to hole")
plot_putts4
# ggsave("plots/plot_putts4.png", width=7, height=6)
### visualize, extrapolated
plot_putts4a = putt_df %>%
  bind_rows(tibble(dist_to_hole=21:50,num_putts=0,num_successes=0)) %>%
  mutate(p = num_successes/num_putts) %>%
  mutate(pred = predict(m4,.,type="response")) %>%
  ggplot(aes(x=dist_to_hole)) +
  geom_point(aes(y=p), size=2) +
  geom_line(aes(y=pred)) +
  ylab("proportion of successful first putt") +
  xlab("distance to hole")
plot_putts4a
# ggsave("plots/plot_putts4a.png", width=7, height=6)



########################################################
### NCAA Mens Basketball Bradley Terry Power Ratings ###
########################################################

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
  mutate(ScoreDiff = ifelse(WLoc=="H", WScore-LScore, LScore-WScore)) %>%
  mutate(Win=as.numeric(ScoreDiff>0))
df_ncaamb1 = df_ncaamb %>% filter(Season == 2023)
df_ncaamb1
# df_ncaamb2 = df_ncaamb1 %>% select(Season, WTeamName, LTeamName,WScore, LScore, WLoc, WTeamID, LTeamID, ScoreDiff,  )
df_ncaamb2 = df_ncaamb1 %>% select(Season, WTeamName, LTeamName,WScore, LScore, WLoc, ScoreDiff,  Win)
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

### get power ratings using Bradley Terry (logistic regression)
bradley_terry = glm(df_ncaamb2$Win ~ X + 0, family="binomial")
power_ratings = bradley_terry$coefficients

### visualize the power ratings
plot_ncaamb_bradley_terry = 
  tibble(teams=colnames(X), power_ratings=unname(power_ratings)) %>%
  ggplot(aes(y=fct_reorder(teams, power_ratings), x=power_ratings)) +
  # ggplot(aes(y=teams, x=power_ratings)) +
  geom_point() +
  theme(axis.text.y = element_text(size=8)) +
  xlab("power rating") + ylab("team") 
plot_ncaamb_bradley_terry
# ggsave("plots/plot_ncaamb_bradley_terry.png", width=20, height=20)

### visualize the power ratings
set.seed(25)
plot_ncaamb_bradley_terry_1 = 
  tibble(teams=colnames(X), power_ratings=unname(power_ratings)) %>%
  slice_sample(n=25) %>%
  ggplot(aes(y=fct_reorder(teams, power_ratings), x=power_ratings)) +
  # ggplot(aes(y=teams, x=power_ratings)) +
  geom_point() +
  theme(axis.text.y = element_text(size=12)) +
  xlab("power rating") + ylab("team") 
plot_ncaamb_bradley_terry_1
# ggsave("plots/plot_ncaamb_bradley_terry_1.png", width=8, height=7)

###
tibble(teams=colnames(X), power_ratings=unname(power_ratings)) %>%
  drop_na() %>%
  arrange(power_ratings) %>%
  head(5)
tibble(teams=colnames(X), power_ratings=unname(power_ratings)) %>%
  drop_na() %>%
  arrange(-power_ratings) %>%
  head(5)


