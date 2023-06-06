
### simple linear regression
### Author: Ryan Brill

### packages
library(tidyverse)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

####################################################
### Predict year t batting average from year t-1 ###
####################################################

### load Lahman batting data
# install.packages("Lahman")
library(Lahman)
data(Batting)

### examine the data
unique(Batting$yearID)
unique(Batting$playerID)

### create nicer dataframe specific to our problem
D1 = Batting %>%
  ### keep years 2020, 2021
  filter(yearID %in% 2020:2021) %>%
  arrange(playerID, yearID) %>%
  ### more than 25 at-bats
  filter(AB >= 25) %>%
  ### create batting average variable
  mutate(BA = H/AB) %>%
  select(playerID, yearID, H, AB, BA) %>%
  ### only keep players who played in both 2020 and 2021
  group_by(playerID) %>%
  filter(n() == 2 & yearID[1] == 2020 & yearID[2] == 2021) %>%
  ungroup() 
D1

D1a = D1 %>%
  select(playerID, yearID, BA) %>%
  pivot_wider(names_from = yearID, values_from = BA, names_glue = "{.value}_{yearID}") %>%
  ### remove outliers
  filter(BA_2020 >= 0.15 & BA_2021 >= 0.15)
D1a

### visualize the relationship of BA from 2020 to 2021  
plot_BA_2020_2021_1 = D1a %>%
  ### `aes` means use column names from the dataframe
  ggplot(aes(x = BA_2020, y = BA_2021)) +
  geom_point()
plot_BA_2020_2021_1
# ggsave("plots/plot_BA_2020_2021_1.png", width=7, height=6)

### it looks like a linear relationship!
plot_BA_2020_2021_2 = D1a %>%
  ggplot(aes(x = BA_2020, y = BA_2021)) +
  geom_point() +
  geom_smooth(formula="y~x", method="lm", se=FALSE)
plot_BA_2020_2021_2
# ggsave("plots/plot_BA_2020_2021_2.png", width=7, height=6)

### visualize the residual sum of squares
m1 = lm(data=D1a, BA_2021~BA_2020)
plot_BA_2020_2021_3 = D1a %>%
  mutate(pred = predict(m1, .)) %>%
  ggplot(aes(x = BA_2020, y = BA_2021)) +
  geom_segment(aes(x=BA_2020, xend=BA_2020, y=pred, yend=BA_2021), color="magenta", linewidth=0.25) +
  geom_point(size=2) +
  geom_smooth(formula="y~x", method="lm", se=FALSE, linewidth=2) 
plot_BA_2020_2021_3
# ggsave("plots/plot_BA_2020_2021_3.png", width=7, height=6)

### examine regression coefficients
m1 = lm(data=D1a, BA_2021~BA_2020)
summary(m1)
m1$coefficients

##################################
### Pythagorean win percentage ###
##################################

data(Teams)

### make 2021 pythagorean dataset
D2 = Teams %>% 
  filter(yearID %in% 2020:2021) %>% 
  select(yearID,teamID,R,RA,W,G) %>%
  rename(RS = R) %>%
  mutate(WP = W/G) %>%
  mutate(WP_Pythag_2 = RS^2/(RS^2+RA^2))
D2

### visualize 2020 Bill James' pythag WP
plot_pythag_2020_2a = D2 %>%
  filter(yearID==2020) %>%
  ggplot(aes(x = WP_Pythag_2, y = WP, label=teamID)) + 
  geom_point() +
  geom_text(hjust=1.2) +
  geom_abline(intercept = 0, slope=1, color="gray60", linetype="dashed") +
  labs(title="2020 win percentage vs. \n pythagorean win percentage")
plot_pythag_2020_2a
# ggsave("plots/plot_pythag_2020_2a.png", width=7, height=6)

### pythag linear regression in 2020
data_train = D2 %>% filter(yearID == 2020)
# data_hold_out = D2 %>% filter(yearID==2021)
m2 = lm(data=data_train, log((1-WP)/WP) ~ log(RA/RS) + 0 )
m2
alpha_ = 1.867
D2 = D2 %>%
  mutate(WP_Pythag_1867 = RS^alpha_/(RS^alpha_+RA^alpha_))

### visualize both pythags
plot_pythag_2020_3a = D2 %>%
  filter(yearID==2020) %>%
  pivot_longer(c(WP_Pythag_2, WP_Pythag_1867)) %>%
  ggplot(aes(x = value, y = WP, color=name, label=teamID)) + 
  geom_point(size=3) +
  # geom_point(aes(x = WP_Pythag_2), color="firebrick", size=3) +
  # geom_point(aes(x = WP_Pythag_1867), color="dodgerblue2", size=3) +
  # geom_text(hjust=1.2) +
  geom_abline(intercept = 0, slope=1, color="gray60", linetype="dashed") +
  labs(title="2020 win percentage vs. \n pythagorean win percentage") +
  scale_color_manual(values=c("dodgerblue2", "firebrick"))
plot_pythag_2020_3a
# ggsave("plots/plot_pythag_2020_3a.png", width=9, height=6)

