
### teaching regression and parametric inference via the 
### time through the order penalty in baseball
### Author: Ryan Brill

### packages
library(tidyverse)
library(splines)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

### TTO data
df0 = read_csv("data/data_tto_ex.csv", show_col_types = F)
df0

####################
### TTO Analysis ###
####################

### bin and average by ORDER_CT
df0 %>% group_by(ORDER_CT) %>% summarise(mean_woba = mean(EVENT_WOBA_19))

### binning and averaging is equivalent to a fixed effects regression
m1 = lm(EVENT_WOBA_19 ~ 0 + factor(ORDER_CT), data=df0)
m1

##### so, we empirically observe that starting pitchers get worse relative to batters as the game progresses

### bin and average across batter sequence number
plot_tto_bin_and_avg = 
  df0 %>% 
  group_by(BATTER_SEQ_NUM) %>% mutate(mean_woba_bsn = mean(EVENT_WOBA_19)) %>%
  group_by(ORDER_CT) %>% mutate(mean_woba_tto = mean(EVENT_WOBA_19)) %>%
  distinct(BATTER_SEQ_NUM, ORDER_CT, mean_woba_bsn, mean_woba_tto) %>%
  ggplot(aes(x=BATTER_SEQ_NUM)) + 
  geom_point(aes(y=mean_woba_bsn)) +
  scale_x_continuous(breaks=seq(1,27,by=9)) +
  xlab("batter sequence number") + ylab("mean wOBA") +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==1, 1, NA))) +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==2, 1, NA))) +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==3, 1, NA))) +
  labs("binning and averaging")
plot_tto_bin_and_avg
ggsave("plots/plot_tto_bin_and_avg.png", plot_tto_bin_and_avg, width=8, height=4)

### we need to adjust for confounders !!!

### fixed effect model on ORDER_CT after adjusting for confounders
# m2 = lm(EVENT_WOBA_19 ~ 0 + factor(ORDER_CT) + HAND_MATCH + BAT_HOME_IND + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19, data=df0)

### fixed effect model on ORDER_CT after adjusting for confounders
m2 = lm(EVENT_WOBA_19 ~ 1 + as.numeric(ORDER_CT>=2) + as.numeric(ORDER_CT>=3) +
                        HAND_MATCH + BAT_HOME_IND + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19, 
        data=df0)
m2
dfm2 = 
  tibble(
    BATTER_SEQ_NUM = 1:27,
    ORDER_CT = c(rep(1,9), rep(2,9), rep(3,9)),
    HAND_MATCH = 1,
    BAT_HOME_IND = 1,
    WOBA_FINAL_BAT_19 = mean(df0$WOBA_FINAL_BAT_19),
    WOBA_FINAL_PIT_19 = mean(df0$WOBA_FINAL_PIT_19),
  ) %>%
  mutate(pred_woba = predict(m2, .))
dfm2
dfm2 %>% distinct(ORDER_CT, pred_woba)
plot_tto_m2 = 
  dfm2 %>% 
  ggplot(aes(x=BATTER_SEQ_NUM)) + 
  geom_point(aes(y=pred_woba)) +
  scale_x_continuous(breaks=seq(1,27,by=9)) +
  xlab("batter sequence number") + ylab("predicted wOBA") +
  labs(subtitle="handedness match, batter at home,\naverage pitcher and batter") +
  # geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==1, 1, NA))) +
  # geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==2, 1, NA))) +
  # geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==3, 1, NA))) +
  labs("binning and averaging")
plot_tto_m2
ggsave("plots/plot_tto_m2.png", plot_tto_m2, width=8, height=5)

### indicator model on BATTER_SEQ_NUM after adjusting for confounders
### with a smoothing spline
m3 = lm(EVENT_WOBA_19 ~ 0 + factor(BATTER_SEQ_NUM) + HAND_MATCH + BAT_HOME_IND + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19, data=df0)
m3
m4 = lm(EVENT_WOBA_19 ~ 1 + BATTER_SEQ_NUM + HAND_MATCH + BAT_HOME_IND + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19, data=df0)
m4
dfm3 = dfm2 %>% 
  mutate(
    pred_woba_ind = predict(m3, .),
    smoothed_woba = smooth.spline(BATTER_SEQ_NUM, pred_woba_ind)$y,
    pred_woba_line = predict(m4, .)
  ) %>%
  group_by(ORDER_CT) %>%
  mutate(mean_woba_tto = mean(pred_woba_ind)) %>%
  ungroup()
dfm3
plot_tto_m3 = 
  dfm3 %>% 
  ggplot(aes(x=BATTER_SEQ_NUM)) + 
  geom_point(aes(y=pred_woba_ind)) +
  geom_line(aes(y=smoothed_woba)) +
  geom_line(aes(y=pred_woba_line), color="blue") +
  scale_x_continuous(breaks=seq(1,27,by=9)) +
  xlab("batter sequence number") + ylab("predicted wOBA") +
  labs(subtitle="handedness match, batter at home,\naverage pitcher and batter") +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==1, 1, NA))) +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==2, 1, NA))) +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==3, 1, NA))) +
  labs("binning and averaging")
plot_tto_m3
ggsave("plots/plot_tto_m3.png", plot_tto_m3, width=8, height=5)

### are the pitchers declining in a continuous or discontinuous fashion?
### include terms for Both ORDER_CT and BATTER_SEQ_NUM
# m5 = lm(EVENT_WOBA_19 ~ factor(ORDER_CT) + BATTER_SEQ_NUM + HAND_MATCH + BAT_HOME_IND + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19, data=df0)
m5 = lm(EVENT_WOBA_19 ~ 1 + as.numeric(ORDER_CT>=2) + as.numeric(ORDER_CT>=3) + BATTER_SEQ_NUM + HAND_MATCH + BAT_HOME_IND + WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19, data=df0)
m5
dfm5 = dfm3 %>% 
  mutate(
    pred_woba = predict(m5, .),
  ) %>%
  group_by(ORDER_CT) %>%
  mutate(mean_woba_tto = mean(pred_woba)) %>%
  ungroup()
dfm5

plot_tto_m5 = 
  dfm5 %>% 
  ggplot(aes(x=BATTER_SEQ_NUM)) + 
  geom_point(aes(y=pred_woba)) +
  scale_x_continuous(breaks=seq(1,27,by=9)) +
  xlab("batter sequence number") + ylab("predicted wOBA") +
  labs(subtitle="handedness match, batter at home,\naverage pitcher and batter") +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==1, 1, NA))) +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==2, 1, NA))) +
  geom_line(aes(y=mean_woba_tto*ifelse(ORDER_CT==3, 1, NA))) +
  labs("binning and averaging")
plot_tto_m5
ggsave("plots/plot_tto_m5.png", plot_tto_m5, width=8, height=5)

### is the pitcher decline significant???

### is pitcher decline significant across times through the order?
m2
summary(m2)
## yes it is!

### is DISCONTINUOUS pitcher decline across times through the order significant 
### after adjusting for CONTINUOUS pitcher decline across batters?
m5
summary(m5)
## no it is not!


