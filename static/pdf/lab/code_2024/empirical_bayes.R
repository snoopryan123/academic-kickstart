
### empirical bayes
### Author: Ryan Brill

### packages
library(tidyverse)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

### read data (get from website)
df_BA = read_csv("data/df_in_season_pred_BA_2019.csv")
df_BA1 = df_BA %>% filter(N_mid >= 25 & N_end-N_mid >= 25)
df_BA
df_BA1

### or, instead, scrape the data yourself!
{
  # library(baseballr)
  # ### first, install Chadwick
  # # then, put Chakdwick folder in cwd `grid_war_app`
  # # https://www.pitchbypitch.com/2013/11/29/installing-chadwick-software-on-mac/
  # ### then, get Retrosheet data!  1952-2007
  # # https://billpetti.github.io/2019-08-10-build-retrosheet-event-roster-database-rstats-baseballr/
  # D0 = retrosheet_data(years_to_acquire = 2019)
  # D1 = D0[[1]]$events %>% relocate(rbi_ct, .after=home_score_ct)
  # D2 = D1 %>%
  #   ### get date
  #   mutate(
  #     # game_date = str_sub(game_id, start=4,end=12)
  #     game_date1 = str_sub(game_id, start=4,end=7),
  #     game_date2 = str_sub(game_id, start=8,end=9),
  #     game_date3 = str_sub(game_id, start=10,end=11),
  #     game_date = paste0(game_date1,"-",game_date2,"-",game_date3),
  #     game_date = as.Date(game_date)
  #   ) %>%
  #   mutate(pre_all_star_2019 = game_date <= "2019-07-09") %>%
  #   select(game_id, game_date, pre_all_star_2019, year, bat_id, h_fl, bat_event_fl, ab_fl) %>%
  #   mutate(HIT_VAL = h_fl != 0) %>%
  #   filter(ab_fl) %>%
  #   arrange(game_id,game_date,bat_id)
  # D2
  # df_BA_end = D2 %>%
  #   group_by(bat_id,year) %>%
  #   summarise(BA_end = mean(HIT_VAL), N_end = n(), .groups = "drop")
  # df_BA_mid = D2 %>%
  #   filter(pre_all_star_2019) %>%
  #   group_by(bat_id,year) %>%
  #   summarise(BA_mid = mean(HIT_VAL), N_mid = n(), .groups = "drop")
  # df_BA = left_join(df_BA_mid, df_BA_end)
  # df_BA
  # # #####df_BA1 = df_BA %>% filter(N_mid >= 25 & N_end >= 50) %>% drop_na()
  # df_BA1 = df_BA %>% drop_na()
  # df_BA1
  # ##plot(df_BA1$BA_mid, df_BA1$BA_end)
  # write_csv(df_BA1, "data/df_in_season_pred_BA_2019.csv")
}

###
# df_train = df_BA %>% select(bat_id, year, BA_mid, N_mid)
# df_test = df_BA %>% select(bat_id, year, BA_end, N_end) ### don't use this until the end
# df_train

df_train = df_BA1 %>% select(bat_id, year, BA_mid, N_mid)
df_test = df_BA1 %>% select(bat_id, year, BA_end, N_end) ### don't use this until the end
df_train

##############################################################################
### Predict end-of-season batting average from mid-season batting averages ###
### using data from just just that season                                  ###
##############################################################################

###### MLE estimator
df_train = df_train %>% mutate(mu_MLE = BA_mid)
df_train

###### Empirical Bayes estimator
### get sigma.sq (assume is known)
# C.sig = 3/16
# C.sig = 0.05
C.sig = 0.035 ### tuned using data from a prev. season...
df_train = df_train %>% mutate(sig.sq = C.sig/N_mid)
df_train
### estimate mu.MLE and tau.sq.MLE
N = nrow(df_train)
# mu.MLE = numeric(N)
# tau.sq.MLE = numeric(N)
df_train = 
  df_train %>%
  ### initialize mu and tau.sq
  mutate(
    X = BA_mid,
    mu = mean(X),
    mu.prev = mu,
    tau.sq = var(X),# - mean(sig.sq),
    tau.sq.prev = tau.sq
  )
df_train

t=1
while (TRUE) {
  print(paste0("iteration t=",t))
  ### save previous iterations' mu and tau.sq
  df_train = df_train %>% mutate(mu.prev = mu, tau.sq.prev = tau.sq)
  
  ### update mu
  df_train = 
    df_train %>%
    mutate(
      mu = sum(X/(tau.sq+sig.sq)) / sum(1/(tau.sq+sig.sq))
    )
  
  ### update tau.sq
  f1 = function(t) {
    xx = df_train %>% summarise(v = sum( (X-mu)^2/(t+sig.sq)^2 ) - sum(1/(t+sig.sq)) )
    xx$v
  }
  # f1(0.0001)
  # f1(1)
  # f1(100)
  # f1(10000000)
  tau.sq_t = uniroot(f1, interval=c(0,100000))$root
  df_train = df_train %>% mutate(tau.sq = tau.sq_t)
  
  ### assess convergence
  df_conv = df_train %>% summarise(
    mu_conv = unique(abs(mu - mu.prev)),
    tau_conv = unique(abs(tau.sq - tau.sq.prev))
  ) 
  conv_num = min(df_conv$mu_conv, df_conv$tau_conv)
  print(conv_num)
  if ( conv_num < 1e-5 ) {
    break
  }
  t = t+1
}
df_train = df_train %>% select(-c(mu.prev, tau.sq.prev))
df_train
### compute posterior mean
df_train = df_train %>%
  mutate(
    mu_EB = (X/sig.sq + mu/tau.sq) / (1/sig.sq + 1/tau.sq)
  )
df_train

### visualize mu_MLE, mu_EB, and N_mid
df_test_A = left_join(df_train, df_test)
df_test_A
df_test_B = df_test_A %>%
  select(bat_id, mu, mu_MLE, mu_EB, N_mid, BA_end, N_end) %>%
  rename(MLE = mu_MLE) %>%
  rename(EB = mu_EB) %>%
  rename(N = N_mid) %>%
  rename(BA = BA_end)
df_test_B

### visualize the shrinkage
plot_shrinkage_BA = df_test_B %>%
  ggplot(aes(x = MLE, y = EB)) +
  geom_vline(aes(xintercept=mu), color="gray80", linetype="dashed", linewidth=1) +
  # geom_hline(aes(yintercept=mu), color="gray80", linetype="dashed", linewidth=1) +
  geom_abline(slope=1, intercept=0, color="gray60", linetype="dashed", linewidth=1) +
  geom_point(aes(size=N, color=N), alpha=0.6) 
plot_shrinkage_BA
ggsave("plots/plot_shrinkage_BA.png", plot_shrinkage_BA,
       width=9, height=6)

### visualize the predictions
plot_preds_BA = df_test_B %>%
  rename(mu_og = mu) %>%
  pivot_longer(c(MLE, EB), names_to = "estimator", values_to = "mu") %>%
  ggplot(aes(x = mu, y = BA, color=estimator)) +
  geom_vline(aes(xintercept=mu_og), color="gray80", linetype="dashed", linewidth=1) +
  geom_abline(slope=1, intercept=0, color="gray60", linetype="dashed", linewidth=1) +
  geom_point(aes(size=N), alpha=0.6) +
  scale_color_manual(values=c("firebrick", "dodgerblue2")) +
  xlab("predicted BA") +
  ylab("BA")
plot_preds_BA
ggsave("plots/plot_preds_BA.png", plot_preds_BA,
       width=9, height=6)

### test rmse
rmse <- function(x,y) sqrt(mean((x-y)**2))
df_rmses = df_test_B %>% summarise(
  rmse_MLE = rmse(MLE,BA), 
  rmse_EB = rmse(EB,BA)
)
df_rmses
gt::gtsave(gt::gt(df_rmses), "plots/rmse_BA.png")

