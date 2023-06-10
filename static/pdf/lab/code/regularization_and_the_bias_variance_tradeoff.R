

### regularization and the bias variance tradeoff
### Author: Ryan Brill

### packages
library(tidyverse)
library(glmnet)
library(lme4)
library(splines)
library(plotly)
library(ggthemes)
library(cowplot)
library(latex2exp)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

#####################################
### Park Effects Simulation Study ###
#####################################

### read data
park_df = read_csv("data/park_df.csv")
park_df
park_df_1 = park_df %>% filter(YEAR >= 2017) %>% 
  mutate(OT_YR = paste0(OFF_TEAM_ID, YEAR),
         DT_YR = paste0(DEF_TEAM_ID, YEAR))
dim(park_df_1)
head(park_df_1 %>% select(GAME_ID, INNING, OT_YR, DT_YR, PARK, INN_RUNS), n=20)

### 2017-2019 Simulation Setup

### data matrix 
X_df = park_df %>% filter(YEAR >= 2017) %>% 
  mutate(OT_YR = paste0(OFF_TEAM_ID, YEAR),
         DT_YR = paste0(DEF_TEAM_ID, YEAR))
### everything relative to ANA park and ANA2017 
X = as.matrix(modelr::model_matrix(~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data=X_df))

# ### get a sense of the scale of the coefficients via OLS
# m1 = lm(INN_RUNS ~ factor(OFF_TEAM_ID) + factor(DEF_TEAM_ID) + factor(PARK), data=park_df %>% filter(YEAR == 2017))
# coeffs.fit = coefficients(m1)
# alpha.fit = coeffs.fit[1]
# beta.oq.fit = coeffs.fit[str_detect(names(coeffs.fit), "OFF_TEAM_ID")]
# beta.dq.fit = coeffs.fit[str_detect(names(coeffs.fit), "DEF_TEAM_ID")]
# beta.pk.fit = coeffs.fit[str_detect(names(coeffs.fit), "PARK")]
# alpha.fit
# c(mean(beta.oq.fit), sd(beta.oq.fit))
# c(mean(beta.dq.fit), sd(beta.dq.fit))
# c(mean(beta.pk.fit), sd(beta.pk.fit))
# sigma(m1)

### set values of true coefficients
set.seed(12345)
beta.0 = 0.4
beta.ot = rnorm(sum(str_detect(colnames(X), "OT")), 0.02, 0.045)
beta.dt = rnorm(sum(str_detect(colnames(X), "DT")), 0.03, 0.07)
# beta.pk = rnorm(sum(str_detect(colnames(X), "PARK")), 0.04, 0.065)
beta.pk = rnorm(sum(str_detect(colnames(X), "PARK")), 0, 0.065)
# beta.0 = 1
# beta.ot = rnorm(sum(str_detect(colnames(X), "OT")), 0, 0.045)
# beta.dt = rnorm(sum(str_detect(colnames(X), "DT")), 0, 0.07)
# beta.pk = rnorm(sum(str_detect(colnames(X), "PARK")), 0, 0.065)
beta.df = matrix(c(beta.0, beta.pk, beta.ot, beta.dt))
rownames(beta.df) = colnames(X)
names.parks = colnames(X)[str_detect(colnames(X), "PARK")]
names.ot = colnames(X)[str_detect(colnames(X), "OT")]
names.dt = colnames(X)[str_detect(colnames(X), "DT")]
beta.pk.df = tibble(beta.pk, PARK=names.parks) 
beta.pk.df = beta.pk.df %>% mutate(PARK = str_sub(rownames(beta.df)[str_detect(rownames(beta.df), "PARK")], -5, -1))
beta.ot.df = tibble(beta.ot, OT=names.ot)
beta.ot.df = beta.ot.df %>% mutate(OT = str_sub(rownames(beta.df)[str_detect(rownames(beta.df), "OT")], -7, -1))
beta.dt.df = tibble(beta.dt, DT=names.dt)
beta.dt.df = beta.dt.df %>% mutate(DT = str_sub(rownames(beta.df)[str_detect(rownames(beta.df), "DT")], -7, -1))

### visualize the true coefficients
plot_true_beta.pk = beta.pk.df %>%
  ggplot() +
  geom_point(aes(x=beta.pk, y=fct_reorder(PARK, beta.pk))) +
  ylab("park") + xlab("alpha_park")
plot_true_beta.pk
plot_true_beta.ot = beta.ot.df %>%
  ggplot() +
  geom_point(aes(x=beta.ot, y=fct_reorder(OT, beta.ot))) +
  theme(axis.text.y =element_text(size=8)) +
  ylab("offensive team-season") + xlab("beta_ot")
# plot_true_beta.ot
plot_true_beta.dt = beta.dt.df %>%
  ggplot() +
  geom_point(aes(x=beta.dt, y=fct_reorder(DT, beta.dt))) +
  theme(axis.text.y =element_text(size=8)) +
  ylab("defensive team-season") + xlab("gamma_dt")
# plot_true_beta.dt
plot_parkfxSim_true_betas = cowplot::plot_grid(plot_true_beta.pk, plot_true_beta.ot, plot_true_beta.dt, nrow=1)
# plot_parkfxSim_true_betas
cowplot::save_plot("plots/plot_parkfxSim_true_betas.png", plot_parkfxSim_true_betas, 
                   base_height=15, nrow=1)

### simulate the response vector y 25 times
set.seed(999)
all(rownames(beta.df) == colnames(X))
Xb = X %*% beta.df
NUM.SIMS = 5 #25
sig = 1
y = matrix(nrow=nrow(Xb), ncol=NUM.SIMS)
for (i in 1:nrow(y)) {
  for (j in 1:ncol(y)) {
    y[i,j] = truncnorm::rtruncnorm(n=1, a=0, mean = Xb[i,1], sd = sig)
  }
}
#### y = cbind(Xb,Xb,Xb,Xb,Xb) + y
y = round(y) # integer-valued response
y[1:10]

### use linear regression to estimate the coefficients
coeffs_pk = matrix(nrow=nrow(beta.pk.df), ncol=NUM.SIMS)
rownames(coeffs_pk) = beta.pk.df$PARK
colnames(coeffs_pk) = paste0("beta.pk.sim",1:NUM.SIMS)
coeffs_pk3 = coeffs_pk
for (i in 1:NUM.SIMS) {
  print(paste0("sim ", i))
  X_i = X_df %>% mutate(y = y[,i])
  # linear regression
  lm_i = lm(y ~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data=X_i)
  coeffs_pk[,i] = coefficients(lm_i)[str_detect(names(coefficients(lm_i)), "PARK")]
  # ridge regression
  ridge3_i = glmnet(
    x = model.matrix(~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data=X_i),
    y = X_i$y, alpha = 0, family="gaussian", lambda = 0.2
  )
  coeffs_ridge3_i = coef(ridge3_i)[,1]
  coeffs_pk3[,i]  = coeffs_ridge3_i[str_detect(names(coeffs_ridge3_i), "PARK")]
}

### visualize the park effects across the different sims
beta.pk.df.sim = 
  beta.pk.df %>%
  rename(beta.pk.true = beta.pk) %>%
  cbind(coeffs_pk)
  # cbind(coeffs_pk3)
# beta.pk.df.sim
plot_parkfxSim_beta.pk.df = beta.pk.df.sim %>%
  mutate(PARK = fct_reorder(PARK,beta.pk.true)) %>%
  pivot_longer(-PARK) %>%
  mutate(alpha = str_sub(name, start=9)) %>%
  ggplot() +
  labs(title=paste0("estimated OLS park effects across M=",NUM.SIMS," sims")) +
  xlab("estimated park effect") +
  geom_point(aes(y=PARK,x=value, color=alpha, shape=alpha=="true"), size=5)
plot_parkfxSim_beta.pk.df
ggsave("plots/plot_parkfxSim_beta.pk.df.png", width=14, height=10)

### visualize mean park effect
plot_parkfxSim_beta.pk.mean.df = cbind(
  beta.pk.df.sim[,1:2],
    do.call("rbind", 
            replicate(nrow(beta.pk.df.sim), 
                      colMeans(beta.pk.df.sim[,3:(3+NUM.SIMS-1)]),
                      simplify = FALSE)
     )
  ) %>%
  mutate(PARK = fct_reorder(PARK,beta.pk.true)) %>%
  pivot_longer(-PARK) %>%
  mutate(alpha = str_sub(name, start=9)) %>%
  ggplot() +
  labs(title=paste0("estimated mean park effects across M=",NUM.SIMS," sims")) +
  xlab("estimated mean park effect") +
  geom_point(aes(y=PARK,x=value, color=alpha, shape=alpha=="true"), size=5)
plot_parkfxSim_beta.pk.mean.df
ggsave("plots/plot_parkfxSim_beta.pk.mean.df.png", width=14, height=10)


### visualize ridge park effects across the different sims
beta.pk.df.sim_ridge = 
  beta.pk.df %>%
  rename(beta.pk.true = beta.pk) %>%
  cbind(coeffs_pk3)
plot_parkfxSim_beta.pk.df_ridge = beta.pk.df.sim_ridge %>%
  mutate(PARK = fct_reorder(PARK,beta.pk.true)) %>%
  pivot_longer(-PARK) %>%
  mutate(alpha = str_sub(name, start=9)) %>%
  ggplot() +
  labs(title=paste0("estimated Ridge park effects across M=",NUM.SIMS," sims")) +
  xlab("estimated park effect") +
  geom_point(aes(y=PARK,x=value, color=alpha, shape=alpha=="true"), size=5)
plot_parkfxSim_beta.pk.df_ridge
ggsave("plots/plot_parkfxSim_beta.pk.df_ridge.png", width=14, height=10)


### error calculations
rmse <- function(x,y) { sqrt( mean( (x-y)^2 ) ) }
err <- function(df) {
  mean(
    sapply(1:NUM.SIMS,
           function(i) {
             rmse(
               df$beta.pk.true,
               df[,paste0("beta.pk.sim",i)]
             )
           })
  )
}
### error
err(beta.pk.df.sim)
err(beta.pk.df.sim_ridge)
### error on non-outliers 
err(beta.pk.df.sim %>% filter( abs(beta.pk.true) < 0.05) )
err(beta.pk.df.sim_ridge %>% filter( abs(beta.pk.true) < 0.05) )
### error on outliers 
err(beta.pk.df.sim %>% filter( abs(beta.pk.true) >= 0.05) )
err(beta.pk.df.sim_ridge %>% filter( abs(beta.pk.true) >= 0.05) )


### park effects from observed data
# OLS
obs.pfx.lm = lm(INN_RUNS ~ factor(PARK) + factor(OT_YR) + factor(DT_YR), data = park_df_1)
obs.pfx.lm.coeffs = coefficients(obs.pfx.lm)[str_detect(names(coefficients(obs.pfx.lm)), "PARK")]
# ridge regression
obs.pfx.ridge = glmnet(
  x = model.matrix(INN_RUNS ~ factor(OT_YR) + factor(DT_YR) + factor(PARK), data=park_df_1),
  y = park_df_1$INN_RUNS, alpha = 0, family="gaussian", lambda = 0.25
)
obs.pfx.ridge.coeffs = coef(obs.pfx.ridge)[,1]
obs.pfx.ridge.coeffs  = obs.pfx.ridge.coeffs[str_detect(names(obs.pfx.ridge.coeffs), "PARK")]
# visualize
plot_obs_pfx = tibble(ols = obs.pfx.lm.coeffs, ridge = obs.pfx.ridge.coeffs, PARK=str_sub(names(obs.pfx.ridge.coeffs), start=-5) )  %>%
  # mutate(ols = ols-mean(ols), ridge=ridge-mean(ridge)) %>%
  mutate(PARK=fct_reorder(PARK, obs.pfx.lm.coeffs)) %>%
  pivot_longer(-PARK) %>%
  rename(method=name, alpha=value) %>%
  ggplot() +
  geom_point(aes(y=PARK, x=alpha, color=method), size=3)
plot_obs_pfx
ggsave("plots/plot_parkfx_obs.png", width=8, height=7)

