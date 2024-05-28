
source("0_clean_lm.R")
HYPERPARAM_COMBO_IDX = 3 #1
source("sim_2_main.R")

#################################################################
### Aggregate the Loss, Coverage, and Length over the 25 sims ###
#################################################################

loss_df = tibble()
loss_df_binned = tibble()
covg_df = tibble()
covg_df_binned = tibble()

for (SIM_NUM in 1:B) {
  print(paste0("***** ", "aggregating results of sim #", SIM_NUM, " *****"))
  sim_str = paste0("sim2_idx", SIM_NUM, "_G", G, "_N", N, "_K", K, "_L", L)
  
  loss_df_s = read_csv(paste0("job_output/", sim_str, "_loss_df.csv"))
  loss_df = bind_rows(loss_df, loss_df_s)
  
  loss_df_binned_s = read_csv(paste0("job_output/", sim_str, "_loss_results_df_binned.csv"))
  loss_df_binned = bind_rows(loss_df_binned, loss_df_binned_s)
  
  covg_df_s = read_csv(paste0("job_output/", sim_str, "_covg_df.csv"))
  covg_df = bind_rows(covg_df, covg_df_s)
  
  covg_df_binned_s = read_csv(paste0("job_output/", sim_str, "_covg_df_binned.csv"))
  covg_df_binned = bind_rows(covg_df_binned, covg_df_binned_s)
  
  # B_list = c(10,25)
  # for (BB in B_list) {
  #   if (B >= BB) {
  #     covg_df_BB = read_csv(paste0("job_output/", sim_str, "_covg_df_B", BB, ".csv"))
  #     covg_df = bind_rows(covg_df, covg_df_BB)
  #   }
  # }
}

################################################################################

### loss DF 
loss_df_A = loss_df %>%
  summarise(
    logloss_wp_y = mean(logloss_wp_y),
    brier_wp_y = mean(brier_wp_y),
    brier_wp_pred = mean(brier_wp_pred), 
    mae_wp_pred = mean(mae_wp_pred),
  ) %>% round_df(digits=6)
print(data.frame(loss_df_A))
# library(gt)
# gtsave(gt(loss_df_A), paste0(sim_str, "_loss_df.png"))

### CI (covg,length) DF 
covg_df_A = covg_df %>%
  group_by(B,brp) %>%
  summarise(
    covered_iidb = mean(covered_iidb),
    covered_cb = mean(covered_cb),
    covered_rcb = mean(covered_rcb),
    covered_iidb_2 = mean(covered_iidb_2),
    covered_cb_2 = mean(covered_cb_2),
    covered_rcb_2 = mean(covered_rcb_2),
    length_iidb = mean(length_iidb),
    length_cb = mean(length_cb),
    length_rcb = mean(length_rcb),
    length_iidb_2 = mean(length_iidb_2),
    length_cb_2 = mean(length_cb_2),
    length_rcb_2 = mean(length_rcb_2),
  ) %>% round_df(digits=4)
print(data.frame(covg_df_A))

### combined data frame
library(htmlTable)
library(kableExtra)
plot_combined_0 = concatHtmlTables(list(htmlTable(loss_df_A), 
                      htmlTable(covg_df_A)), 
                 headers = c("losses", "CI coverages and lengths")
                 )
# plot_combined_0
save_kable(plot_combined_0, file = paste0(sim_str, "_plot.png"))

################################################################################

## PLOT bias as a function of actual WP
plot_loss_df_binned = loss_df_binned %>%
  group_by(wp_actual_bin) %>%
  summarise(
    mae_wp_pred = mean(mae_wp_pred)
  ) %>%
  ggplot(aes(x = wp_actual_bin, y = mae_wp_pred)) +
  geom_point(size=2) +
  ylab("MAE") +
  # ylab("MAE(true WP, estimated WP)") +
  xlab("true win probability") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
# plot_loss_df_binned
ggsave(paste0(sim_str, "_plot_loss_df_binned.png"), plot_loss_df_binned, width=15, height=5)

## PLOT CI covg as a function of actual WP
plot_covg_df_binned = covg_df_binned %>%
  filter(brp == 0.5) %>%
  group_by(wp_actual_bin) %>%
  summarise(
    covered_rcb = mean(covered_rcb),
    covered_rcb_2 = mean(covered_rcb_2),
  ) %>% 
  pivot_longer(cols = c(covered_rcb, covered_rcb_2)) %>%
  ggplot(aes(x = wp_actual_bin, y=value, color=name, size=name)) +
  geom_point() +
  # geom_point(aes(y = covered_rcb_2), size=3.5, color="magenta") +
  # geom_point(aes(y = covered_rcb), size=2) +
  # scale_alpha_manual(
  #   values = c("covered_rcb" = 1, "covered_rcb_2" = 0.5),
  #   guide = 'none'
  # ) +
  scale_color_manual(
    values = c("covered_rcb_2" = "magenta", "covered_rcb" = "black"),
    name="",
    labels = c("covered_rcb" = "randomized\ncluster\nbootstrap\n", "covered_rcb_2" = "randomized\ncluster\nbootstrap\nadjusted\nnear\n0 and 1\n"),
  ) +
  scale_size_manual(
    values = c("covered_rcb" = 5, "covered_rcb_2" = 2.5),
    guide = 'none'
  ) +
  theme(
    legend.title=element_text(size=15), 
    legend.text=element_text(size=15)
  ) +
  scale_y_continuous(breaks=seq(0,1,by=0.1)) +
  # geom_point(aes(y = covered_rcb), color="black", size=2) +
  ylab("coverage") +
  xlab("true win probability") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
# plot_covg_df_binned
ggsave(paste0(sim_str, "_plot_covg_df_binned.png"), plot_covg_df_binned, width=15, height=5)

## PLOT CI upper and lower covg as a function of actual WP
plot_covg_UL_df_binned = covg_df_binned %>%
  filter(brp == 0.5) %>%
  group_by(wp_actual_bin) %>%
  summarise(
    covered_rcb = mean(covered_rcb),
    covered_rcb_L = mean(covered_rcb_L),
    covered_rcb_U = mean(covered_rcb_U),
  ) %>% 
  pivot_longer(cols=c(covered_rcb_L, covered_rcb_U)) %>% 
  ggplot(aes(x = wp_actual_bin)) +
  geom_point(aes(y = value, color=name), size=2) +
  scale_color_manual(values=c("covered_rcb_L" = "dodgerblue2", "covered_rcb_U" = "firebrick"),
                     name="",
                     labels=c("covered_rcb_L" = "randomized\ncluster\nbootstrap\nlower\ncoverage\n", 
                              "covered_rcb_U" = "randomized\ncluster\nbootstrap\nupper\ncoverage\n")) +
  scale_y_continuous(breaks=seq(0,1,by=0.1)) +
  # geom_point(aes(y = covered_rcb), color="black", size=2) +
  # geom_point(aes(y = covered_rcb_L), color="dodgerblue2", size=2) +
  # geom_point(aes(y = covered_rcb_U), color="firebrick", size=2) +
  ylab("coverage") +
  xlab("true win probability") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13)) +
  theme(
    legend.title=element_text(size=15), 
    legend.text=element_text(size=15)
  ) 
# plot_covg_UL_df_binned
ggsave(paste0(sim_str, "_plot_covg_UL_df_binned.png"), plot_covg_UL_df_binned, width=15, height=5)

## PLOT CI length as a function of actual WP
plot_ci_len_df_binned = covg_df_binned %>%
  filter(brp == 0.5) %>%
  group_by(wp_actual_bin) %>%
  summarise(
    length_rcb = mean(length_rcb),
    length_rcb_2 = mean(length_rcb_2)
  ) %>%
  pivot_longer(cols = c(length_rcb, length_rcb_2)) %>%
  ggplot(aes(x = wp_actual_bin, y=value, color=name, size=name)) +
  geom_point() +
  # ggplot(aes(x = wp_actual_bin, y = length_rcb)) +
  scale_color_manual(
    values = c("length_rcb_2" = "magenta", "length_rcb" = "black"),
    name="",
    labels = c("length_rcb" = "randomized\ncluster\nbootstrap\n", "length_rcb_2" = "randomized\ncluster\nbootstrap\nadjusted\nnear\n0 and 1\n"),
  ) +
  scale_size_manual(
    values = c("length_rcb" = 5, "length_rcb_2" = 2.5),
    guide = 'none'
  ) +
  theme(
    legend.title=element_text(size=15), 
    legend.text=element_text(size=15)
  ) +
  geom_point(size=2) +
  ylab("CI length") +
  xlab("true win probability") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
# plot_ci_len_df_binned
ggsave(paste0(sim_str, "_plot_ci_len_df_binned.png"), plot_ci_len_df_binned, width=15, height=5)

### combined plot
plot_combined = cowplot::plot_grid(plot_loss_df_binned, plot_covg_UL_df_binned, plot_covg_df_binned, plot_ci_len_df_binned, ncol=1)
# plot_combined
save_plot(paste0(sim_str, "_plot_wp_binned.png"), plot_combined, base_width=18, base_height=21)



