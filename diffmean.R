d_hat <- gss %>% 
  specify(age ~ college) %>% 
  calculate(stat = "diff in means", order = c("degree", "no degree"))
d_hat

null_dist <- gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("degree", "no degree"))

visualize(null_dist) +
  shade_p_value(obs_stat = d_hat, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = d_hat, direction = "two-sided")

t_hat <- gss %>% 
  specify(age ~ college) %>% 
  calculate(stat = "t", order = c("degree", "no degree"))
t_hat


null_dist <- gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("degree", "no degree"))

null_dist_theory <- gss %>%
  specify(age ~ college) %>%
  assume("t")

visualize(null_dist) +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")


visualize(null_dist_theory) +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")


visualize(null_dist, method = "both") +
  shade_p_value(obs_stat = t_hat, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = t_hat, direction = "two-sided")


d_hat
t_hat

null_dist_gen <- gss %>%
  specify(age ~ college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000000, type = "permute")

diff <- null_dist_gen %>% 
  calculate(stat = "diff in means", order = c("degree", "no degree"))

ttest <- null_dist_gen %>% 
  calculate(stat = "t", order = c("degree", "no degree"))

diff %>%
  get_p_value(obs_stat = d_hat, direction = "greater")

ttest %>%
  get_p_value(obs_stat = t_hat, direction = "greater")
