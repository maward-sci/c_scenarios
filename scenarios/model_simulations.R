source('./model/scen_generation.R')

df <- create_seagrass_exp(model_params, n_sim = 20)

ggplot(df %>% filter(restoration_status=='Restoration')) +
  geom_line(aes(x = year, y = methane, color = sim, group = sim)) +
  facet_grid(~restoration_status)

summary <- summarize_simulations(df)
ggplot(summary) +
  geom_line(aes(x = year, y = methane_mean)) +
  geom_ribbon(aes(x = year, ymin = methane_mean - methane_sd, ymax = methane_mean + methane_sd), inherit.aes=TRUE) +
  facet_grid(treatments~restoration_status)
