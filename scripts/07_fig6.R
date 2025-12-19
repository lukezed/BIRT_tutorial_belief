# scripts/07_fig6.R
source("scripts/00_packages.R")
bf_grm_new <- readRDS("models/bf_grm_new.rds")

item_order_final <- c(
  "Bm_5", "Bm_6", "Bm_7", "Bm_8",
  "Bl_1_r", "Bl_2", "Bl_3_r", "Bl_4_r", "Bl_5", "Bl_6_r", "Bl_7_r", "Bl_8_r", 
  "Bl_9_r", "Bl_10_r", "Bl_11_r", 
  "Bp_1_r", "Bp_2_r", "Bp_3", "Bp_4", "Bp_5"
)

colors_dim <- c(
  "Bm (Math)"     = "#555555", 
  "Bl (Learner)"  = "#E69F00", 
  "Bp (Pedagogy)" = "#0072B2"
)

ranef_all <- ranef(bf_grm_new)


# Data Preparation
df_diff <- ranef_all$item[, , "Intercept"] %>%
  as_tibble(rownames = "item") %>%
  mutate(
    dimension = case_when(
      grepl("^Bm", item) ~ "Bm (Math)",
      grepl("^Bl", item) ~ "Bl (Learner)",
      grepl("^Bp", item) ~ "Bp (Pedagogy)"
    ),
    item = factor(item, levels = item_order_final)
  )

df_disc <- exp(ranef_all$item[, , "disc_Intercept"]) %>%
  as_tibble(rownames = "item") %>%
  mutate(
    dimension = case_when(
      grepl("^Bm", item) ~ "Bm (Math)",
      grepl("^Bl", item) ~ "Bl (Learner)",
      grepl("^Bp", item) ~ "Bp (Pedagogy)"
    ),
    item = factor(item, levels = item_order_final)
  )

# Plot 1: Difficulty
p1 <- ggplot(df_diff, aes(x = Estimate, y = item, color = dimension)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.3, linewidth = 0.6) + 
  scale_color_manual(values = colors_dim) +
  labs(title = "Item Difficulty", x = NULL, y = NULL) +
  theme(legend.position = "none")

# Plot 2: Discrimination
p2 <- ggplot(df_disc, aes(x = Estimate, y = item, color = dimension)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.3, linewidth = 0.6) + 
  scale_color_manual(values = colors_dim) +
  labs(title = "Item Discrimination", x = NULL, y = NULL) +
  theme(legend.position = "none")


df_gen <- ranef_all$id[, , "Intercept"] %>%
  as_tibble(rownames = "person") %>% arrange(Estimate) %>% mutate(id = row_number())

df_m <- ranef_all$id[, , "dim_m"] %>%
  as_tibble(rownames = "person") %>% arrange(Estimate) %>% mutate(id = row_number())

df_l <- ranef_all$id[, , "dim_l"] %>%
  as_tibble(rownames = "person") %>% arrange(Estimate) %>% mutate(id = row_number())

df_p <- ranef_all$id[, , "dim_p"] %>%
  as_tibble(rownames = "person") %>% arrange(Estimate) %>% mutate(id = row_number())

all_values <- c(
  df_gen$Q2.5, df_gen$Q97.5,
  df_m$Q2.5,   df_m$Q97.5,
  df_l$Q2.5,   df_l$Q97.5,
  df_p$Q2.5,   df_p$Q97.5
)
global_ylim <- range(all_values, na.rm = TRUE)

# P3: General
p3 <- ggplot(df_gen, aes(x = id, y = Estimate)) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), 
                  color = "black", alpha = 0.9, linewidth = 0.4, size = 0.2) +
  coord_flip() +
  ylim(global_ylim) + 
  labs(title = "General Belief", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank())

# P4: Math
p4 <- ggplot(df_m, aes(x = id, y = Estimate)) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), 
                  color = "#555555", alpha = 0.9, linewidth = 0.4, size = 0.2) +
  coord_flip() +
  ylim(global_ylim) + 
  labs(title = "Belief about Math", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank())

# P5: Learner
p5 <- ggplot(df_l, aes(x = id, y = Estimate)) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), 
                  color = "#E69F00", alpha = 0.9, linewidth = 0.4, size = 0.2) +
  coord_flip() +
  ylim(global_ylim) + 
  labs(title = "Belief about Learners", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank())

# P6: Pedagogy
p6 <- ggplot(df_p, aes(x = id, y = Estimate)) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), 
                  color = "#0072B2", alpha = 0.9, linewidth = 0.4, size = 0.2) +
  coord_flip() +
  ylim(global_ylim) + 
  labs(title = "Belief about Pedagogy", x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank())



fig_6 <- (p1 | p2) / (p3 | p4) / (p5 | p6) + 
  plot_layout(heights = c(1.4, 1, 1))

print(fig_6)

ggsave("figures/fig6_final_diagnostics.png", fig_6, width = 7.5, height = 9, dpi = 300, bg = "white")