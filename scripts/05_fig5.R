# scripts/05_fig5.R
source("scripts/00_packages.R")

if (!exists("ct_grsm")) ct_grsm <- readRDS("models/ct_grsm.rds")
if (!exists("bf_grsm")) bf_grsm <- readRDS("models/bf_grsm.rds")

item_order <- c(
  "Bm_1_r", "Bm_2_r", "Bm_3_r", "Bm_4_r", "Bm_5", "Bm_6", "Bm_7", "Bm_8",
  "Bl_1_r", "Bl_2", "Bl_3_r", "Bl_4_r", "Bl_5", "Bl_6_r", "Bl_7_r", "Bl_8_r", 
  "Bl_9_r", "Bl_10_r", "Bl_11_r", "Bp_1_r", "Bp_2_r", "Bp_3", "Bp_4", "Bp_5"
)

custom_colors <- c(
  "Bm (Math)"     = "#555555", 
  "Bl (Learner)"  = "#E69F00", 
  "Bp (Pedagogy)" = "#0072B2"  
)

# --- A) CT-GRSM Difficulty ---
ranef_ct <- ranef(ct_grsm)
df_diff_ct <- ranef_ct$item[, , "Intercept"] %>%
  as_tibble(rownames = "item") %>%
  mutate(
    dimension = case_when(
      grepl("^Bm", item) ~ "Bm (Math)",
      grepl("^Bl", item) ~ "Bl (Learner)",
      grepl("^Bp", item) ~ "Bp (Pedagogy)"
    ),
    item = factor(item, levels = item_order)
  )

# --- B) CT-GRSM Discrimination (Exp Transform) ---
df_disc_ct <- exp(ranef_ct$item[, , "disc_Intercept"]) %>% 
  as_tibble(rownames = "item") %>%
  mutate(
    dimension = case_when(
      grepl("^Bm", item) ~ "Bm (Math)",
      grepl("^Bl", item) ~ "Bl (Learner)",
      grepl("^Bp", item) ~ "Bp (Pedagogy)"
    ),
    item = factor(item, levels = item_order)
  )

# --- C) BF-GRSM Difficulty ---
ranef_bf <- ranef(bf_grsm)
df_diff_bf <- ranef_bf$item[, , "Intercept"] %>%
  as_tibble(rownames = "item") %>%
  mutate(
    dimension = case_when(
      grepl("^Bm", item) ~ "Bm (Math)",
      grepl("^Bl", item) ~ "Bl (Learner)",
      grepl("^Bp", item) ~ "Bp (Pedagogy)"
    ),
    item = factor(item, levels = item_order)
  )

# --- D) BF-GRSM Discrimination (Exp Transform) ---
df_disc_bf <- exp(ranef_bf$item[, , "disc_Intercept"]) %>% 
  as_tibble(rownames = "item") %>%
  mutate(
    dimension = case_when(
      grepl("^Bm", item) ~ "Bm (Math)",
      grepl("^Bl", item) ~ "Bl (Learner)",
      grepl("^Bp", item) ~ "Bp (Pedagogy)"
    ),
    item = factor(item, levels = item_order)
  )

# P1: CT Difficulty
p1 <- ggplot(df_diff_ct, aes(x = Estimate, y = item, color = dimension)) +
  geom_point(size = 1.5) + 
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.3, linewidth = 0.6) + 
  scale_color_manual(values = custom_colors) +
  labs(title = "CT-GRSM: Difficulty", x = NULL, y = NULL) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", color = "black"),
    plot.title = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6, color = "black")
  )

# P2: CT Discrimination
p2 <- ggplot(df_disc_ct, aes(x = Estimate, y = item, color = dimension)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_point(size = 1.5) + 
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.3, linewidth = 0.6) + 
  scale_color_manual(values = custom_colors) +
  labs(title = "CT-GRSM: Discrimination", x = NULL, y = NULL) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", color = "black"),
    plot.title = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.y = element_blank()
  )

# P3: BF Difficulty
p3 <- ggplot(df_diff_bf, aes(x = Estimate, y = item, color = dimension)) +
  geom_point(size = 1.5) + 
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.3, linewidth = 0.6) + 
  scale_color_manual(values = custom_colors) +
  labs(title = "BF-GRSM: Difficulty", x = "Estimate", y = NULL) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", color = "black"),
    plot.title = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6, color = "black")
  )

# P4: BF Discrimination
p4 <- ggplot(df_disc_bf, aes(x = Estimate, y = item, color = dimension)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_point(size = 1.5) + 
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.3, linewidth = 0.6) + 
  scale_color_manual(values = custom_colors) +
  labs(title = "BF-GRSM: Discrimination", x = "Estimate", y = NULL) +
  theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", color = "black"),
    plot.title = element_text(size = 8, face = "bold"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.y = element_blank()
  )

fig_5 <- (p1 | p2) / (p3 | p4)

print(fig_5)

ggsave("figures/fig5_model_comparison.png", 
       fig_5, 
       width = 6.5,
       height = 6, 
       dpi = 600,
       bg = "white")