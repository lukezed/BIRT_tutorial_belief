source("scripts/00_packages.R")

bf_grm_dif  <- readRDS("models/bf_grm_dif.rds") 
bf_grm_dif2 <- readRDS("models/bf_grm_dif2.rds")

colors_dim <- c(
  "Bm (Math)"     = "#555555", 
  "Bl (Learner)"  = "#E69F00", 
  "Bp (Pedagogy)" = "#0072B2"
)

item_order_final <- c(
  "Bm_5", "Bm_6", "Bm_7", "Bm_8",
  "Bl_1_r", "Bl_2", "Bl_3_r", "Bl_4_r", "Bl_5", "Bl_6_r", "Bl_7_r", "Bl_8_r", 
  "Bl_9_r", "Bl_10_r", "Bl_11_r", 
  "Bp_1_r", "Bp_2_r", "Bp_3", "Bp_4", "Bp_5"
)


theme_square <- theme_classic(base_size = 9) + 
  theme(
    text = element_text(family = "Helvetica", color = "black"), 
    plot.title = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7, color = "black"),
    
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    aspect.ratio = 1,
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-5,-5,-5,-5),
    legend.text = element_text(size = 8) 
  )

# ==============================================================================
# P1: Gender Diagonal (Top-Left)
# ==============================================================================
ranef_dif <- ranef(bf_grm_dif)$item
df_female <- ranef_dif[, , "genderFemale"] %>% as_tibble(rownames = "item") %>% select(item, F_Est = Estimate, F_Low = Q2.5, F_High = Q97.5)
df_male <- ranef_dif[, , "genderMale"] %>% as_tibble(rownames = "item") %>% select(item, M_Est = Estimate, M_Low = Q2.5, M_High = Q97.5)

df_diag <- left_join(df_female, df_male, by = "item") %>%
  mutate(
    dimension = case_when(grepl("^Bm", item) ~ "Bm (Math)", grepl("^Bl", item) ~ "Bl (Learner)", grepl("^Bp", item) ~ "Bp (Pedagogy)"),
    is_dif = (F_High < M_Low) | (F_Low > M_High),
    alpha_val = if_else(is_dif, 1, 0.5), shape_val = if_else(is_dif, 19, 1)
  )

p1 <- ggplot(df_diag, aes(x = F_Est, y = M_Est, color = dimension)) +
  geom_abline(intercept = 0, slope = 1, color = "grey40", linetype = "dashed") +
  geom_errorbar(aes(ymin = M_Low, ymax = M_High, alpha = alpha_val), width = 0, linewidth = 0.7) +
  geom_errorbar(aes(xmin = F_Low, xmax = F_High, alpha = alpha_val), width = 0, linewidth = 0.7) +
  geom_point(aes(shape = shape_val, alpha = alpha_val), size = 1.5) + 
  geom_text_repel(aes(label = ifelse(is_dif, as.character(item), "")), size = 2.5, fontface = "bold", max.overlaps = 20, show.legend = FALSE) +
  scale_color_manual(values = colors_dim) + scale_shape_identity() + scale_alpha_identity() + 
  coord_fixed(ratio = 1) + 
  labs(title = "A.", x = "Female", y = "Male") +
  theme_square + theme(legend.position = "none")

# ==============================================================================
# P2: Gender Forest (Top-Right)
# ==============================================================================
ranef_dif2 <- ranef(bf_grm_dif2)$item
df_gen_for <- ranef_dif2[, , "genderMale"] %>%
  as_tibble(rownames = "item") %>%
  mutate(
    dimension = case_when(grepl("^Bm", item) ~ "Bm (Math)", grepl("^Bl", item) ~ "Bl (Learner)", grepl("^Bp", item) ~ "Bp (Pedagogy)"),
    item = factor(item, levels = item_order_final) 
  )

p2 <- ggplot(df_gen_for, aes(x = Estimate, y = item, color = dimension)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0, linewidth = 0.6) +
  geom_point(size = 2) + 
  scale_color_manual(values = colors_dim) +
  labs(title = "B.", x = "Log-odds (Male - Female)", y = NULL) +
  theme_square + theme(legend.position = "none")

# ==============================================================================
# P3: District Forest (Bottom-Left)
# ==============================================================================
df_d2 <- ranef_dif2[, , "District2"] %>% as_tibble(rownames = "item") %>% mutate(district = "District 2")
df_d3 <- ranef_dif2[, , "District3"] %>% as_tibble(rownames = "item") %>% mutate(district = "District 3")
district_effects <- bind_rows(df_d2, df_d3) %>% mutate(item = factor(item, levels = item_order_final))

p3 <- ggplot(district_effects, aes(x = Estimate, y = item, color = district)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0, linewidth = 0.6, position = position_dodge(width = 0.6)) +
  geom_point(size = 1, position = position_dodge(width = 0.6)) + 
  labs(title = "C.", x = "Log-odds Difference", y = NULL) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  theme_square + 
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  guides(color = guide_legend(override.aes = list(size = 1.5)))

# ==============================================================================
# P4: District Effect (Bottom-Right)
# ==============================================================================
p4_eff_dist <- conditional_effects(bf_grm_dif2, effects = "District", categorical = TRUE)

df_p4 <- p4_eff_dist$District

p4 <- ggplot(df_p4, aes(x = District, y = estimate__, color = cats__)) +
  geom_errorbar(aes(ymin = lower__, ymax = upper__), 
                width = 0.3, linewidth = 0.6, position = position_dodge(width = 0.5)) +
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) + 
  
  labs(title = "D.", x = "District", y = "Probability") +
  theme_square + 
  theme(
    legend.position = "bottom", 
    legend.title = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(size = 1.5)))

fig_7 <- (p1 | p2) / (p3 | p4)

print(fig_7)

ggsave("figures/fig7_combined_dif.png",
       fig_7, 
       width = 6.5, 
       height = 7,
       dpi = 600,
       bg = "white")