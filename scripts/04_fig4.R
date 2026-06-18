# scripts/04_fig4.R
source(here::here("scripts", "00_packages.R"))

if (!exists("merge_rsm")) {
  merge_rsm <- readRDS(here("models", "merge_rsm.rds"))
}

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

# Extract random effects
ranef_wc <- ranef(merge_rsm)

# Difficulty plot (P1)
p1 <- ranef_wc$item[, , "Intercept"] %>%
  as_tibble(rownames = "item") %>%
  mutate(
    dimension = case_when(
      grepl("^Bm", item) ~ "Bm (Math)",
      grepl("^Bl", item) ~ "Bl (Learner)",
      grepl("^Bp", item) ~ "Bp (Pedagogy)"
    ),
    item = factor(item, levels = item_order)
  ) %>%
  ggplot(aes(x = Estimate, y = item, color = dimension)) +
  
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin = Q2.5, xmax = Q97.5), width = 0.3, linewidth = 0.6) + 
  
  scale_color_manual(values = custom_colors) +
  labs(title = "Item Difficulty", x = "Estimate", y = NULL) +
theme_classic() +
  theme(
    legend.position = "none",
    text = element_text(family = "Helvetica", color = "black"),
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 7, color = "black")
  )

p2 <- ranef_wc$id[, , "Intercept"] %>%
  as_tibble(rownames = "person") %>%
  arrange(Estimate) %>%
  mutate(id = row_number()) %>%
  ggplot(aes(x = id, y = Estimate)) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), 
                  alpha = 0.8, linewidth = 0.7, size = 0.2, 
                  color = "black") + 
  coord_flip() +
  labs(title = "Person Parameters", x = NULL, y = "Estimate") +
  theme_classic() + 
  theme(
    text = element_text(family = "Helvetica", color = "black"),
    plot.title = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 7, color = "black")
  )


fig_4 <- p1 | p2
print(fig_4)

ggsave(
  filename = here("figures", "fig4_wright_map.png"), 
  plot = fig_4, 
  width = 6.5, 
  height = 3.5, 
  dpi = 600, 
  bg = "white"
)