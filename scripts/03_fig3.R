# scripts/03_fig3.R

source("scripts/00_packages.R")

data_1 <- readRDS("data/processed/data_1.rds")

plot_data <- data_1 %>%
  mutate(
    dimension = case_when(
      grepl("^Bm", item) ~ "Bm (Math)",
      grepl("^Bl", item) ~ "Bl (Learner)",
      grepl("^Bp", item) ~ "Bp (Pedagogy)"
    ),
    dimension = factor(dimension, levels = c("Bm (Math)", "Bl (Learner)", "Bp (Pedagogy)"))
  ) %>%
  count(item, dimension, response) %>%
  group_by(item) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(response = factor(response, levels = 1:5))

custom_colors <- c(
  "Bm (Math)"     = "#555555", 
  "Bl (Learner)"  = "#E69F00", 
  "Bp (Pedagogy)" = "#0072B2"  
)

fig_3 <- ggplot(plot_data, aes(x = response, y = percentage, fill = dimension)) +
  geom_col(width = 0.75, color = NA) + 
  
  facet_wrap(~item, ncol = 6) +
  scale_fill_manual(values = custom_colors) +
  
  labs(
    title = NULL,
    x = NULL,
    y = "Percentage (%)",
    fill = "Dimension"
  ) +
  
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 100)) +
  
  theme_minimal(base_size = 10) +
  
  theme(
    text = element_text(family = "sans"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3), 
    panel.grid.minor = element_blank(),
    
    panel.border = element_rect(color = "grey85", fill = NA, linewidth = 0.5),
    
    axis.line.x = element_line(color = "grey80", linewidth = 0.3),
    axis.text = element_text(color = "grey30", size = 7), 
    axis.title.y = element_text(size = 9, margin = margin(r = 8)),
    
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold", size = 8),

    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.margin = margin(l = 5)
  )

ggsave(
  filename = "figures/fig3_response_distribution.png", 
  plot = fig_3, 
  width = 7.5, 
  height = 5.5, 
  dpi = 300, 
  bg = "white",
  scale = 1 
)