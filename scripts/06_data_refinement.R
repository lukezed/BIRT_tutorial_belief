# scripts/06_data_refinement.R
source(here::here("scripts", "00_packages.R"))

  data_2 <- readRDS(here("data", "processed", "data_2.rds"))

items_to_remove <- c("Bm_1_r", "Bm_2_r", "Bm_3_r", "Bm_4_r")

data_3 <- data_2 %>%
  filter(!item %in% items_to_remove) %>%
  mutate(item = fct_drop(item))

saveRDS(data_3, here("data", "processed", "data_3.rds"))