# scripts/01_data_cleaning.R

source("scripts/00_packages.R")
raw_data <- read_csv("data/raw/belief_clean.csv")

df_renamed <- raw_data
colnames(df_renamed) <- gsub("^Ba_", "Bm_", colnames(df_renamed))
colnames(df_renamed) <- gsub("^Bb_", "Bl_", colnames(df_renamed))
colnames(df_renamed) <- gsub("^Bc_", "Bp_", colnames(df_renamed))

items_all <- c(
  "Bm_1_r", "Bm_2_r", "Bm_3_r", "Bm_4_r", "Bm_5", "Bm_6", "Bm_7", "Bm_8",
  "Bl_1_r", "Bl_2", "Bl_3_r", "Bl_4_r", "Bl_5", "Bl_6_r", "Bl_7_r", "Bl_8_r",
  "Bl_9_r", "Bl_10_r", "Bl_11_r", "Bp_1_r", "Bp_2_r", "Bp_3", "Bp_4", "Bp_5"
)

df_valid <- df_renamed %>%
  filter(!if_all(all_of(items_all), is.na)) %>%
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    YearG = case_when(
      YearG == 1 ~ "Year7",
      YearG == 2 ~ "Year8",
      YearG == 3 ~ "Year9",
      TRUE ~ NA_character_
    ),
    
    gender = factor(gender, levels = c("Female", "Male")), 
    YearG = factor(YearG, levels = c("Year7", "Year8", "Year9")),
    District = as.character(District),
    District = factor(District, levels = c("1", "2", "3"))
  )

data_1 <- df_valid %>%
  select(id, gender, YearG, District, all_of(items_all)) %>%
  pivot_longer(
    cols = all_of(items_all), 
    names_to = "item", 
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%
  mutate(
    response = as.integer(response),
    
    dim_m = if_else(grepl("^Bm", item), 1, 0),
    dim_l = if_else(grepl("^Bl", item), 1, 0),
    dim_p = if_else(grepl("^Bp", item), 1, 0),

    id = factor(id),
    item = factor(item, levels = items_all)
  )


if(!dir.exists("data/processed")) 
  dir.create("data/processed", recursive = TRUE)

saveRDS(data_1, "data/processed/data_1.rds")