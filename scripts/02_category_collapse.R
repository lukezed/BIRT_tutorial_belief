# scripts/02_category_collapse.R

# Resource loading
source("scripts/00_packages.R")
data_1 <- readRDS("data/processed/data_1.rds")

# 1(Strongly Disagree) & 2(Disagree) -> 1

data_2 <- data_1 %>%
  mutate(
    response = case_when(
      response %in% c(1, 2) ~ 1, 
      response == 3 ~ 2,
      response == 4 ~ 3,
      response == 5 ~ 4
    ),

    response = as.integer(response)
  )

saveRDS(data_2, "data/processed/data_2.rds")