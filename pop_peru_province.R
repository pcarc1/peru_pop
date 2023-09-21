# Province population projections for Perú
# Paloma Cárcamo

library(tidyverse)

data_raw <- read_rds("data/processed/district_pop_1990-2022.rds")

data <- data_raw |> 
  mutate(province = substr(ubigeo, 1, 4),
         province = case_when(
         ubigeo == "160204" ~ "1607",
         ubigeo == "160203" ~ "1607",
         ubigeo == "160207" ~ "1607",
         ubigeo == "160208" ~ "1607",
         ubigeo == "160209" ~ "1607",
         ubigeo == "160109" ~ "1608",
         ubigeo == "160114" ~ "1608",
         .default = province
       ))

pop_prov <- data |> 
  group_by(year, province) |> 
  summarise(population = sum(population))

write_csv(pop_prov, "data/processed/population_province_long.csv")

pop_prov_wide <- pop_prov |> 
  pivot_wider(names_from = year, values_from = population)

write_csv(pop_prov, "data/processed/population_province_wide.csv")
