# District population projections for Perú
# Paloma Cárcamo

library(tidyverse)
library(readxl)

########################
# Processing district-level population for 2005-2015

df_list <- list()

for (file_num in 1:11) {
  file_path <- sprintf("data/raw/population/d01%03d.xls", file_num)
  
  d_raw <- read_xls(file_path)
  
  d_processed <- d_raw |> 
    select(2, 4) |> 
    rename("ubigeo" = 1, "population" = 2) |> 
    drop_na() |> 
    filter(!grepl("[^0-9]", ubigeo)) |> 
    mutate(year = 2005 + file_num - 1, .before = ubigeo)
  
  df_list[[file_num]] <- d_processed
}

pop_data <- bind_rows(df_list)

pop_data <- pop_data |> 
  mutate(
    level = case_when(
      ubigeo == "000000" ~ "country",
      substr(ubigeo, nchar(ubigeo) - 3, nchar(ubigeo)) == "0000" ~ "region",
      substr(ubigeo, nchar(ubigeo) - 1, nchar(ubigeo)) == "00" ~ "province",
      TRUE ~ "district"
    )
  )

write_csv(pop_data, "data/interim/district_population_2005-2015.csv")

########################
# processing district-level population for 1990-1995

d_raw <- read_xlsx("data/raw/population/1990_1995.xlsx")

## create ubigeos
d1 <- d_raw |> 
  mutate(across(1:5, ~ ifelse(!is.na(.), ifelse(nchar(.) == 1, as.character(paste0("0", .)), as.character(.)), .))) |> 
  rowwise() |> 
  mutate(ubigeo = paste0(na.omit(c_across(1:5)), collapse = ""), .before = 1, .keep = "unused") |> 
  ungroup()

## format populations by year
d2 <- d1 |> 
  rowwise() |> 
  mutate(
    `1990` = paste0(na.omit(c_across(3:8)), collapse = ""),
    `1991` = paste0(na.omit(c_across(9:13)), collapse = ""),
    `1992` = paste0(na.omit(c_across(14:18)), collapse = ""),
    `1993` = paste0(na.omit(c_across(19:22)), collapse = ""),
    `1994` = paste0(na.omit(c_across(23:25)), collapse = ""),
    `1995` = paste0(na.omit(c_across(26:28)), collapse = ""),
    .after = ubigeo, .keep = "unused"
  ) |> 
  ungroup() |> 
  select(ubigeo, `1990`, `1991`, `1992`, `1993`, `1994`, `1995`) |> 
  filter(ubigeo != "")

## pivot longer

d3 <- d2 |> 
  pivot_longer(
    cols = 2:7,
    names_to = "year",
    values_to = "population"
  )

## add levels
d3 <- d3 |> 
  mutate(
    level = case_when(
      ubigeo == "000000" ~ "country",
      substr(ubigeo, nchar(ubigeo) - 3, nchar(ubigeo)) == "0000" ~ "region",
      substr(ubigeo, nchar(ubigeo) - 1, nchar(ubigeo)) == "00" ~ "province",
      TRUE ~ "district"
    )
  )

write_csv(d3, "data/interim/district_population_1990-1995.csv")

########################
# processing district-level population for 1996-2000

d_raw <- read_xlsx("data/raw/population/1996-2000.xlsx")

## create ubigeos
d1 <- d_raw |> 
  mutate(across(1:5, ~ ifelse(!is.na(.), ifelse(nchar(.) == 1, as.character(paste0("0", .)), as.character(.)), .))) |> 
  rowwise() |> 
  mutate(ubigeo = paste0(na.omit(c_across(1:5)), collapse = ""), .before = 1, .keep = "unused") |> 
  ungroup()

## format populations by year
d2 <- d1 |> 
  rowwise() |> 
  mutate(
    `1996` = paste0(na.omit(c_across(3:11)), collapse = ""),
    `1997` = paste0(na.omit(c_across(12:16)), collapse = ""),
    `1998` = paste0(na.omit(c_across(17:21)), collapse = ""),
    `1999` = paste0(na.omit(c_across(22:25)), collapse = ""),
    `2000` = paste0(na.omit(c_across(26:27)), collapse = ""),
    .after = ubigeo, .keep = "unused"
  ) |> 
  ungroup() |> 
  select(-c("...6")) |> 
  filter(ubigeo != "")

## pivot longer

d3 <- d2 |> 
  pivot_longer(
    cols = 2:6,
    names_to = "year",
    values_to = "population"
  )

## add levels
d3 <- d3 |> 
  mutate(
    level = case_when(
      ubigeo == "000000" ~ "country",
      substr(ubigeo, nchar(ubigeo) - 3, nchar(ubigeo)) == "0000" ~ "region",
      substr(ubigeo, nchar(ubigeo) - 1, nchar(ubigeo)) == "00" ~ "province",
      TRUE ~ "district"
    )
  )

write_csv(d3, "data/interim/district_population_1996-2000.csv")

########################
# processing district-level population for 2001-2005

d_raw <- read_xlsx("data/raw/population/2001-2005.xlsx")

## create ubigeos
d1 <- d_raw |> 
  mutate(across(1:5, ~ ifelse(!is.na(.), ifelse(nchar(.) == 1, as.character(paste0("0", .)), as.character(.)), .))) |> 
  rowwise() |> 
  mutate(ubigeo = paste0(na.omit(c_across(1:5)), collapse = ""), .before = 1, .keep = "unused") |> 
  ungroup()

## format populations by year
d2 <- d1 |> 
  rowwise() |> 
  mutate(
    `2001` = paste0(na.omit(c_across(4:9)), collapse = ""),
    `2002` = paste0(na.omit(c_across(10:15)), collapse = ""),
    `2003` = paste0(na.omit(c_across(16:21)), collapse = ""),
    `2004` = paste0(na.omit(c_across(22:25)), collapse = ""),
    `2005` = paste0(na.omit(c_across(26:27)), collapse = ""),
    .after = ubigeo, .keep = "unused"
  ) |> 
  ungroup() |> 
  select(-c("...6", "...7")) |> 
  filter(ubigeo != "")

## pivot longer

d3 <- d2 |> 
  pivot_longer(
    cols = 2:6,
    names_to = "year",
    values_to = "population"
  )

## add levels
d3 <- d3 |> 
  mutate(
    level = case_when(
      ubigeo == "000000" ~ "country",
      substr(ubigeo, nchar(ubigeo) - 3, nchar(ubigeo)) == "0000" ~ "region",
      substr(ubigeo, nchar(ubigeo) - 1, nchar(ubigeo)) == "00" ~ "province",
      TRUE ~ "district"
    )
  )

write_csv(d3, "data/interim/district_population_2001-2005.csv")

########################
# join all dbs

db1 <- read_csv("data/interim/district_population_1990-1995.csv")
db2 <- read_csv("data/interim/district_population_1996-2000.csv")
db3 <- read_csv("data/interim/district_population_2001-2005.csv")
db4 <- read_csv("data/interim/district_population_2005-2015.csv")
db5 <- read_csv("data/interim/population.csv")

db1 <- db1 |> 
  filter(level == "district") |> 
  select(-level)

db2 <- db2 |> 
  filter(level == "district") |> 
  select(-level)

db3 <- db3 |> 
  filter(level == "district", year <2005) |> 
  select(-level)

db4 <- db4 |> 
  filter(level == "district") |> 
  select(-level)

db5 <- db5 |> 
  group_by(ubigeo) |> 
  tidyr::complete(year = 2016:2023) |> 
  ungroup()

db5a <- db5 |> 
  group_by(ubigeo) |> 
  mutate(population = round(exp(zoo::na.spline(log(population))), digits = 0)) |> 
  ungroup()

pop <- rbind(db1, db2, db3, db4, db5a)

write_csv(pop,"data/processed/district_pop_1990-2022.csv")
write_rds(pop, "data/processed/district_pop_1990-2022.rds")
