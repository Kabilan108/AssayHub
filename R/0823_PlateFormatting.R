# Import libraries
library(tidyverse)
library(ggpubr)
library(readxl)

PICOBS <- tibble(Patient = 1:106)

#!-- Plate #408
full_join(
    x = read_excel(path = "Plate-Readings/Plate #408.xlsx",
                     sheet = "Layout") %>%
            pivot_longer(-1) %>%
            setNames(c("row", "col", "Sample")) %>%
            mutate(Location = str_c(row, col)) %>%
            select(Location, Sample) %>%
            drop_na(Sample),
    y = read_excel(path = "Plate-Readings/Plate #408.xlsx",
                     sheet = "Results") %>%
            mutate(Location = str_extract(Location, "(?<=\\(\\d,).+(?=\\))")) %>%
            filter(grepl("Unknown", Sample)) %>%
            select(1, 3:7),
    by = "Location"
) %>%
    add_column(Patient = str_extract(.$Sample, ".+(?=[tT]\\d)") |> as.numeric(),
               Time = str_extract(.$Sample, "[tT]\\d") |> toupper(),
               .after = "Sample") %>%
    pivot_longer(-c(1:4)) %>%
    select(-c(Location, Sample)) %>%
    split(., .$name) %>%
    map_df(., function(df) {
        df %>%
          arrange(Patient, Time) %>%
          pivot_wider(names_from = Time, values_from = value,
                      values_fn = toString) %>%
          select(Patient, name, T1, T2, T3, T4) %>%
          full_join(PICOBS, . ,by = "Patient")
    }) %>%
    write.csv("Processed/Plate #408.csv", row.names = FALSE, na = "")

# Plate #399
full_join(
    x = read_excel(path = "Plate-Readings/Plate #399.xlsx",
                     sheet = "Layout") %>%
            pivot_longer(-1) %>%
            setNames(c("row", "col", "Sample")) %>%
            mutate(Location = str_c(row, col)) %>%
            select(Location, Sample) %>%
            drop_na(Sample),
    y = read_excel(path = "Plate-Readings/Plate #399.xlsx",
                     sheet = "Results") %>%
            mutate(Location = str_extract(Location, "(?<=\\(\\d,).+(?=\\))")) %>%
            filter(grepl("Unknown", Sample)) %>%
            select(1, 3:7),
    by = "Location"
) %>%
    add_column(Patient = str_extract(.$Sample, ".+(?=[tT]\\d)") |> as.numeric(),
               Time = str_extract(.$Sample, "[tT]\\d") |> toupper(),
               .after = "Sample") %>%
    pivot_longer(-c(1:4)) %>%
    select(-c(Location, Sample)) %>%
    split(., .$name) %>%
    map_df(., function(df) {
        df %>%
          arrange(Patient, Time) %>%
          pivot_wider(names_from = Time, values_from = value,
                      values_fn = toString) %>%
          select(Patient, name, T1, T2, T3, T4) %>%
          full_join(PICOBS, . ,by = "Patient")
    }) %>%
    write.csv("Processed/Plate #399.csv", row.names = FALSE, na = "")