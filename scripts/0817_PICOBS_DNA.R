# Load packages
pacman::p_load(tidyverse, ggpubr, # for data wrangling and visulization
               readxl) # for 4-param logistic regression

# Import functions from processing
import::from(Functions.R, .all = TRUE)

# Specify input files
plate.file <- list()
plate.file$T <- c("Path" = "Plate-Readings/0817_DNA_BroadRange_PICOBS.xlsx",
                  "Sheet" = "TOP")
plate.file$B <- c("Path" = "Plate-Readings/0817_DNA_BroadRange_PICOBS.xlsx",
                  "Sheet" = "BOTTOM")
layout.file <- c("Path" = "Plate-Layouts/0817_PICOBS_DNA.xlsx",
                 "Sheet" = "PICOBS")

# Top Reading
df <- align.data(plate.file$T, layout.file)
samples <- sample.list(df)
p <- standard.curve(samples$stds) + ggtitle("Standard Curve: Top")
concentrations.T <- predict.concentrations(samples$stds, samples$unknowns)

# Bottom Reading
df <- align.data(plate.file$B, layout.file)
samples <- sample.list(df)
q <- standard.curve(samples$stds) + ggtitle("Standard Curve: Bottom")
concentrations.B <- predict.concentrations(samples$stds, samples$unknowns)

# Combine results & Average concentrations
(p | q)
full_join(concentrations.T, concentrations.B,
          by = "Sample", suffix = c(".T", ".B")) %>%
    rowwise() %>%
    mutate(Concentration = mean(c(Concentration.T, Concentration.B))) %>%
    select(Sample, Concentration) %>%
    write.csv("Processed/0817_DNA_BroadRange_PICOBS.csv")