# Load packages
pacman::p_load(tidyverse, ggpubr, # for data wrangling and visulization
               readxl, # reading excel files
               scales, # for relative scales in plots
               purrr, # for functional programming
               viridis, # for color palettes
               drc, minpack.lm) # for 4-param logistic regression

#* Processing Functions

#' Align Microplate Readings and Plate Layout
#'
#' Creates dataframe matching OD values for each well with the
#' SampleIDs from the layout.
#'
#' @param plate.file excel file containing OD values from plate reader.
#' @param layout.file excel file containing plate layout.
#'
#' @return Dataframe listing SampleIDs with corresponding OD values.
#'
#' @examples
#' align.data(plate.file = c("Path" = "OD.xlsx", "Sheet" = "A"),
#'            layout.file = c("Path = "layout.xlsx", "Sheet" = "A"))
#'
#' @export
align.data <- function(plate.file = NULL, layout.file = NULL) {
    # Load input files
    plate <- read_excel(path = plate.file["Path"],
                        sheet = plate.file["Sheet"],
                        col_types = c("text", rep("numeric", 12))) %>%
                suppressWarnings() %>%
                suppressMessages() %>%
                setNames(c("row", 1:12))
    layout <- read_excel(path = layout.file["Path"],
                         sheet = layout.file["Sheet"]) %>%
                suppressWarnings() %>%
                suppressMessages() %>%
                setNames(c("row", 1:12))

    # Convert layout & plate to long format and merge them
    plate <- plate %>%
                pivot_longer(-row, names_to = "col", values_to = "value")
    layout <- layout %>%
                pivot_longer(-row, names_to = "col", values_to = "sample")
    df <- full_join(x = layout, y = plate, by = c("row", "col")) %>%
            mutate(col = as.numeric(col)) %>%
            arrange(col, row)
    return(df)
}

#' Generate Sample Lists
#'
#' Genrate lists for standards and unknown samples.
#'
#' @param df dataframe containing SampleIDs and corresponding OD values
#'
#' @return list containing dataframes for standards and unknown samples.
#'
#' @examples
#' sample.list(df)
#'
#' @export
sample.list <- function(df) {
    # Extract standard concentrations and clean up table
    samples <- list()
    samples$stds <- df %>%
                      filter(grepl("[Ss]tandard", sample)) %>%
                      mutate(Concentration = str_extract(sample, "(?<=\\[).+?(?=\\])") %>%
                                                str_extract(., ".*[^\ ng/uL]") %>%
                                                as.numeric(),
                             Standard = str_extract(sample, "[Ss]tandard \\d"),
                             OD = value) %>%
                      select(Standard, Concentration, OD) %>%
                      drop_na()
    # Extract unknown samples and clean up table
    samples$unknowns <- df %>%
                          filter(!grepl("Standard", sample)) %>%
                          select(-c(row, col)) %>%
                          setNames(c("Sample", "OD"))
    return(samples)
}

#' Generate standard curve
#'
#' Visualize standard curves for predicting unknown concentrations.
#'
#' @param stds dataframe containg standard concentrations and OD values
#'
#' @return figure containing standard curve
#'
#' @export
standard.curve <- function(stds) {
    p <- ggplot(stds,
                aes(x = OD, y = Concentration, color = Standard)) +
            geom_point(size = 5, alpha = .6) +
            geom_smooth(method = "lm", size = .9, color = "#bb0303") +
            stat_regline_equation(size = 6.5, color = "#bb0303") +
            ggtitle("Standard Curve") +
            theme_minimal() +
            theme(
                text = element_text(family = "Palantino"),
                plot.title = element_text(size = 23, hjust = 0.5),
                axis.title = element_text(size = 20),
                axis.text = element_text(size = 13),
                plot.background = element_rect(fill = "#e2e2e2"),
                panel.background = element_rect(fill = "#e2e2e2", size = 0),
                panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                                colour = "#b4b2b2"),
                panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                                colour = "#c5c5c5")
            )
    return(p)
}

#' Predict Unknown Concentrations
#'
#' Fit standards to linear model and use this to predict
#' unknown concentrations.
#'
#' @param stds standard concentrations and their OD values
#' @param unknowns dataframe containing OD values of unknown samples
#'
#' @return dataframe containing predictions
#' @export
predict.concentrations <- function(stds, unknowns) {
    # Fit data to linear model
    fit <- lm(Concentration ~ OD, stds)

    # Predict Concentrations
    unknowns$Concentration <- predict(fit, unknowns)
    return(unknowns)
}

#* Dashboard functions

#' Calibration Curve
#'
#' Plot Standard Calibration Curve based on calculated
#' concentrations and OD values
#'
#' @param standards dataframe containing standard concentrations and OD values
#'
#' @return figure containing calibration curve
#'
#' @export
calibration <- function(standards) {
    p <- ggplot(data = standards, aes(concentration, OD)) +
            geom_point() +
            geom_smooth(method = "loess") +
            ggtitle("Standard Calibration Curve") +
            theme_minimal()
    return(p)
}