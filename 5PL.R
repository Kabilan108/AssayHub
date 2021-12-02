# Load packages
require(minpack.lm)
require(tidyverse)
require(patchwork)
require(ggpubr)
import::from(modelr, rsquare, mse)

#' Determine Hill's slope
#'
#' Hill's slope (B) refers to the steepness of the curve.
#' It takes a value of ±1.
#' This estimates the Hill's slope of a 5PL curve based on the
#' sign of the slope between the extreme points.
#'
#' @param x predictor variable values
#' @param y response variable values
#'
#' @return Hill's slope estimate
hills <- function(x, y) {
  return(
    sign((y[length(y)] - y[1]) / (x[length(x)] - x[1]))
    )
}


#' Estimate Inflection Point
#'
#' The inflection point (C) is the point at which the curve changes
#' signs or directions.
#' It is the standard concentration where y = (D - A)/2.
#' This estimates the x-coordinate of the inflection point of a 5PL curve (the
#' standard concentration whose response is closest to the mid response)
#'
#' @param x predictor variable values
#' @param y response variable values
#'
#' @return x-coord of Inflection point estimate
inflection <- function(x, y) {
  return(
    x[which.min(abs(y - (max(y) - min(y)) / 2))]
  )
}


#' Generate nls fit for 5PL Curve
#'
#' Fits the data provided to a 5-Parameter Logistic Curve.
#' The fit is generated via non-linear least squares using a modified
#' version of the Levenberg-Marquardt algorithm.
#'
#' @param data dataframe containing Standard Concentrations and OD values
#' @param predictor predictor variable (typically standard concentration)
#' @param response response variable (typically the OD values)
#'
#' @return nls fit object
#'
#' @example
#' fit_5PL(data, Conc, OD)
#'
#' @export
fit.5PL <- function(standards, predictor, response) {
  # Define prefictor and response variables
  Conc <- (select(standards, {{ predictor }}))[[1]]
  OD <- (select(standards, {{ response }}))[[1]]

  # Define starting estimates for curve parameters
  #* A: Minimum Asymptote
  #   > The response (OD) value at 0 standard concentration.
  #   > Estimated as (background corrected) min{ response (OD) }
  #* B: Hill's Slope
  #* C: Inflection Point
  #* D: Maximum Asymptote
  #   > The response (OD) value at infinite standard concentration.
  #   > Estimated as (background corrected) max{ response (OD) }
  #* E: Asymmetry Factor
  #   > Curve initially assumed to be symmetrical {E = 1}
  start <- c("A" = min(OD),
             "B" = hills(Conc, OD),
             "C" = inflection(Conc, OD),
             "D" = max(OD),
             "E" = 1)

  # Fit curve
  fit <- nlsLM(
    formula = OD ~ D + (A - D) / (1 + (Conc / C)^B)^E,
    start = start,
    control = nls.control(maxiter = 1000)
  )
  return(fit)
}


#' Generate Table with Parameter Estimates
#'
#' Creates a tableGrob containing the parameter estimates
#'
#' @param fit nls fit object
#'
#' @return figure containing table with parameters
param.tbl <- function(fit, standards) {
  broom:: tidy(fit) %>%
    select(term, estimate) %>%
    add_row(tibble_row(term = "R^2", estimate = rsquare(fit, standards))) %>%
    mutate(estimate = format(estimate, digits = 2)) %>%
    add_row(
      tibble_row(term = "MSE", estimate = mse(fit, standards)) %>%
        mutate(estimate = format(estimate, digits = 3, scientific = TRUE))
    ) %>%
    gridExtra::tableGrob(
      rows = NULL,
      cols = NULL,
      theme = gridExtra::ttheme_default(core = list(
        fg_params = list(parse = TRUE, col = "#586e75"),
        bg_params = list(fill = rep(c("#fdf6e3", "#eee8d5"), 4)[1:7],
                        col = "#586e75")
      ))
    ) %>%
    as_ggplot() %>%
    return()
}


#' Generate Figure with 5PL Equation
#'
#' Creates figure with annotation showing 5PL equation
eqn.5PL <- function() {
  ggplot() +
    annotate(
      geom = "text", x = 0, y = 0,
      label = "italic(y == D + frac(A-D, (1 + ( frac(x,C) )^B)^E))",
      parse = TRUE, size = 5, color = "#586e75", family = "Helvetica"
    ) +
    theme_void() +
    theme(plot.background = element_rect(size = NA, fill = "#fdf6e3")) %>%
  return()
}


#' Generate 5PL Curve Figure
#'
#' Creates a figure containing the 5PL Curve & parameter estimates.
#'
#' @param data dataframe containing Standard Concentrations and OD values
#' @param predictor predictor variable (typically standard concentration)
#' @param response response variable (typically the OD values)
#' @param s.dil.fac dilution factor for standards
#'
#' @return figure containing 5PL curve
#'
#' @example
#' FPL(data, Conc, OD)
#'
#' @export
plot.5PL <- function(standards, predictor, response, fit, s.dil.fac = 2) {
  # Define starting estimates for parameters
  start <- fit$m$getPars()

  # Create table with parameter estimates
  tbl <- param.tbl(fit, standards)

  # Create figure with 5PL equation
  eqn <- eqn.5PL()

  # Create 5PL Curve
  fpl <- standards %>%
    ggplot(aes({{ predictor }}, {{ response }})) +
      geom_smooth(
        aes(color = "5PL"), method = "nlsLM",
        formula = y ~ D + (A - D) / (1 + (x / C)^B)^E,
        method.args = list(start = start,
                            control = nls.control(maxiter = 100)),
        fullrange = TRUE, size = 1.5, se = FALSE
      ) +
      geom_point(aes(color = "Standards"), size = 3.5, alpha = .6) +
      labs(x = "Concentration", y = "Measurment (OD)") +
      scale_color_manual(
        values = c("#F8766D", "black"),
        guide = guide_legend(
          override.aes = list(linetype = c("solid", "blank"),
                              shape = c(NA, 19))
        )
      ) +
      ggthemes::theme_solarized_2() +
      theme(
        text = element_text(family = "Helvetica", color = "#586e75"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.position = "top",
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 12)
      )

  # Combine figure elements
  (fpl | (eqn / tbl)) +
    plot_layout(nrow = 1, widths = c(2, .9)) &
    plot_annotation(title = "ELISA Standard Calibration Curve") &
    theme(
      plot.background = element_rect(size = NA, fill = "#fdf6e3"),
      plot.title = element_text(hjust = 0.5, vjust = 0.1, family = "Helvetica",
                                size = 17, color = "#586e75")
    )
}


#' Predict Concentrations
#'
#' This function predicts the concentrations of unknown samples
#' based on a 5PL fit.
#'
#' @param fit 5PL nls fit object
#' @param unknowns dataframe containing OD values for unknown samples
#' @param u.dil.fac dilution factor for unknown samples
#'
#' @return dataframe with predicted concentrations
#'
#' @export
predict.5PL <- function(fit, standards, unknowns, u.dil.fac = 1) {
  # Get OD values
  OD <- unknowns$OD

  # Predict concentrations
  temp <- with(as.list(fit$m$getPars()), {
    C * (((A - D) / (OD - D))^(1 / E) - 1) ^ (1 / B)
    # TODO: Implement BDL & ADL
    # temp[temp > max(standards$Conc), ] == "ADL"
    # temp[temp < min(standards$Conc), ] == "BDL"
  })
  unknowns["Conc (Diluted)"] = temp
  unknowns["Conc (Undiluted)"] = temp * u.dil.fac
  return(unknowns)
}


#' Perform background correction
#'
#' Perform background correction on samples by subtracting the
#' mean OD value of BLANKs from all other values
#'
#' @param data dataframe containing OD values of samples
#'
#' @returnm background corrected dataframe
#'
#' @export
bkgdCorrect <- function(data) {
  # Get mean background OD
  bkgd <- filter(data, grepl("BLANK", Sample)) %>%
            group_by(Sample) %>%
            summarize(OD = mean(OD)) %>%
            .[[2]]
  # Perform background correction
  data %>%
    mutate(OD = OD - bkgd) %>%
    filter(!grepl("BLANK", Sample))
}


#' Standards & unknowns
#'
#' Split data frame into standards and unknowns and
#' add standard concentrations to standard dataframe
#'
#' @param data dataframe containing OD values
#' @param stat.conc starting concentration for standards
#' @param s.dil.fac dilution factor for standards
#'
#' @retrun list containing separate dataframes for standards & unknowns
#'
#' @export
sample.list <- function(data, start.conc, s.dil.fac) {
  # Get number of standards used
  n <- unique(filter(data, grepl("^S", Sample))$Sample) |> length()

  # Create vector with standard concentrations
  stds <- (start.conc * s.dil.fac ^ -seq(0, n - 1)) |>
            setNames(str_c("S", seq(1, n)))

  # Create standards table averaged by standard
  S <- filter(data, grepl("^S", Sample)) %>%
        mutate(Conc = recode(Sample, !!! stds)) %>%
        group_by(Sample) %>%
        summarise(Well = reduce(Well, str_c, sep = " & "),
                  OD = mean(OD), Conc = mean(Conc))
  # Create table for unknown samples
  U <- filter(data, !grepl("^S", Sample))
  # Combine tables into list
  return(list(Standards = S, Unknowns = U))
}