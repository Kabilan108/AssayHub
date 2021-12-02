# Load packages
require(tidyverse)

#' Load Layout & OD Values from Excel file
#'
#' Read OD values and layout from excel file
#' and combine them in data frame appropriately
#'
#' @param file excel file containing layout and OD values
#' @param sheet.OD sheet containing OD values
#' @param sheet.Layout sheet containing plate Layout
#'
#' @return dataframe containing sample list with OD values
#' @export
load.plate <- function(file = NULL, sheet.OD = "OD", sheet.Layout = "Layout") {
  full_join(
    x = readxl::read_excel(path = file, sheet = sheet.Layout) %>%
          pivot_longer(-1) %>%
          setNames(c("Row", "Column", "Sample")) %>%
          mutate(Well = str_c(Row, Column)) %>%
          select(Well, Sample),
    y = readxl::read_excel(path = file, sheet = sheet.OD) %>%
          pivot_longer(-1) %>%
          setNames(c("Row", "Column", "OD")) %>%
          mutate(Well = str_c(Row, Column)) %>%
          select(Well, OD),
    by = "Well"
  ) %>%
  suppressMessages() %>%
  return()
}