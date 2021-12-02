# Import libraries
pkgs <- list("tidyverse", "readxl")
pkgs <- lapply(pkgs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (!is_empty(args)) {
  in_dir <- args[1]
  out_dir <- args[2]
} else {
  # Get Input Directory
  in_dir <- readline("Where are the Input files? ")
  while (!dir.exists(in_dir)) {
    cat(bgRed("                ERROR !                 \n Please Provide a Valid Input Directory \n"))
    in_dir <- readline("Where are the Input files? ")
  }
  # Get output directory
  out_dir <- readline("Where would you like the processed files? ")
  while (!dir.exists(in_dir)) {
    cat(bgRed("                ERROR !                 \n Please Provide a Valid Input Directory \n"))
    in_dir <- readline("Where are the Input files? ")
  }
}

# Print logo
cat(readLines("~/KFL/Workflow/ASCII/logo.txt"), sep = "\n")

# Get user input
# ftype <- menu(title = "Please Select File Type:",
#               choices = c("Single Protein Elisa", "Multiplex Elisa"))
ftype <- 1

# Quit if no type specified
suppressMessages(
if (ftype == 0) {
  cat(readLines("~/KFL/Workflow/ASCII/fu_bye.txt"), sep = "\n")
} else {
  # Start Processing files
  cat(readLines("~/KFL/Workflow/ASCII/chill.txt"), sep = "\n")

  # Loop Through Files in Input Directory
  for (file in dir(in_dir)[!grepl("~", dir(in_dir))]) {
    # Create i/o files
    in_file <- file.path(in_dir, file)
    out_file <- file.path(out_dir, str_replace(file, ".xlsx", ".csv"))

    # Create patient # list
    patients <- tibble(Patient = if (grepl("GT#", file)) 1:106 else 1:210)

    # Get dilution factor
    DF <- readxl::read_excel(path = in_file, sheet = "Analysis",
                            range = "G37:G37", col_names = FALSE)[[1]]

    # Process file based on file type
    if (ftype == 1) {
      inner_join(
        x = read_excel(path = in_file, sheet = "Layout", skip = 1) %>%
              pivot_longer(-1) %>%
              setNames(c("row", "col", "Sample")) %>%
              mutate(Well = toupper(str_c(row, col)),
                    Sample = str_replace_all(Sample, "\ ", "")) %>%
              select(Well, Sample) %>%
              drop_na(),
        y = read_excel(path = in_file, sheet = "Analysis", skip = 37) %>%
              select(2, 3, 7) %>%
              setNames(c("Sample", "Well", "Conc")) %>%
              filter(grepl("Unknown", Sample)) %>%
              mutate(Conc = as.character(DF * as.numeric(str_replace(Conc, "-", "0")))) %>%
              select(-Sample) %>%
              drop_na(),
        by = "Well"
      ) %>%
        mutate(
          Patient = str_extract(Sample,
                      "[^([Pt][Tt]#?)].*(?=[Tt]\\d)") |> as.numeric(),
          Time = str_extract(Sample, "[Tt]\\d$") |> toupper()
        ) %>%
        select(-c(Well, Sample)) %>%
        pivot_wider(names_from = "Time", values_from = "Conc",
                    values_fn = toString) %>%
        full_join(patients, ., by = "Patient") %>%
        select(Patient, T1, T2, T3, T4) %>%
        write_csv(out_file, na = "")
    } else if (ftype == 2) {
      full_join(
        x = read_excel(path = in_file, sheet = "Layout") %>%
              pivot_longer(-1) %>%
              setNames(c("row", "col", "Sample")) %>%
              mutate(Location = str_c(row, col)) %>%
              select(Location, Sample) %>%
              drop_na(Sample),
        y = read_excel(path = in_file, sheet = "Analysis") %>%
              mutate(Location =
                      str_extract(Location, "(?<=\\(\\d,).+(?=\\))")) %>%
              filter(grepl("Unknown", Sample)) %>%
              select(-Sample, -`Total Events`),
        by = "Location"
      ) %>%
        add_column(
          Patient = str_extract(.$Sample,
                      "(?<=Pt).+(?=[tT]\\d)") |> as.numeric(),
          Time = str_extract(.$Sample, "[tT]\\d$") |> toupper(),
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
          full_join(patients, ., by = "Patient")
        }) %>%
        write.csv(out_file, row.names = FALSE, na = "")
    }
  }
  # Say Goodbye
  cat(readLines("~/KFL/Workflow/ASCII/bye.txt"), sep = "\n")
})
