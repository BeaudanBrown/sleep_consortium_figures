library(targets)
library(tarchetypes)
library(crew)
library(data.table)

dotenv::load_dot_env()
data_dir <- Sys.getenv("DATA_DIR2")

# Ensure single threaded within targets
Sys.setenv(R_DATATABLE_NUM_THREADS = 1)
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(MKL_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)

# Set target options:
tar_option_set(
  packages = c(
    "data.table",
    "readr",
    "ggplot2",
    "forestplot",
    "readxl",
    "patchwork",
    "grid",
    "meta"
    # )
  ),
  controller = crew_controller_local(
    workers = future::availableCores() - 1
  )
)

dir.create("results/pngs", recursive = TRUE, showWarnings = FALSE)
dir.create("results/csvs", recursive = TRUE, showWarnings = FALSE)
dir.create("results/pdfs", recursive = TRUE, showWarnings = FALSE)

# Run the R scripts in the R/ folder
tar_source()

dependents <- data.table(
  name = c(
    "attention",
    "executive",
    "cognition",
    "verbal",
    "language",
    "visuospatial"
  ),
  label = c(
    "Attention & Processing Speed",
    "Executive Function",
    "Global Cognition",
    "Verbal Learning and Memory",
    "Language",
    "Visuospatial Function"
  ),
  dependent = c("APS", "EXF", "PC1", "VLM", "LAN", "VSF")
)

source("pipeline_targets.R")

## pipeline
list(
  tar_file(
    cog_file,
    file.path(data_dir, "All_Cognition_Analyses.xlsx")
  ),
  tar_file(
    dem_file,
    file.path(data_dir, "All_IncidentDementia_Analyses.xlsx")
  ),
  interaction_targets,
  assoc_targets
)
