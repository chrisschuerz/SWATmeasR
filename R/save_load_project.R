#' Initiate a new SWATmeasR project
#'
#' @param project_name the name of the new `measr_project`.
#' @param project_path Path to the SWAT TxtInOut folder to which the farmr
#'   should be applied.
#'
#' @returns Generates a new measr_project in the working environment (as an R6
#'   object) and saves the project the SWAT+ project folder.
#'
#' @export
#'
new_measr <- function(project_name, project_path) {
  measr_obj <- measr_project$new(project_name = project_name,
                                 project_path = project_path)
  cat('Input files read from the SWAT+ project located in\n',
      project_path, '\n')

  assign(project_name, measr_obj, envir = sys.frame(-1))
  measr_obj$save()
  cat("SWATmeasR object saved as", paste0("'", project_name, ".measr'"),
      'in this path.')
}

#' Load an existing SWATmeasR project
#'
#' @param file File path of a SWATmeasR project located in a SWAT+ project
#'   folder.
#'
#' @returns Loads an existing SWATmeasR project (as an R6 object) into the R
#'   working environment and saves the project the SWAT+ project folder.
#'
#' @export
#'
load_measr <- function(file) {
  project_path <- dirname(file)
  project_name <- strsplit(basename(file), "\\.")[[1]][1]
  file_ext     <- strsplit(basename(file), "\\.")[[1]][2]

  if(tolower(file_ext) != "measr") stop("measr object must be a *.measr file!")

  measr_obj <- readRDS(file)
  measr_obj$.data$meta$project_path <- project_path
  measr_obj$.data$meta$project_name <- project_name

  assign(project_name, measr_obj, envir = sys.frame(-1))
  measr_obj$save()
}
