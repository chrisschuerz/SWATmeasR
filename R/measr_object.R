#' `measr_project` is an `R6Class` to include NSWRMs in a SWAT+ model setup.
#'
#' @description
#' A `measr_project` is an `R` object which stores all data (SWAT+ model input files,
#' NSWRM definitions, potential measure locations) and the corresponding functions
#' to implement NSWRMs in the text input files of a SWAT+ project.
#'
#' @usage new_measr(project_name, project_path)
#'
#' @rdname measr_project
#'
#' @import R6
#'
#' @export
measr_project <- R6::R6Class(
  "measr_project",
  cloneable = FALSE,
  lock_objects = FALSE,
  public = list(

    #' @field .data List which stores the all input data that is required to
    #'   implement NSWRMs in the SWAT+ project which is located in
    #'   `project_path`. `.data` has the following elements:
    #'   - `.data$model_setup`: stores all SWAT+ input files that were loaded
    #'   from the `project_path`. Two versions of the input files are saved in
    #'   `.data$model_setup`
    #'     * `.data$model_setup$original_inputs` are the unchanged input files
    #'       which are required to reset to an initial condition.
    #'     * `.data$model_setup$modified_inputs` are the input files which are
    #'       modified when NSWRMs are implemented.
    .data = list(),

    #' @description Create a new SWATmeasR project. To start a new
    #'   `meas_project` use the function `new_measr(project_name, project_path)`
    #'   instead of the class function `measr_project$new()`.
    #'
    #' @param project_name Name of the `measr_project`. This name is assigned to
    #'   the object in the R environment and is also the name of the saved
    #'   object in the SWAT+ project folder.
    #' @param project_path Path to the SWAT project folder on the hard drive
    #'   (i.e. txtinout folder).
    #'
    #' @returns Saves the project as a single data file with the name
    #'   '<project_name>.measr'
    initialize = function(project_name, project_path) {
      if(file.exists(paste0(project_path, '/', project_name,".measr"))){
        stop("measR project allready exists in", project_path)
      }

      self$.data$meta$project_name <- project_name
      self$.data$meta$project_path <- project_path
      self$.data$model_setup$original_inputs <- read_swat_inputs(project_path)
      self$.data$model_setup$modified_inputs <- self$.data$model_setup$original_inputs

    },

    #' @description
    #' Save the SWATmeasR project in the SWAT+ project folder.
    #'
    save = function(){
      obj_save <- get(x = self$.data$meta$project_name,
                      envir = sys.frame(-1))
      saveRDS(object = obj_save,
              file = paste0(self$.data$meta$project_path, '/',
                            self$.data$meta$project_name, '.measr'))
    }
  )
)
