#' `measr_project` is an `R6Class` to include NSWRMs in a SWAT+ model setup.
#'
#' @description A `measr_project` is an `R` object which stores all data (SWAT+
#' model input files, NSWRM definitions, potential measure locations) and the
#' corresponding functions to implement NSWRMs in the text input files of a
#' SWAT+ project.
#'
#' @usage new_measr(project_name, project_path)
#'
#' @param project_name Name of the `measr_project`. This name is assigned to the
#'   object in the R environment and is also the name of the saved object in the
#'   SWAT+ project folder.
#' @param project_path Path to the SWAT project folder on the hard drive (i.e.
#'   txtinout folder).
#'
#' @returns Returns a `swat_measr` object in the `R` environment and saves the
#'   project as a single data file with the name '<project_name>.measr' in
#'   `project_path`.
#'
#' @import R6
#'
#' @docType class
#'
#' @aliases new
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
    initialize = function(project_name, project_path) {
      if(file.exists(paste0(project_path, '/', project_name,".measr"))){
        stop("measR project allready exists in", project_path)
      }

      self$.data$meta$project_name <- project_name
      self$.data$meta$project_path <- project_path
      self$.data$model_setup$original_inputs <- read_swat_inputs(project_path)
      self$.data$model_setup$modified_inputs <- self$.data$model_setup$original_inputs

    },

    #' @description Load the NSWRM location table from a '.csv' file into the
    #'   `measr_project`.
    #'
    #' @details The NSWRM locations are defined by the spatial objects which are
    #'   modified by a measure implementation. The measure locations are
    #'   provided with a .csv table. The table must have the following columns:
      #'   - `id`: The ID of the implemented measure. This can be integer row
      #'     indices. These ID values will be later on linked to a ID vector
      #'     which defines the measures that are implemented.
      #'   - `name`: Name of the implemented measure.
      #'   - `type`: In OPTAIN a set of NSWRMs is defined which can be
      #'     implemented in a model setup. `type` can be one of the folling
      #'     labels: 'buffer', 'grassfilter', 'hedge', 'grassslope',
      #'     'grassrchrg', 'afforest', 'pond', 'floodres', 'channres', 'swale',
      #'     'wetland', 'cdrain', 'terrace', 'notill', 'lowtill', 'lowtillcc',
      #'     'mulching', 'subsoiling', 'rotation', 'intercrop', 'covercrop',
      #'     'earlysow', 'droughtplt'.
      #'   - `obj_id`: The ID of the spatial object on which the measure is
      #'     implemented. For all `type`s except 'channres' the `obj_typ` is
      #'     a vector of HRU IDs. For 'channres' it is a vector of channel IDs
    #'
    #' @param file_path Path to the '.csv' definition file.
    #' @param overwrite Overwrite existing location table? Default is `FALSE`.
    #' If `TRUE` existing location table can be overwritten.
    #'
    load_nswrm_location = function(file_path, overwrite = FALSE) {
      self$.data$nswrm_definition <- load_nswrm_loc(file_path,
                                                    self$.data$nswrm_definition,
                                                    self$.data$model_setup$modified_inputs,
                                                    overwrite)
      self$save()
    },

    #' @description Load a definition table for an NSWRM type from a '.csv' file into the
    #' `measr_project`.
    #'
    #' @details
      #' The different `type` options represent different groups of NSWRMs. The
      #' different options group the following measures:
      #'
      #' - `'land_use'`: A land use measure definition table can include the
      #'   definitions for all NSWRMs which are represented by a land use
      #'   change. The lookup table can include the settings for the NSWRM codes
      #'   **buffer**, **edgefilter**, **hedge**, **grassslope**, **grassrchrg**
      #'   , and **afforest**. The `land_use` table must provide the columns
      #'   `type`, `lum_plnt`, `lum_mgt`, `lum_cn2`, `lum_cpr`, and
      #'   `lum_ovn`.
      #' - `'pond'`: A pond definition table includes all definitions for pond
      #'   locations. The `pond` table must provide the columns `name`,
      #'   `to_cha_id`, and `from_cha_id`.
    #'
    #' @param file_path Path to the '.csv' definition file.
    #' @param type Type of the NSWRM which is defined by this input file. The
    #'   type must be one of the options `'land_use'`, `'pond'`.
    #' @param overwrite Overwrite existing definition table? Default is `FALSE`.
    #' If `TRUE` existing definition table can be overwritten.
    #'
    load_nswrm_definition = function(file_path, type, overwrite = FALSE) {
      self$.data$nswrm_definition <- load_nswrm_def(file_path, type,
                                                    self$.data$nswrm_definition,
                                                    self$.data$model_setup$modified_inputs,
                                                    overwrite)
      self$save()
    },

    #' @description
    #' Reload SWAT+ input files.
    #'
    #' @param reset Default is `FALSE`. If `TRUE` reset of already loaded input
    #'   files is forced also when the SWAT inputs were alredy modified.
    #'
    reload_swat_inputs = function(reset = FALSE){
      if(!reset & any(self$.data$model_setup$original_inputs$file_updated)) {
        stop('Cannot reload SWAT+ input files when measures \n',
             'were already implemented in the SWAT+ project.\n',
             "Set 'reset = TRUE' to force a reset of the loaded inputs.")
      }
      self$.data$model_setup$original_inputs <- read_swat_inputs(
        self$.data$meta$project_path)
      self$.data$model_setup$modified_inputs <-
        self$.data$model_setup$original_inputs
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
    },

    #' @description
    #' Reset the changes applied to the SWAT+ input files.
    #'
    reset = function(){
      self$.data$model_setup$modified_inputs <-
        self$.data$model_setup$original_inputs
      self$save()
    }
  )
)
