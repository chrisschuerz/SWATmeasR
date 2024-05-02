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
#' @export
measr_project <- R6Class(
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
        stop("measR project already exists in ", project_path)
      }

      self$.data$meta$project_name <- project_name
      self$.data$meta$project_path <- project_path
      self$data$meta$measr_version <- packageVersion('SWATmeasR')
      self$.data$model_setup$original_inputs <-
        read_swat_inputs(project_path)
      self$.data$model_setup$modified_inputs <-
        self$.data$model_setup$original_inputs

    },

    #' @description Load a definition table for an NSWRM type from a '.csv' file
    #' into the `measr_project`.
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
      #' - `'management'`: The management definition is an `*.rds` object which
      #'   must be prepared with the function
      #'   `prepare_management_scenario_inputs()`. The function uses the
      #'   `SWATfarmR` projects which are located in the `project_path` and
      #'   generates the file `'mgt_scenarios.rds'` which is the required input
      #'   to define management related NSWRMs.
      #' - `'pond'`: A pond definition table includes all definitions for pond
      #'   locations. The `pond` table must provide the columns `hru_id`,
      #'   `to_cha_id`, and `from_cha_id`.
      #'   Optionally, the hydrology.res parameters for a pond and the pointers
      #'   for `'rel'` (release decision table), `'sed'` for sediment.res
      #'   parametrisations, and `'nut'` for nutrients.res parametrisations can
      #'   be defined.
      #' - `'constr_wetland'`: A constructed wetland is implemented the same way
      #'   as a pond. Thus the same definitions like for the pond apply for a
      #'   constructed wetland. As a constructed wetland is considered to be part
      #'   of the channel/reservoir network, the definitions of `to_cha_id` and
      #'   `from_cha_id` are mandatory.
      #' - `'wetland'`: A wetland definition table includes all definitions for
      #'   wetland locations. The `wetland` table must provide the columns `hru_id`.
      #'   Optionally, water from a wetland can be directly routed into a channel
      #'   and overwrite the original routing of the wetland HRU. In that case
      #'   `to_cha_id` must be defined. The hydrology.wet parameters for a
      #'   wetland and the pointers for `'rel'` (release decision table),
      #'   `'sed'` for sediment.res parametrisations, and  `'nut'` for
      #'   nutrients.res parametrisations can be defined.
      #'
    #'
    #' @param file_path Path to the '.csv' or '.rds' definition file.
    #' @param type Type of the NSWRM which is defined by this input file. The
    #'   type must be one of the options `'land_use'`, `'management'`, `'pond'`,
    #'   `'constr_wetland'`, or `'wetland'`.
    #' @param overwrite Overwrite existing definition table? Default is `FALSE`.
    #' If `TRUE` existing definition table can be overwritten.
    #'
    load_nswrm_definition = function(file_path, type, overwrite = FALSE) {
      self$.data$nswrm_definition <- load_nswrm_def(
        file_path, type,
        self$.data$nswrm_definition,
        self$.data$model_setup$modified_inputs,
        overwrite
        )
      self$save()
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
      #'   - `nswrm`: In OPTAIN a set of NSWRMs is defined which can be
      #'     implemented in a model setup.
      #'   - `obj_id`: The ID of the spatial object on which the measure is
      #'     implemented. For all `nswrm`s except 'channres' the `obj_typ` is
      #'     a vector of HRU IDs. For 'channres' it is a vector of channel IDs
    #'
    #' @param file_path Path to the '.csv' definition file.
    #' @param overwrite Overwrite existing location table? Default is `FALSE`.
    #' If `TRUE` existing location table can be overwritten.
    #'
    load_nswrm_location = function(file_path, overwrite = FALSE) {
      self$.data$nswrm_definition <- load_nswrm_loc(
        file_path,
        self$.data$nswrm_definition,
        self$.data$model_setup$modified_inputs,
        overwrite
        )
      self$save()
    },

    #' @description Implement NSWRMs in the SWAT+ model input tables.
    #'
    #' @details
        #' All locations which are included in `nswrm_id` must be defined in the
        #' NSWRM location table
        #' (`measr_project$.data$nswrm_definition$nswrm_locations`). All NSWRM
        #' location IDs which are included in `nswrm_id` are implemented in
        #' the SWAT+ input tables. The measures are implemented in a specific
        #' order with a certain hirachy. Therefore the implementation of a
        #' measure can be overruled by the implementation of another measure
        #' which would be implemented by using the same spatial object. For
        #' example the implementation of a pond overrules a land use change
        #' to grassland that is performed to implement a grassed waterway.
    #'
    #' @param nswrm_id Numeric vector of NSWRM location IDs which are defined in
    #'   the NSWRM location table
    #'   (`measr_project$.data$nswrm_definition$nswrm_locations`) with the column
    #'   `id`.
    #' @param reset Reset existing NSWRM implementations? Default is `FALSE`. If
    #'   measures were already implemented the SWAT+ input tables must be reset,
    #'   before implementing a new set of measures. If `TRUE` the SWAT+ input
    #'   tables will be reset before implementing a new set of measures.
    #'
    implement_nswrm = function(nswrm_id, reset = FALSE) {
      self$.data$model_setup$modified_inputs <- implement_nswrm(
        nswrm_id,
        self$.data$nswrm_definition,
        self$.data$model_setup$modified_inputs,
        reset
      )
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
    #' Write updated SWAT+ input files to `project_path`.
    #'
    write_swat_inputs = function(){
      self$.data$model_setup$modified_inputs$files_written <- TRUE
      write_swat_inputs(self$.data$model_setup$modified_inputs,
                        self$.data$model_setup$modified_inputs$file_updated,
                        self$.data$meta$project_path)
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
    #' @param write_files Should the original files of the files which were
    #'  changed als be rewritten in the project folder? Default `FALSE` only
    #'  resets the tables in the `measr_project`. `TRUE` also resets the
    #'  respective input files in `project_path`.
    #'
    reset = function(){
      if(self$.data$model_setup$modified_inputs$files_written) {
        cat('Resetting input files in project folder...')
        write_swat_inputs(self$.data$model_setup$original_inputs,
                          self$.data$model_setup$modified_inputs$file_updated,
                          self$.data$meta$project_path)
        self$.data$model_setup$modified_inputs$files_written <- FALSE
        cat(' Done!\n')
      }
      cat('Resetting tables in measR project...')
      self$.data$model_setup$modified_inputs <-
        self$.data$model_setup$original_inputs
      self$save()
      cat(' Done!\n')
    }
  )
)
