#' Load the locations of NSWRMs (links to spatial objects) from a '.csv' file
#' into the `measr_project`.
#'
#' @param file_path Path to the '.csv' location file.
#' @param nswrm_defs List with already loaded NSWRM definition tables
#' @param swat_inputs List with SWAT+ input files.
#' @param overwrite Overwrite existing location table? If `TRUE` existing
#'   location table can be overwritten.
#'
#' @returns The list with NSWRM definition tables with the NSWRM location table
#'   added.
#'
#' @importFrom dplyr left_join %>%
#' @importFrom readr cols read_csv
#' @importFrom purrr map map_lgl
#'
#' @keywords internal
#'
load_nswrm_loc <- function(file_path, nswrm_defs, swat_inputs, overwrite) {
  if(is.null(nswrm_defs)) {
    stop('No NSWRMs were defined yet. \n\n',
         'All NSWRMs must be defined before loading their locations.\n',
         "Please add the definitions with measr_project$load_nswrm_definition()",
         " before you continue.")
  }
  # Check if any input files were already updated. In this case it is not
  # allowed to reload a location file
  if(any(swat_inputs$file_updated)) {
    stop('Cannot load/update NSWRM definition tables when measures \n',
         'were already implemented in the SWAT+ project.\n',
         'If you want to load NSWRM definition tables in this project,\n',
         'you have to reset the project first with measr_project$reset().')
  }
  # Overwrite must be set to allow a rewrite
  if ('nswrm_locations' %in% names(nswrm_defs) & !overwrite) {
    stop("An NSWRM location table already exists for this project.\n",
         "If the existing table should be overwritten set ",
         "'overwrite = TRUE'.")
  }

  # Load the csv input file
  nswrm_loc <- read_csv(file_path, lazy = FALSE,
                      col_types = cols(id = 'i', .default = 'c' ),
                      na = c('', 'NA'))

  # Check the column names. The names below must be in the table.
  col_names <- c('id', 'nswrm', 'obj_id')
  col_miss <- ! col_names %in%  names(nswrm_loc)
  if(any(col_miss)) {
    stop('The following columns are missing in the NSWRM location table:\n',
        col_names[col_miss])
  }

  # Add a column which defines the type of NSWRM, such as 'management',
  # 'land_use', 'pond', ...
  nswrm_loc <- nswrm_loc %>%
    left_join(., nswrm_defs$nswrm_lookup, by = "nswrm")

  # All NSWRMs for which locations will be defined, definitions must have been
  # loaded before with load_nswrm_definition(). An error is triggered if
  # definitions are still missing.
  nswrm_defs_miss <- unique(nswrm_loc$nswrm[is.na(nswrm_loc$type)])

  if(length(nswrm_defs_miss) > 0) {
    stop('All NSWRMs must be defined before loading the NSWRM locations. \n\n',
         'Definitions for the following NSWRMs are still missing:\n',
         paste(nswrm_defs_miss, collapse = ', '), '\n\n',
         "Please add the definitions with measr_project$load_nswrm_definition()",
         " before you continue.")
  }

  if(any(is.na(nswrm_loc$id))) {
    stop("The 'id's in the following rows were interpreted as NA:\n",
         paste(nswrm_loc$id[is.na(nswrm_loc$id)], collapse = ', '))
    }

  nswrm_loc$obj_id <- map(nswrm_loc$obj_id,
                            ~ eval(parse(text = paste0('c(', .x, ')'))))

  # Check if all object IDs are available in hru.con
  id_not_in_hru <- map_lgl(nswrm_loc$obj_id,
                           ~ !all(.x %in% swat_inputs$hru.con$id))
  # Check if all object IDs are available in chandeg.con
  id_not_in_cha <- map_lgl(nswrm_loc$obj_id,
                           ~ !all(.x %in% swat_inputs$chandeg.con$id))
  # id must only be a chandeg ID for channel restoration, otherwise it is an
  # HRU ID
  id_no_obj <- ifelse(nswrm_loc$nswrm == 'channres',
                      id_not_in_cha, id_not_in_hru)

  if(any(id_no_obj)) {
    stop("The following location definitions ('id') define 'obj_id's that ",
         "are not provided in the respective SWAT+ input table:\n",
         paste(nswrm_loc$id[id_no_obj], collapse = ', '))
  }

  # Updated approach to compare list ids in pond definition and
  # location definition.
  hru_pond_ids <- nswrm_loc$obj_id[nswrm_loc$type == 'pond']
  # hru_pond_def_miss <- !map_lgl(nswrm_defs$pond$hru_id,
  #                               ~ match_ids(hru_pond_ids, .x))
  hru_pond_def_miss <- !map_lgl(hru_pond_ids,
                                ~ match_ids(nswrm_defs$pond$hru_id, .x))

  if(any(hru_pond_def_miss)) {
    stop("Pond locations ('obj_id's) were defined ",
         "for which a definition in the pond setting input file was missing:\n",
         paste(hru_pond_ids[hru_pond_def_miss], collapse = ', '))
  }

  hru_cwtl_ids <- nswrm_loc$obj_id[nswrm_loc$type == 'constr_wetland']
  hru_cwtl_def_miss <- !map_lgl(hru_cwtl_ids,
                                ~ match_ids(nswrm_defs$constr_wetland$hru_id, .x))

  if(any(hru_cwtl_def_miss)) {
    stop("Constructed wetland locations ('obj_id's) were defined ",
         "for which a definition in the constr_wetland setting input file was missing:\n",
         paste(hru_cwtl_ids[hru_cwtl_def_miss], collapse = ', '))
  }

  hru_wetl_ids <- unique(unlist(nswrm_loc$obj_id[nswrm_loc$type == 'wetland']))
  hru_wetl_def_miss <- ! hru_wetl_ids %in% nswrm_defs$wetland$hru_id

  if(any(hru_wetl_def_miss)) {
    stop("Wetland locations ('obj_id's) were defined ",
         "for which a definition in the wetland setting input file was missing:\n",
         paste(hru_wetl_ids[hru_wetl_def_miss], collapse = ', '))
  }

  nswrm_defs$nswrm_locations <- nswrm_loc

  return(nswrm_defs)
}

#' Load a definition table for an NSWRM type from a '.csv' file into the
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
#'   `type`, `plnt_com`, `mgt`, `cn2`, `cons_prac`, and
#'   `ov_mann`.
#' - `'management'`: The management definition is an `*.rds` object which
#'   must be prepared with the function
#'   `prepare_management_scenario_inputs()`. The function uses the
#'   `SWATfarmR` projects which are located in the `project_path` and
#'   generates the file `'mgt_scenarios.rds'` which is the required input
#'   to define management related NSWRMs.
#' - `'pond'`: A pond definition table includes all definitions for pond
#'   locations. The `pond` table must provide the columns `name`,
#'   `to_cha_id`, and `from_cha_id`.
#'
#' @param file_path Path to the '.csv' or '.rds' definition file.
#' @param type Type of the NSWRM which is defined by this input file. The
#'   type must be one of the options `'land_use'`, `'management'`,`'pond'`,
#'   `'wetland'`, or `'constr_wetland'`.
#' @param nswrm_defs List with already loaded NSWRM definition tables
#' @param swat_inputs List with SWAT+ input files.
#' @param overwrite Overwrite existing definition table? If `TRUE` existing
#'   definition table can be overwritten.
#'
#' @returns The list with NSWRM definition tables with the new added entry.
#'
#' @keywords internal
#'
load_nswrm_def <- function(file_path, type, nswrm_defs, swat_inputs, overwrite) {
  # if (!'nswrm_locations' %in% names(nswrm_defs)) {
  #   stop("The NSWRM location table was not loaded yet. \n",
  #        "Load the NSWRM location table with measr_object$load_nswrm_location()",
  #        " before you load the NSWRM definitions.")
  # }
  nswrm_defs_names <- names(nswrm_defs)
  # If nswrm_defs does not exist yet generate empty list
  if(is.null(nswrm_defs)) {
    nswrm_defs <- list()
    nswrm_defs$nswrm_lookup <- tibble(type  = character(),
                                      nswrm = character())
  }

  # New definitions can only be loaded when no NSWRMs were not implemented yet.
  if(any(swat_inputs$file_updated)) {
    stop('Cannot load/update NSWRM definition tables when measures \n',
         'were already implemented in the SWAT+ project.\n',
         'If you want to load NSWRM definition tables in this project,\n',
         'you have to reset the project first with measr_project$reset().')
  }

  # For testing so far only the two types land_use and pond are implemented.
  # Will be updated when additional routines are inlcuded.
  if (!type %in% c('land_use', 'management', 'pond',
                   'wetland', 'constr_wetland')) {
    stop("'type' must be 'land_use', 'management', 'pond', 'wetland', or 'constr_wetland'.")
  }

  if (type %in% names(nswrm_defs) & !overwrite) {
    stop("An NSWRM definition table for the type '", type, "' already exists.\n",
         "If the existing table should be overwritten please set ",
         "'overwrite = TRUE'.")
  }

  if (type == 'land_use') {
    nswrm_defs$land_use <- load_luse_def(file_path, swat_inputs)
    nswrm_defs$nswrm_lookup <- update_nswrm_lookup(nswrm_defs$nswrm_lookup,
                                                   type,
                                                   nswrm_defs$land_use$nswrm,
                                                   overwrite)

  } else if (type == 'management') {
    nswrm_defs$management <- load_mgt_def(file_path, swat_inputs)
    nswrm_defs$nswrm_lookup <- update_nswrm_lookup(nswrm_defs$nswrm_lookup,
                                                   type,
                                                   names(nswrm_defs$management),
                                                   overwrite)
  } else if (type == 'pond') {
    nswrm_defs$pond <- load_water_def(file_path, swat_inputs, type)
    nswrm_defs$nswrm_lookup <- update_nswrm_lookup(nswrm_defs$nswrm_lookup,
                                                   type,
                                                   type,
                                                   overwrite)
  } else if (type == 'wetland') {
    nswrm_defs$wetland <- load_water_def(file_path, swat_inputs, type)
    nswrm_defs$nswrm_lookup <- update_nswrm_lookup(nswrm_defs$nswrm_lookup,
                                                   type,
                                                   type,
                                                   overwrite)
  } else if (type == 'constr_wetland') {
    nswrm_defs$constr_wetland <- load_water_def(file_path, swat_inputs, type)
    nswrm_defs$nswrm_lookup <- update_nswrm_lookup(nswrm_defs$nswrm_lookup,
                                                   type,
                                                   type,
                                                   overwrite)
  }

  if(type %in% nswrm_defs_names & overwrite) {
    warning("NSWRM definition table for '", type, "' will be overwritten!\n",
            'This also requires to reload the NSWRM location table if it was ',
            'already loaded before')
    nswrm_defs$nswrm_locations <- NULL
  }

  return(nswrm_defs)
}

#' Load the definition input table for NSWRMs which are represented by a land
#' use change
#'
#' @param file_path Path to the '.csv' definition file.
#' @param swat_inputs List with SWAT+ input files.
#'
#' @returns The loaded land use definition table as a tibble.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_df
#' @importFrom readr cols read_csv
#' @importFrom tidyr replace_na
#'
#' @keywords internal
#'
load_luse_def <- function(file_path, swat_inputs) {
  luse_def <- read_csv(file_path, lazy = FALSE,
                       col_types = cols(.default = 'c'), na = c('', 'NA'))

  col_names <-  c('nswrm', 'plnt_com', 'mgt',
                  'cn2', 'cons_prac', 'ov_mann')
  col_miss <- ! col_names %in%  names(luse_def)
  if(any(col_miss)) {
    stop("The following columns are missing in the 'land_use' ",
         "definition table:\n", paste(col_names[col_miss], collapse = ', '))
  }

  if (any(is.na(luse_def$nswrm))) {
    stop("The column 'nswrm' cannot have empty fields.")
  }

  luse_def <- map_df(luse_def, ~replace_na(.x, '::keep::'))

  # Check optional columns 'tile' and 'lum_dtl'

  if (!'tile' %in% names(luse_def)) {
    luse_def$tile <- '::keep::'
  }

  if (!'lum_dtl' %in% names(luse_def)) {
    luse_def$lum_dtl <- '::keep::'
  }

  dtl_names <- luse_def$lum_dtl %>%
    str_remove_all(., 'c\\(|\\)') %>%
    str_split(., ',') %>%
    unlist(.) %>%
    str_trim(.) %>%
    unique(.)

  # Checks for all inputs if they are available in the respective SWAT+ input
  # files
  lum_plnt_miss <- !luse_def$plnt_com %in%
    c(swat_inputs$plant.ini$pcom_name, 'null', '::keep::')
  lum_mgt_miss  <- !luse_def$mgt %in%
    c(swat_inputs$management.sch$name, 'null', '::keep::')
  lum_cn2_miss  <- !luse_def$cn2 %in%
    c(swat_inputs$cntabe.lum$name, 'null', '::keep::')
  lum_cpr_miss  <- !luse_def$cons_prac %in%
    c(swat_inputs$cons_practice.lum$name, 'null', '::keep::')
  lum_ovn_miss  <- !luse_def$ov_mann %in%
    c(swat_inputs$ovn_table.lum$name, 'null', '::keep::')
  lum_tile_miss  <- !luse_def$tile %in%
    c(swat_inputs$tiledrain.str$name, 'null', '::keep::')
  lum_dtl_miss  <- !dtl_names %in%
    c(swat_inputs$lum.dtl_names, 'null', '::keep::')

  if (any(c(lum_plnt_miss, lum_mgt_miss, lum_cn2_miss,
            lum_cpr_miss, lum_ovn_miss, lum_tile_miss, lum_dtl_miss))) {
    if(any(lum_plnt_miss)) {
      plnt_msg <- paste0("'plnt_com' not defined in 'plant.ini': ",
                         paste(unique(luse_def$plnt_com[lum_plnt_miss]),
                               collapse = ', '), '\n')
    } else {
      plnt_msg <- ''
    }
    if(any(lum_mgt_miss)) {
      sch_msg  <- paste0("'mgt' not defined in 'management.sch': ",
                         paste(unique(luse_def$mgt[lum_mgt_miss]),
                               collapse = ', '), '\n')
    } else {
      sch_msg  <- ''
    }
    if(any(lum_cn2_miss)) {
      cn2_msg <- paste0("'cn2' not defined in 'cntabe.lum': ",
                         paste(unique(luse_def$cn2[lum_cn2_miss]),
                               collapse = ', '), '\n')
    } else {
      cn2_msg <- ''
    }
    if(any(lum_cpr_miss)) {
      cpr_msg  <- paste0("'cons_prac' not defined in 'cons_practice.lum': ",
                         paste(unique(luse_def$cons_prac[lum_cpr_miss]),
                               collapse = ', '), '\n')
    } else {
      cpr_msg  <- ''
    }
    if(any(lum_ovn_miss)) {
      ovn_msg  <- paste0("'ov_mann' not defined in 'ovn_table.lum': ",
                         paste(unique(luse_def$ov_mann[lum_ovn_miss]),
                               collapse = ', '), '\n')
    } else {
      ovn_msg  <- ''
    }
    if(any(lum_tile_miss)) {
      tile_msg  <- paste0("'tile' not defined in 'tiledrain.str': ",
                         paste(unique(luse_def$tile[lum_tile_miss]),
                               collapse = ', '), '\n')
    } else {
      tile_msg  <- ''
    }
    if(any(lum_dtl_miss)) {
      dtl_msg  <- paste0("Operation schedules not defined in 'lum.dtl': ",
                         paste(unique(dtl_names[lum_dtl_miss]),
                               collapse = ', '), '\n')
    } else {
      dtl_msg  <- ''
    }

    stop('The following options are not defined in the respective SWAT+ input ',
         'files: \n\n',
         plnt_msg, sch_msg, cn2_msg, cpr_msg, ovn_msg, tile_msg, dtl_msg,
         '\n\nPlease do the following to solve this issue:\n',
         'i)   Add the missing entries in the SWAT+ input files\n',
         "ii)  Reload all SWAT+ input files with ",
         "measr_project$reload_swat_inputs()\n",
         '     (can only be done when no NSWRMs wer implemented yet)\n',
         "iii) Load again 'land_use' definition table with ",
         "measr_project$load_nswrm_definition()")
  }

  return(luse_def)
}

#' Load the definition input table for NSWRMs which are represented by a land
#' use change
#'
#' @param file_path Path to the '.csv' definition file.
#' @param swat_inputs List with SWAT+ input files.
#'
#' @returns The loaded land use definition table as a tibble.
#'
#' @importFrom readr read_rds
#'
#' @keywords internal
#'
load_mgt_def <- function(file_path, swat_inputs) {
  # Load the .rds management input file which was generated with
  # prepare_management_scenario_inputs()
  mgt_def <- read_rds(file_path)

  # Check if the land use labels in the SWAT+ project and the status quo
  # that is saved in the rds file are the same.
  if(any(swat_inputs$hru_data.hru$lu_mgt != mgt_def$status_quo$hru_data$lu_mgt)) {
    stop("The 'lu_mgt' for the status quo case in the management definition \n",
         "does not match the 'lu_mgt' labels of hru-data.hru of the SWAT+ project.")
  }

  # Check if the numbers of operations for all defined managements match
  # between the SWAT+ project and the rds file. This can indicate that
  # the number of years which were written differ.
  n_op_mgt <- distinct(swat_inputs$management.sch, name, numb_ops) %>%
    set_names(c('name', 'numb_ops_mgt'))
  n_op_quo <- distinct(mgt_def$status_quo$management.sch, name, numb_ops) %>%
    left_join(., n_op_mgt, by = 'name') %>%
    mutate(numb_ops_mgt = ifelse(is.na(numb_ops_mgt), -999, numb_ops_mgt)) %>%
    mutate(n_diff = numb_ops - numb_ops_mgt)

  if(any(n_op_quo$n_diff != 0)) {
    stop("The number of operations defined in the 'management.sch' for the ",
         "status quo case in the management definition\n",
         "does not match the number operations defined in the SWAT+ project.\n")
  }

  # Check if the operation sequence in one of the selected managements
  # match between SWAT+ project and the rds file. Otherwise this can indicate
  # that different years are written in the project and the scenario input file.
  n_op_max <- filter(n_op_quo, numb_ops == max(numb_ops))[1,]

  mgt_op_max  <- filter(swat_inputs$management.sch, name == n_op_max$name)
  mgt_quo_max <- filter(mgt_def$status_quo$management.sch, name == n_op_max$name)

  if(any(mgt_op_max$op_typ != mgt_quo_max$op_typ) |
     any(mgt_op_max$mon    != mgt_quo_max$mon)    |
     any(mgt_op_max$day    != mgt_quo_max$day)) {
    stop("Operations defined in the 'management.sch' for the ",
         "status quo case in the management definition\n",
         "do not match the operations defined in the SWAT+ project.\n")
  }

  # To indicate that labels/managements/plant.ini in the SWAT+ input files
  # were replaced by one of the scenario inputs the suffixes (lum, mgt, com)
  # are replaced by three character suffixes which are the first 3 letters of
  # the scenario label.
  short_labels <- get_mgt_short_labels(names(mgt_def))

  for (scen_i in names(mgt_def)) {
    mgt_def[[scen_i]]$hru_data.hru$lu_mgt <-
      str_replace(mgt_def[[scen_i]]$hru_data.hru$lu_mgt,
                  'lum$', short_labels[scen_i]) %>%
      str_replace(., paste0('null', short_labels[scen_i]), 'null')
    mgt_def[[scen_i]]$landuse.lum$name <-
      str_replace(mgt_def[[scen_i]]$landuse.lum$name,
                  'lum$', short_labels[scen_i]) %>%
        str_replace(., paste0('null', short_labels[scen_i]), 'null')
    mgt_def[[scen_i]]$landuse.lum$plnt_com <-
      str_replace(mgt_def[[scen_i]]$landuse.lum$plnt_com,
                  'com$', short_labels[scen_i]) %>%
        str_replace(., paste0('null', short_labels[scen_i]), 'null')
    mgt_def[[scen_i]]$landuse.lum$mgt <-
      str_replace(mgt_def[[scen_i]]$landuse.lum$mgt,
                  'mgt$', short_labels[scen_i]) %>%
        str_replace(., paste0('null', short_labels[scen_i]), 'null')
    mgt_def[[scen_i]]$management.sch$name <-
      str_replace(mgt_def[[scen_i]]$management.sch$name,
                  'mgt$', short_labels[scen_i]) %>%
        str_replace(., paste0('null', short_labels[scen_i]), 'null')
    mgt_def[[scen_i]]$plant.ini$pcom_name <-
      str_replace(mgt_def[[scen_i]]$plant.ini$pcom_name,
                  'com$', short_labels[scen_i]) %>%
        str_replace(., paste0('null', short_labels[scen_i]), 'null')
  }

  return(mgt_def)
}

#' Load the definition input table for HRUs which will be replaced by ponds,
#' or where wetlands will be added
#'
#' @param file_path Path to the '.csv' definition file.
#' @param swat_inputs List with SWAT+ input files.
#' @param type Either 'pond' or 'wetland'. Label controls different settings in
#'   definition
#'
#' @returns The loaded pond/wetland definition table as a tibble.
#'
#' @importFrom dplyr group_by left_join mutate select summarise %>%
#' @importFrom purrr map map_lgl
#' @importFrom readr cols col_guess read_csv
#' @importFrom tidyr unnest
#' @importFrom tidyselect all_of
#'
#' @keywords internal
#'
load_water_def <- function(file_path, swat_inputs, type) {
  def_tbl <- read_csv(file_path, lazy = FALSE,
                       col_types = cols(col_guess()),
                       na = c('', 'NA'))
  # Land object type wetlands cannot have cha_from_ids. This is now checked
  if (type == 'wetland' & 'cha_from_id' %in% names(def_tbl)) {
    stop("'cha_from_id' cannot be defined for type 'wetland' as wetlands cannot",
         ' \n receive water from channels. If this is required please implement',
         " \n thos as 'constr_wetland'.")
  }

  # Initialization and checks of input table columns
  def_tbl <- check_settings_column(def_tbl, 'hru_id', 'integer', type)
  if (type == 'wetland') {
    def_tbl <- check_settings_column(def_tbl, 'lu_mgt', 'character', 'all')
  }
  def_tbl <- check_settings_column(def_tbl, 'cha_to_id', 'integer', type)
  if (!'cha_from_id' %in% names(def_tbl) & type != 'wetland') {
    def_tbl <- mutate(def_tbl, cha_from_id = NA_integer_)
  } else if (type != 'wetland') {
    def_tbl$cha_from_id <- map(def_tbl$cha_from_id,
                                ~ eval(parse(text = paste0('c(', .x, ')')))) %>%
      map(., ~as.integer(.x))
  }
  if(type %in% c('pond', 'constr_wetland')) {
    hyd_par_names <- names(swat_inputs$hydrology.res)[4:11]
  } else if (type == 'wetland') {
    hyd_par_names <- names(swat_inputs$hydrology.wet)[2:11]
  }

  for (par_i in hyd_par_names) {
    def_tbl <- check_settings_column(def_tbl, par_i, 'numeric', 'all')
  }

  def_tbl <- check_settings_column(def_tbl, 'rel', 'character', 'all')
  def_tbl <- check_settings_column(def_tbl, 'sed', 'character', 'all')
  def_tbl <- check_settings_column(def_tbl, 'nut', 'character', 'all')

  def_tbl <- select(def_tbl, hru_id, any_of('lu_mgt'), cha_to_id,
                    any_of('cha_from_id'), all_of(hyd_par_names), rel, sed, nut)

  hru_id_na <- is.na(def_tbl$hru_id)
  is_no_hru_id    <- map_lgl(def_tbl$hru_id, ~ any(!.x  %in% swat_inputs$hru_data.hru$id))
  if(type != 'wetland') {
    is_no_lu_mgt <- FALSE
    cha_id_na <- is.na(def_tbl$cha_to_id)
    is_no_cha_to_id <- ! def_tbl$cha_to_id %in% swat_inputs$chandeg.con$id
    is_no_cha_fr_id <- map_lgl(def_tbl$cha_from_id,
                         ~ !all(is.na(.x) | .x %in% swat_inputs$chandeg.con$id))
  } else {
    is_no_lu_mgt <- ! (def_tbl$lu_mgt %in% swat_inputs$landuse.lum$name |
                         is.na(def_tbl$lu_mgt))
    cha_id_na <- FALSE
    is_no_cha_to_id <- ! (def_tbl$cha_to_id %in% swat_inputs$chandeg.con$id |
                          is.na(def_tbl$cha_to_id))
    is_no_cha_fr_id <- FALSE
  }
  is_no_rel_name  <- ! (def_tbl$rel %in% swat_inputs$res_rel.dtl_names |
                        is.na(def_tbl$rel))
  is_no_sed_name  <- ! (def_tbl$sed %in% swat_inputs$sediment.res$name |
                       is.na(def_tbl$sed))
  is_no_nut_name  <- ! (def_tbl$nut %in% swat_inputs$nutrients.res$name |
                        is.na(def_tbl$nut))

  if (any(c(hru_id_na, is_no_lu_mgt, cha_id_na, is_no_hru_id,
            is_no_cha_to_id, is_no_cha_fr_id,
            is_no_rel_name, is_no_sed_name,
            is_no_nut_name))) {
    if(any(hru_id_na)) {
      hru_na_msg <- paste0("Row IDs where 'hru_id' returns NA: ",
                           paste(which(hru_id_na), collapse = ', '), '\n')
    } else {
      hru_na_msg <- ''
    }
    if(any(cha_id_na)) {
      cha_na_msg <- paste0("Row IDs where 'cha_to_id' returns NA: ",
                           paste(which(cha_id_na), collapse = ', '), '\n')
    } else {
      cha_na_msg <- ''
    }
    if(any(is_no_hru_id)) {
      no_hru_msg <-
        paste0("Row IDs where 'hru_id' is not defined in hru-data.hru: ",
               paste(which(is_no_hru_id), collapse = ', '), '\n')
    } else {
      no_hru_msg <- ''
    }
    if(any(is_no_lu_mgt)) {
      no_lum_msg <-
        paste0("Row IDs where 'lu_mgt' is not defined in landuse.lum: ",
                           paste(which(is_no_lu_mgt), collapse = ', '), '\n')
    } else {
      no_lum_msg <- ''
    }
    if(any(is_no_cha_to_id)) {
      no_cha_to_msg <-
        paste0("Row IDs where 'cha_to_id' is not defined in channel-lte.cha: ",
               paste(which(is_no_cha_to_id), collapse = ', '), '\n')
    } else {
      no_cha_to_msg <- ''
    }
    if(any(is_no_cha_fr_id)) {
      no_cha_fr_msg <-
        paste0("Row IDs where 'cha_from_id' is not defined in channel-lte.cha: ",
               paste(which(is_no_cha_fr_id), collapse = ', '), '\n')
    } else {
      no_cha_fr_msg <- ''
    }
    if(any(is_no_rel_name)) {
      no_rel_name_msg <-
        paste0("Row IDs where 'rel' is not defined in res_rel.dtl: ",
               paste(which(is_no_rel_name), collapse = ', '), '\n')
    } else {
      no_rel_name_msg <- ''
    }
    if(any(is_no_sed_name)) {
      no_sed_name_msg <-
        paste0("Row IDs where 'sed' is not defined in sediment.res: ",
               paste(which(is_no_sed_name), collapse = ', '), '\n')
    } else {
      no_sed_name_msg <- ''
    }
    if(any(is_no_nut_name)) {
      no_nut_name_msg <-
        paste0("Row IDs where 'nut' is not defined in nutrients.res: ",
               paste(which(is_no_nut_name), collapse = ', '), '\n')
    } else {
      no_nut_name_msg <- ''
    }

    stop('The following issues where identified for the rows of the pond ',
         'definition input table: \n\n',
         hru_na_msg, cha_na_msg, no_hru_msg, no_lum_msg,
         no_cha_to_msg, no_cha_fr_msg,
         no_rel_name_msg, no_sed_name_msg, no_nut_name_msg,
         '\n\nPlease fix the reported issues in the .csv file and reload it.')
  }

  hru_area <- select(swat_inputs$hru.con, id, area)

  if(type %in% c('pond', 'constr_wetland')) {
    rel_dflt <- ifelse('drawdown_days' %in% swat_inputs$res_rel.dtl_names,
                       'drawdown_days',
                       'null')
  } else {
    rel_dflt <- ifelse('wetland' %in% swat_inputs$res_rel.dtl_names,
                       'wetland',
                       'null')
  }
  type_lbl <- ifelse(type %in% c('pond', 'constr_wetland'), 'res', 'wet')
  sed_dflt <- ifelse(paste0('sed', type_lbl, 1) %in% swat_inputs$sediment.res$name,
                     paste0('sed', type_lbl, 1),
                     'null')
  nut_dflt <- ifelse(paste0('nut', type_lbl, 1) %in% swat_inputs$nutrients.res$name,
                     paste0('nut', type_lbl, 1),
                     'null')

  # Initialize pond and wetland parameters and pointers
  if (type %in% c('pond', 'constr_wetland')) {
    pond_area <- def_tbl %>%
      select(hru_id) %>%
      mutate(id = 1:nrow(.)) %>%
      unnest(., cols = c(hru_id)) %>%
      left_join(., hru_area, by = c('hru_id' = 'id')) %>%
      group_by(id) %>%
      summarise(., area = sum(area))

    def_tbl <- def_tbl %>%
      mutate(area_ps = ifelse(is.na(vol_ps), 0.8*pond_area$area, area_ps),
             vol_ps  = ifelse(is.na(vol_ps), 2*area_ps, vol_ps),
             area_es = ifelse(is.na(area_es), pond_area$area, area_es),
             vol_es  = ifelse(is.na(vol_es), 3*area_es, vol_es),
             k       = ifelse(is.na(k), 0, k),
             evap_co = ifelse(is.na(evap_co), 0.6, evap_co),
             shp_co1 = ifelse(is.na(shp_co1), 0, shp_co1),
             shp_co2 = ifelse(is.na(shp_co2), 0, shp_co2),
             rel     = ifelse(is.na(rel), rel_dflt, rel),
             sed     = ifelse(is.na(sed), sed_dflt, sed),
             nut     = ifelse(is.na(nut), nut_dflt, nut)
             )
    if(any(def_tbl$area_ps >= pond_area$area, na.rm = T)) {
      stop("'area_ps' cannot be larger than the land area which is replaced by a pond.")
    }
    if(any(def_tbl$area_es > pond_area$area, na.rm = T)) {
      stop("'area_es' cannot be larger than the land area which is replaced by a pond.")
    }
    if(any(def_tbl$area_ps > def_tbl$area_es, na.rm = T)) {
      stop("'area_ps' cannot be larger than 'area_es'.")
    }
    if(any(def_tbl$vol_ps > def_tbl$vol_es, na.rm = T)) {
      stop("'vol_ps' cannot be larger than 'vol_es'.")
    }

  } else if (type == 'wetland') {
    def_tbl <- def_tbl %>%
      mutate(hru_ps      = ifelse(is.na(hru_ps),  0.1, hru_ps),
             dp_ps       = ifelse(is.na(dp_ps),  20.0, dp_ps),
             hru_es      = ifelse(is.na(hru_es), 0.25, hru_es),
             dp_es       = ifelse(is.na(dp_es), 100.0, dp_es),
             k           = ifelse(is.na(k), 0.01, k),
             evap        = ifelse(is.na(evap), 0.7, evap),
             vol_area_co = ifelse(is.na(vol_area_co), 1.0, vol_area_co),
             vol_dp_a    = ifelse(is.na(vol_dp_a), 1.0, vol_dp_a),
             vol_dp_b    = ifelse(is.na(vol_dp_b), 1.0, vol_dp_b),
             hru_frac    = ifelse(is.na(hru_frac), 0.5, hru_frac),
             rel     = ifelse(is.na(rel), rel_dflt, rel),
             sed     = ifelse(is.na(sed), sed_dflt, sed),
             nut     = ifelse(is.na(nut), nut_dflt, nut)

             )

    if(any(def_tbl$hru_ps > def_tbl$hru_es, na.rm = T)) {
      stop("'hru_ps' cannot be larger than 'hru_es'.")
    }
    if(any(def_tbl$dep_ps > def_tbl$dep_es, na.rm = T)) {
      stop("'dep_ps' cannot be larger than 'dep_es'.")
    }
  }

  return(def_tbl)
}

#' Add entries for NSWRMs in the NSWRM lookup table
#'
#' @param nswrm_lookup NSWRM lookup table
#' @param type NSWRM type in which the NSWRMs are grouped based on their
#'   implementation.
#' @param nswrm Character vector of NSWRM labels.
#' @param overwrite If an NSWRM definition is overwritten, then the lookup is
#'   also replaced
#'
#' @returns The NSWRM lookup tibble with the newly added entries.
#'
#' @importFrom tibble add_row
#'
#' @keywords internal
#'
update_nswrm_lookup <- function(nswrm_lookup, type, nswrm, overwrite) {
  if(overwrite) {
    nswrm_lookup <- nswrm_lookup[nswrm_lookup$type != type, ]
  }
  is_in_lookup <- nswrm %in% nswrm_lookup$nswrm
  if(any(is_in_lookup)) {
    stop("The following 'nswrm' labels are already in use by other measure ",
         "types:\n", paste(nswrm[is_in_lookup], collapse = ', '), '\n',
         "Please use different names to load those NSWRM definitions.")
  }
  nswrm_lookup <- add_row(nswrm_lookup,
                          type  = type,
                          nswrm = nswrm)

  return(nswrm_lookup)
}

#' Derive short labels for the management names to be used as suffixes in the
#' management definition files.
#'
#' @param mgt_lables Vector with management scenario names.
#'
#' @returns A vector with short labels with 3 characters.
#'
#' @importFrom purrr map map_chr
#' @importFrom stringr str_sub str_split
#'
#' @keywords internal
#'
get_mgt_short_labels <- function(mgt_labels) {
  lbl_sub  <- str_sub(mgt_labels, 1, 3)
  lbl_cnt  <- table(lbl_sub)
  lbl_dupl <- which(lbl_cnt > 1)
  has_dupl <- length(lbl_dupl) > 0

  while(has_dupl) {
    lbl_new <- str_split(lbl[lbl_sub %in% names(lbl_dupl)], '') %>%
      map(., ~ .x[c(1,2, round(runif(1, 1, length(.x))))]) %>%
      map_chr(., ~ paste(.x, collapse = ''))
    lbl_sub[lbl_sub %in% names(lbl_dupl)] <- lbl_new
    lbl_cnt  <- table(lbl_sub)
    lbl_dupl <- which(lbl_cnt > 1)
    has_dupl <- length(lbl_dupl) > 0
  }

  names(lbl_sub) <- mgt_labels

  return(lbl_sub)
}

#' Check columns and types of data for settings input table.
#'
#' @param tbl Settings table.
#' @param col_name Name of column which is checked and/or coerced.
#' @param val_class Class which data should have (e.g. character, integer, or numeric).
#' @param measr_type Type of measure, either pond, wetland or all.
#'
#' @returns The input table with data type coerced to val_class if necessary.
#'
#' @keywords internal
#'
check_settings_column <- function(tbl, col_name, val_class, measr_type) {
  if(col_name == 'hru_id' & ! col_name %in% names(tbl)) {
    stop("The column 'hru_id' is not defined.")
  }
  if (! col_name %in% names(tbl) & measr_type %in% c('pond', 'constr_wetland')) {
    stop(paste0("'", col_name,"' must be defined for all ponds."))
  } else if (!col_name %in% names(tbl) | all(is.na(tbl[[col_name]]))) {
    tbl[[col_name]] <- as(NA, val_class)
  } else if (measr_type %in% c('pond', 'constr_wetland')) {
    tbl[[col_name]] <- map(tbl[[col_name]],
                               ~ eval(parse(text = paste0('c(', .x, ')')))) %>%
      map(., ~as.integer(.x))
  } else if(measr_type == 'wetland') {
    tbl[[col_name]] <- as(tbl[[col_name]], val_class)
  }

  return(tbl)
}
