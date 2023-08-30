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
  if(any(swat_inputs$file_updated)) {
    stop('Cannot load/update NSWRM definition tables when measures \n',
         'were already implemented in the SWAT+ project.\n',
         'If you want to load NSWRM definition tables in this project,\n',
         'you have to reset the project first with measr_project$reset().')
  }
  if ('nswrm_locations' %in% names(nswrm_defs) & !overwrite) {
    stop("An NSWRM location table already exists for this project.\n",
         "If the existing table should be overwritten set ",
         "'overwrite = TRUE'.")
  }

  nswrm_loc <- read_csv(file_path, lazy = FALSE,
                      col_types = cols(id = 'i', .default = 'c' ),
                      na = c('', 'NA'))

  col_names <- c('id', 'nswrm', 'obj_id')
  col_miss <- ! col_names %in%  names(nswrm_loc)
  if(any(col_miss)) {
    stop('The following columns are missing in the NSWRM location table:\n',
        col_names[col_miss])
  }

  nswrm_loc <- nswrm_loc %>%
    left_join(., nswrm_defs$nswrm_lookup, by = "nswrm")

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
#'   `type`, `lum_plnt`, `lum_mgt`, `lum_cn2`, `lum_cpr`, and
#'   `lum_ovn`.
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
#'   type must be one of the options `'land_use'`, `'pond'`.
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
  if (!type %in% c('land_use', 'management', 'pond')) {
    stop("'type' must be 'land_use', 'management', or 'pond'.")
  }

  if (type %in% names(nswrm_defs) & !overwrite) {
    stop("An NSWRM definition table for the type '", type, "' already exists.\n",
         "If the existing table should be overwritten please set ",
         "'overwrite = TRUE'.")
  } else if(type %in% names(nswrm_defs) & overwrite) {
    warning("NSWRM definition table for '", type, "' will be overwritten!\n",
            'This also requires to reload the NSWRM location table if it was',
            'already loaded before')
    nswrm_defs$nswrm_locations <- NULL
  }

  if (type == 'land_use') {
    nswrm_defs$land_use <- load_luse_def(file_path, swat_inputs)
    nswrm_defs$nswrm_lookup <- update_nswrm_lookup(nswrm_defs$nswrm_lookup,
                                                   'land_use',
                                                   nswrm_defs$land_use$nswrm,
                                                   overwrite)

  } else if (type == 'management') {
    nswrm_defs$management <- load_mgt_def(file_path, swat_inputs)
    nswrm_defs$nswrm_lookup <- update_nswrm_lookup(nswrm_defs$nswrm_lookup,
                                                   'management',
                                                   names(nswrm_defs$management),
                                                   overwrite)
  } else if (type == 'pond') {
    nswrm_defs$pond <- load_pond_def(file_path, swat_inputs)
    nswrm_defs$nswrm_lookup <- update_nswrm_lookup(nswrm_defs$nswrm_lookup,
                                                   'pond',
                                                   'pond',
                                                   overwrite)
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
                       col_types = cols(.default = 'c'), na = c('', 'NA')) %>%
    map_df(., ~replace_na(.x, 'null'))

  col_names <-  c('nswrm', 'lum_plnt', 'lum_mgt',
                  'lum_cn2', 'lum_cpr', 'lum_ovn')
  col_miss <- ! col_names %in%  names(luse_def)
  if(any(col_miss)) {
    stop("The following columns are missing in the 'land_use' ",
         "definition table:\n", col_names[col_miss])
  }

  # Checks for all inputs if they are available in the respective SWAT+ input
  # files
  lum_plnt_miss <- !luse_def$lum_plnt %in%
    c(swat_inputs$plant.ini$pcom_name, 'null')
  lum_mgt_miss  <- !luse_def$lum_mgt %in%
    c(swat_inputs$management.sch$name, 'null')
  lum_cn2_miss  <- !luse_def$lum_cn2 %in%
    c(swat_inputs$cntabe.lum$name, 'null')
  lum_cpr_miss  <- !luse_def$lum_cpr %in%
    c(swat_inputs$cons_practice.lum$name, 'null')
  lum_ovn_miss  <- !luse_def$lum_ovn %in%
    c(swat_inputs$ovn_table.lum$name, 'null')

  if (any(c(lum_plnt_miss, lum_mgt_miss, lum_cn2_miss,
            lum_cpr_miss, lum_ovn_miss))) {
    if(any(lum_plnt_miss)) {
      plnt_msg <- paste0("'lum_plnt' not defined in 'plant.ini': ",
                         paste(unique(luse_def$lum_plnt[lum_plnt_miss]),
                               collapse = ', '), '\n')
    } else {
      plnt_msg <- ''
    }
    if(any(lum_mgt_miss)) {
      sch_msg  <- paste0("'lum_mgt'  not defined in 'management.sch': ",
                         paste(unique(luse_def$lum_mgt[lum_mgt_miss]),
                               collapse = ', '), '\n')
    } else {
      sch_msg  <- ''
    }
    if(any(lum_cn2_miss)) {
      cn2_msg <- paste0("'lum_cn2' not defined in 'cntabe.lum': ",
                         paste(unique(luse_def$lum_cn2[lum_cn2_miss]),
                               collapse = ', '), '\n')
    } else {
      cn2_msg <- ''
    }
    if(any(lum_cpr_miss)) {
      cpr_msg  <- paste0("'lum_cpr' not defined in 'cons_practice.lum': ",
                         paste(unique(luse_def$lum_cpr[lum_cpr_miss]),
                               collapse = ', '), '\n')
    } else {
      cpr_msg  <- ''
    }
    if(any(lum_ovn_miss)) {
      ovn_msg  <- paste0("'lum_ovn' not defined in 'ovn_table.lum': ",
                         paste(unique(luse_def$lum_ovn[lum_ovn_miss]),
                               collapse = ', '), '\n')
    } else {
      ovn_msg  <- ''
    }

    stop('The following options are not defined in the respective SWAT+ input ',
         'files: \n\n',
         plnt_msg, sch_msg, cn2_msg, cpr_msg, ovn_msg,
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
  mgt_def <- read_rds(file_path)

  if(any(swat_inputs$hru_data.hru$lu_mgt != mgt_def$status_quo$hru_data$lu_mgt)) {
    stop("The 'lu_mgt' for the status quo case in the management definition \n",
         "does not match the 'lu_mgt' labels of hru-data.hru of the SWAT+ project.")
  }
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

  return(mgt_def)
}

#' Load the definition input table for HRUs which will be replaced by ponds
#'
#' @param file_path Path to the '.csv' definition file.
#' @param swat_inputs List with SWAT+ input files.
#'
#' @returns The loaded pond definition table as a tibble.
#'
#' @importFrom dplyr mutate %>%
#' @importFrom purrr map map_lgl
#' @importFrom readr cols read_csv
#'
#' @keywords internal
#'
load_pond_def <- function(file_path, swat_inputs) {
  pond_def <- read_csv(file_path, lazy = FALSE,
                       col_types = cols(hru_id = 'i', cha_to_id = 'i',
                                        .default = 'c'), na = c('', 'NA'))

  if (!'cha_from_id' %in% names(pond_def)) {
    pond_def <- mutate(pond_def, cha_from_id = NA_integer_)
  } else {
    pond_def$cha_from_id <- map(pond_def$cha_from_id,
                                ~ eval(parse(text = paste0('c(', .x, ')'))))
  }

  hru_id_na <- is.na(pond_def$hru_id)
  cha_id_na <- is.na(pond_def$cha_to_id)
  is_no_hru_id    <- ! pond_def$hru_id %in% swat_inputs$hru_data.hru$id
  is_no_cha_to_id <- ! pond_def$cha_to_id %in% swat_inputs$chandeg.con$id
  is_no_cha_fr_id <- map_lgl(pond_def$cha_from_id,
                       ~ !all(is.na(.x) | .x %in% swat_inputs$chandeg.con$id))
  # hru_def_miss <-

  if (any(c(hru_id_na, cha_id_na, is_no_hru_id,
            is_no_cha_to_id, is_no_cha_fr_id))) {
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

    stop('The following issues where identified for the rows of the pond ',
         'definition input table: \n\n',
         hru_na_msg, cha_na_msg, no_hru_msg, no_cha_to_msg, no_cha_fr_msg,
         '\n\nPlease fix the reported issues in the .csv file and reload it.')
  }

  return(pond_def)
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
