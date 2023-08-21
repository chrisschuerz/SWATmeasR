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
#'   `nswrm`, `lum_plnt`, `lum_mgt`, `lum_cn2`, `lum_cpr`, and
#'   `lum_ovn`.
#' - `'pond'`: A pond definition table includes all definitions for pond
#'   locations. The `pond` table must provide the columns `name`,
#'   `to_cha_id`, and `from_cha_id`.
#'
#' @param file_path Path to the '.csv' definition file.
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
  if(any(swat_inputs$file_updated)) {
    stop('Cannot load/update NSWRM definition tables when measures \n',
         'were already implemented in the SWAT+ project.\n',
         'If you want to load NSWRM definition tables in this project,\n',
         'you have to reset the project first with measr_object$reset().')
  }
  if (!type %in% c('land_use', 'pond')) {
    stop("'type' must be 'land_use', or 'pond'.")
  }
  if(is.null(nswrm_defs)){
    nswrm_defs <- list()
  }

  if (type %in% names(nswrm_defs) & !overwrite) {
    stop("An NSWRM definition table for the type '", type, "' already exists.\n",
         "If the existing table should be overwritten please set ",
         "'overwrite = TRUE'.")
  }

  if (type == 'land_use') {
    nswrm_defs$land_use <- load_luse_def(file_path, swat_inputs)
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
  # Checks for all inputs if they are available in the respective SWAT+ input
  # files
  lum_plnt_miss <- !luse_def$lum_plnt %in% c(swat_inputs$plant.ini$pcom_name, 'null')
  lum_mgt_miss  <- !luse_def$lum_mgt %in% c(swat_inputs$management.sch$name, 'null')
  lum_cn2_miss  <- !luse_def$lum_cn2 %in% c(swat_inputs$cntabe.lum$name, 'null')
  lum_cpr_miss  <- !luse_def$lum_cpr %in% c(swat_inputs$cons_practice.lum$name, 'null')
  lum_ovn_miss  <- !luse_def$lum_ovn %in% c(swat_inputs$ovn_table.lum$name, 'null')

  if (any(c(lum_plnt_miss, lum_mgt_miss, lum_cn2_miss,
            lum_cpr_miss, lum_ovn_miss))) {
    if(any(lum_plnt_miss)) {
      plnt_msg <- paste0("'lum_plnt' not defined in 'plant.ini': ",
                         paste(luse_def$lum_plnt[lum_plnt_miss],
                               collapse = ', '), '\n')
    } else {
      plnt_msg <- ''
    }
    if(any(lum_mgt_miss)) {
      sch_msg  <- paste0("'lum_mgt'  not defined in 'management.sch': ",
                         paste(luse_def$lum_mgt[lum_mgt_miss],
                               collapse = ', '), '\n')
    } else {
      sch_msg  <- ''
    }
    if(any(lum_cn2_miss)) {
      cn2_msg <- paste0("'lum_cn2' not defined in 'cntabe.lum': ",
                         paste(luse_def$lum_cn2[lum_cn2_miss],
                               collapse = ', '), '\n')
    } else {
      cn2_msg <- ''
    }
    if(any(lum_cpr_miss)) {
      cpr_msg  <- paste0("'lum_cpr' not defined in 'cons_practice.lum': ",
                         paste(luse_def$lum_cpr[lum_cpr_miss],
                               collapse = ', '), '\n')
    } else {
      cpr_msg  <- ''
    }
    if(any(lum_ovn_miss)) {
      ovn_msg  <- paste0("'lum_ovn' not defined in 'ovn_table.lum': ",
                         paste(luse_def$lum_ovn[lum_ovn_miss],
                               collapse = ', '), '\n')
    } else {
      ovn_msg  <- ''
    }

    stop('The following options are not defined in the respective SWAT+ input ',
         'files: \n\n',
         plnt_msg, sch_msg, cn2_msg, cpr_msg, ovn_msg,
         '\n\nPlease do the following to solve this issue:\n',
         'i)   Add the missing entries in the SWAT+ input files\n',
         "ii)  Reload all SWAT+ input files with measr_object$reload_swat_inputs()\n",
         '     (can only be done when no NSWRMs wer implemented yet)\n',
         "iii) Load again 'land_use' definition table with measr_object$load_nswrm_definition()")
  }

  return(luse_def)
}
