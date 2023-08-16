#' Update the landuse of selected HRUs
#'
#' The land use ('lu-mgt') in hru-data.hru of the land objects defined by the
#' `hru_id` is overwritten. The new land use label is defined by `type`. For
#' `type` a new land use is defined in landuse.lum if no land use with this name
#' already exists in landuse.lum which has the exact same parametrization as
#' defined with the additional input arguments. If an identical landuse already
#' exists, this landuse is assigned to the HRUs. If not a new one is added with
#' the defined parameters (`plnt_com`, `lum_mgt`, `lum_cn2`, `lum_cpr`, and
#' `lum_ovn`).
#'
#' @param swat_inputs List with SWAT+ input files.
#' @param hru_id HRU IDs for which the land use is updated.
#' @param type Type of the new land cover. This will be the new lan duse label
#'   in the updated landuse.lum. `type` must be a single character string
#' @param plnt_com The plant community which is assigned to the land use. A
#'   single character string must be provided. The passed value must be defined
#'   in the 'plant.ini' intput file. If (default) `NULL`, then 'plnt_com' will
#'   be set 'null' in the landuse.lum.
#' @param lum_mgt The management which is assigned to the land use. A single
#'   character string must be provided. The passed value must be defined in the
#'   'management.sch' intput file. If (default) `NULL`, then 'mgt' will be set
#'   'null' in the landuse.lum.
#' @param lum_cn2 The cn2 parameter set which is assigned to the land use. A
#'   single character string must be provided. The passed value must be defined
#'   in the 'cntabe.lum' intput file. If (default) `NULL`, then 'cn2' will be
#'   set 'null' in the landuse.lum.
#' @param lum_cpr The conservation practice which is assigned to the land use. A
#'   single character string must be provided. The passed value must be defined
#'   in the 'cons_practice.lum' intput file. If (default) `NULL`, then
#'   'cons_prac' will be set 'null' in the landuse.lum.
#' @param lum_ovn The Mannings n parametrization which is assigned to the land
#'   use. A single character string must be provided. The passed value must be
#'   defined in the 'ovn_table.lum' intput file. If (default) `NULL`, then
#'   'ov_mann' will be set 'null' in the landuse.lum.
#'
#' @returns The SWAT+ input tables list with the updated landuse.lum and
#'   hru-data.hru input tables.
#'
#' @importFrom dplyr select %>%
#' @importFrom purrr map map_df map_lgl
#' @importFrom stringr str_detect str_remove_all
#' @importFrom tibble add_row
#' @importFrom tidyr replace_na
#'
#' @export
#'
update_landuse <- function(swat_inputs, hru_id, type,
                           plnt_com = NULL, lum_mgt = NULL, lum_cn2  = NULL,
                           lum_cpr  = NULL, lum_ovn = NULL) {
  # General data type/structure checks
  stopifnot(is.numeric(hru_id))
  stopifnot(is.character(type))
  stopifnot(length(type) == 1)
  stopifnot(is.character(plnt_com) | is.null(plnt_com))
  stopifnot(length(plnt_com) <= 1)
  stopifnot(is.character(lum_mgt) | is.null(lum_mgt))
  stopifnot(length(lum_mgt) <= 1)
  stopifnot(is.character(lum_cn2) | is.null(lum_cn2))
  stopifnot(length(lum_cn2) <= 1)
  stopifnot(is.character(lum_cpr) | is.null(lum_cpr))
  stopifnot(length(lum_cpr) <= 1)
  stopifnot(is.character(lum_ovn) | is.null(lum_ovn))
  stopifnot(length(lum_ovn) <= 1)

  # Checks for all inputs if they are available in the respective SWAT+ input
  # files
  if (! is.null(plnt_com)) {
    if (!plnt_com %in% swat_inputs$plant.ini$pcom_name) {
      stop("The 'plnt_com' = '", plnt_com, "' does not exist in 'plant.ini'.\n",
           "Cannot update the land use for '", type, "' in the following HRUs:\n",
           paste(hru_id, collapse = ', '))
    }
  } else {
    plnt_com <- 'null'
  }
  if (! is.null(lum_mgt)) {
    if (!lum_mgt %in% swat_inputs$management.sch$name) {
      stop("The 'lum_mgt' = '", lum_mgt, "' does not exist in 'management.sch'.\n",
           "Cannot update the land use for '", type, "' in the following HRUs:\n",
           paste(hru_id, collapse = ', '))
    }
  } else {
    lum_mgt <- 'null'
  }
  if (! is.null(lum_cn2)) {
    if (!lum_cn2 %in% swat_inputs$cntabe.lum$name) {
      stop("The 'lum_cn2' = '", lum_cn2, "' does not exist in 'cntable.lum'.\n",
           "Cannot update the land use for '", type, "' in the following HRUs:\n",
           paste(hru_id, collapse = ', '))
    }
  } else {
    lum_cn2 <- 'null'
  }
  if (! is.null(lum_cpr)) {
    if (!lum_cpr %in% swat_inputs$cons_practice.lum$name) {
      stop("The 'lum_cn2' = '", lum_cn2, "' does not exist in 'cons_practice.lum'.\n",
           "Cannot update the land use for '", type, "' in the following HRUs:\n",
           paste(hru_id, collapse = ', '))
    }
  } else {
    lum_cpr <- 'null'
  }
  if (! is.null(lum_ovn)) {
    if (!lum_ovn %in% swat_inputs$ovn_table.lum$name) {
      stop("The 'lum_ovn' = '", lum_ovn, "' does not exist in 'ovn_table.lum'.\n",
           "Cannot update the land use for '", type, "' in the following HRUs:\n",
           paste(hru_id, collapse = ', '))
    }
  } else {
    lum_ovn <- 'null'
  }

  # Get all lum names from landuse.lum
  lum_names <- swat_inputs$landuse.lum$name
  # Find all alternative names with for the input argument 'type'
  lum_alt <- lum_names[str_detect(lum_names,
                                  paste0(type, '_lum|',
                                         type, '[:digit:]+', '_lum'))]

  # If type_lum or any of the alternatives are available check if their
  # parameters match. Otherwise generate a new landuse.lum entry
  if(length(lum_alt) > 0) {
    # Get all lines for the lum alternatives from landuse.lum
    lum_i <- swat_inputs$landuse.lum[swat_inputs$landuse.lum$name %in% lum_alt,]

    # Compare the parameters of the alternatives to the input arguments
    lum_upd_par   <- c(plnt_com, lum_mgt, lum_cn2, lum_cpr, lum_ovn)
    # Check if all are inputs are identical with the parameters in one of
    # the selected rows from landuse.lum
    lum_par_ident <- lum_i %>%
      split(., 1:nrow(.)) %>%
      map(., ~ select(.x, plnt_com, mgt, cn2, cons_prac, ov_mann)) %>%
      map(., ~ unlist(.x)) %>%
      map_lgl(., ~ all(.x == lum_upd_par))

    # If non of the landuse.lum lines do have idntical parameters to the input
    # arguments create a new entry in landuse.lum
    if (! any(lum_par_ident)) {
      # Identify maximum ID which was assigned to this type
      id_max <- lum_alt %>%
        str_remove_all(., paste0(type, '_lum')) %>%
        as.numeric(.) %>%
        c(0,.) %>%
        max(., na.rm = TRUE)

      # Create new lum label
      lum_lbl <- paste0(type, id_max+1, '_lum')

      # Add entry in landuse lum with the new name and parameters
      swat_inputs$landuse.lum <- swat_inputs$landuse.lum %>%
        add_row(., name = lum_lbl,
                   plnt_com = plnt_com,
                   mgt = lum_mgt,
                   cn2 = lum_cn2,
                   ov_mann = lum_ovn) %>%
      map_df(., ~replace_na(.x, 'null'))
      swat_inputs$file_updated['landuse.lum'] <- TRUE
    } else {
      # Use the first identical landuse to assign to the HRUs
      lum_lbl <- lum_alt[lum_par_ident][1]
    }
  } else {
    lum_lbl <- paste0(type, '_lum')
    swat_inputs$landuse.lum <- swat_inputs$landuse.lum %>%
      add_row(., name = lum_lbl,
              plnt_com = plnt_com,
              mgt = lum_mgt,
              cn2 = lum_cn2,
              ov_mann = lum_ovn) %>%
      map_df(., ~replace_na(.x, 'null'))
    swat_inputs$file_updated['landuse.lum'] <- TRUE
  }

  # Assign updated land use to the respective HRUs in hru-data.hru
  swat_inputs$hru_data.hru$lu_mgt[swat_inputs$hru_data.hru$id %in% hru_id] <-
    lum_lbl
  swat_inputs$file_updated['hru_data.hru'] <- TRUE

  return(swat_inputs)
}
