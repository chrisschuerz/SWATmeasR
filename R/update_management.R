#' Update the management inputs for selected HRUs
#'
#' @param swat_inputs List with SWAT+ input files.
#' @param hru_id HRU IDs for which the land use is updated.
#' @param mgt_def List of management input files which define the management
#'   scenario.
#'
#' @returns The SWAT+ input tables list with the updated hru-data.hru,
#'   landuse.lum, management.sch, and plant.ini input tables.
#'
#' @importFrom dplyr bind_rows
#'
#' @keywords internal
#'
update_management <- function(swat_inputs, hru_id, mgt_def) {
  lum_mgt_sel <- mgt_def$hru_data.hru$lu_mgt[mgt_def$hru_data.hru$id %in% hru_id]

  swat_inputs$hru_data.hru$lu_mgt[swat_inputs$hru_data.hru$id %in% hru_id] <-
    lum_mgt_sel

  lum_add <- filter(mgt_def$landuse.lum, name %in% lum_mgt_sel)
  mgt_add <- filter(mgt_def$management.sch, name %in% lum_add$mgt)
  ini_add <- filter(mgt_def$plant.ini, pcom_name %in% lum_add$plnt_com)

  swat_inputs$landuse.lum <- bind_rows(swat_inputs$landuse.lum, lum_add)
  swat_inputs$management.sch <- bind_rows(swat_inputs$management.sch,
                                          mgt_add)
  swat_inputs$plant.ini <- bind_rows(swat_inputs$plant.ini, ini_add)

  # Set the input files which are adjusted by management related changes
  # so that they will be written when writing output files.
  swat_inputs$file_updated[c('hru_data.hru', 'landuse.lum', 'management.sch',
                             'plant.ini')] <- TRUE

  return(swat_inputs)
}


