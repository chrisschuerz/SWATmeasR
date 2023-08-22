#' Implement NSWRMs in the SWAT+ model input tables.
#'
#' @param file_path Path to the '.csv' location file.
#' @param nswrm_defs List with already loaded NSWRM definition tables
#' @param swat_inputs List with SWAT+ input files.
#' @param reset Reset existing NSWRM implementations? Default is `FALSE`. If
#'   measures were already implemented the SWAT+ input tables must be reset,
#'   before implementing a new set of measures. If `TRUE` the SWAT+ input tables
#'   will be reset before implementing a new set of measures.
#'
#' @returns The list of SWAT+ input tables with NSWRMs implemented in the
#'   respective input tables.
#'
#' @importFrom dplyr filter
#'
#' @keywords internal
#'
implement_nswrm <- function(nswrm_id, nswrm_defs, swat_inputs, overwrite) {

  # Filter all NSWRM locations with the IDs = nswrm_id
  nswrm_loc_sel <- filter(nswrm_defs$nswrm_locations, id %in% nswrm_id)

  # Implement NSWRMs of type 'land_use' change ------------------------------
  # The vector includes all NSWRM types which are represented by a land use
  # change
  nswrm_land_use <- unique(nswrm_loc_sel$nswrm[nswrm_loc_sel$type == 'land_use'])

  # Loop over all land use change types to perform the land use changes in the
  # SWAT+ input tables.
  for (nswrm_i in nswrm_land_use) {
    id_nswrm  <- nswrm_loc_sel$id[nswrm_loc_sel$nswrm == nswrm_i]
    def_nswrm <- filter(nswrm_defs$land_use, nswrm == nswrm_i)
    swat_inputs <- update_landuse(swat_inputs,
                                  hru_id   = id_nswrm,
                                  nswrm    = def_nswrm$nswrm,
                                  lum_plnt = def_nswrm$lum_plnt,
                                  lum_mgt  = def_nswrm$lum_mgt,
                                  lum_cn2  = def_nswrm$lum_cn2,
                                  lum_cpr  = def_nswrm$lum_cpr,
                                  lum_ovn  = def_nswrm$lum_ovn)
  }
  # -------------------------------------------------------------------------

  # Replace land objects with pond objects ----------------------------------
  pond_loc_sel <- filter(nswrm_loc_sel, type == 'pond')
  pond_def_sel <- filter(nswrm_defs$pond, hru_id %in% unlist(pond_loc_sel$obj_id))

  swat_inputs <- replace_by_ponds(swat_inputs,
                                  hru_id = pond_def_sel$hru_id,
                                  to_cha_id = pond_def_sel$cha_to_id,
                                  from_cha_id    = pond_def_sel$cha_from_id)
  # -------------------------------------------------------------------------

  return(swat_inputs)
}
