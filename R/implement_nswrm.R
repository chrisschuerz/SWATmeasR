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
  luse_chg_types <- c('buffer', 'edgefilter', 'hedge',
                      'grassslope', 'grassrchrg', 'afforest')

  # Loop over all land use change types to perform the land use changes in the
  # SWAT+ input tables.
  for (type_i in luse_chg_types) {
    swat_inputs <- update_luse_by_type(type_i, nswrm_loc_sel,
                                       nswrm_defs, swat_inputs)
  }

  # Replace land objects with pond objects ----------------------------------
  pond_loc_sel <- filter(nswrm_loc_sel, type == 'pond')
  pond_def_sel <- filter(nswrm_defs$pond, hru_id %in% unlist(pond_loc_sel$obj_id))

  swat_inputs <- replace_by_ponds(swat_inputs,
                                  hru_id = pond_def_sel$hru_id,
                                  to_cha_id = pond_def_sel$cha_to_id,
                                  from_cha_id    = pond_def_sel$cha_from_id)

  return(swat_inputs)
}

#' Title
#'
#' @param type_i
#' @param nswrm_loc_sel
#' @param nswrm_defs
#' @param swat_inputs
#'
#' @returns
#'
#' @importFrom dplyr filter
#'
#' @keywords internal
#'
update_luse_by_type <- function(type_i, nswrm_loc_sel, nswrm_defs, swat_inputs) {
  if(type_i %in% nswrm_loc_sel$type) {
    id_type  <- nswrm_loc_sel$id[nswrm_loc_sel$type == type_i]
    def_type <- filter(nswrm_defs$land_use, type == type_i)
    swat_inputs <- update_landuse(swat_inputs,
                                  hru_id   = id_type,
                                  type     = def_type$type,
                                  lum_plnt = def_type$lum_plnt,
                                  lum_mgt  = def_type$lum_mgt,
                                  lum_cn2  = def_type$lum_cn2,
                                  lum_cpr  = def_type$lum_cpr,
                                  lum_ovn  = def_type$lum_ovn)
  }

  return(swat_inputs)
}
