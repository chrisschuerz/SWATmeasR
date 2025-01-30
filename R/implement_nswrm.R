#' Implement NSWRMs in the SWAT+ model input tables.
#'
#' @param file_path Path to the '.csv' location file.
#' @param nswrm_defs List with already loaded NSWRM definition tables
#' @param swat_inputs List with SWAT+ input files.
#'
#' @returns The list of SWAT+ input tables with NSWRMs implemented in the
#'   respective input tables.
#'
#' @importFrom dplyr filter
#'
#' @keywords internal
#'
implement_nswrm <- function(nswrm_id, nswrm_defs, swat_inputs) {
  # Check if NSWRM locations were already loaded into the measr_project.
  if(!'nswrm_locations' %in% names(nswrm_defs)) {
    stop("The NSWRM locations must be loaded before you can implement them ",
         "in the SWAT+ model setup.\n",
         "Use 'measr_project$load_nswrm_location()' to load the locations file.")
  }

  # Filter all NSWRM locations with the IDs = nswrm_id
  nswrm_loc_sel <- filter(nswrm_defs$nswrm_locations, id %in% nswrm_id)

  # Implement NSWRMs of type 'management'------------------------------------
  # The vector includes all NSWRM types which are represented by a change in
  # management practices.
  nswrm_management <- unique(nswrm_loc_sel$nswrm[nswrm_loc_sel$type == 'management'])

  # Loop over all management types to perform the land use changes in the SWAT+
  # input tables.
  for (nswrm_i in nswrm_management) {
    hru_id <- unique(unlist(nswrm_loc_sel$obj_id[nswrm_loc_sel$nswrm == nswrm_i]))
    def_nswrm <- nswrm_defs$management[[nswrm_i]]
    swat_inputs <- update_management(swat_inputs,
                                     hru_id  = hru_id,
                                     mgt_def = def_nswrm)

    swat_inputs$implemented_nswrms <-
      update_implemented_nswrms(swat_inputs$implemented_nswrms,
                                nswrm_i = nswrm_i,
                                obj_typ_i = 'hru',
                                obj_id_i = hru_id,
                                obj_typ_upd_i = NA_character_,
                                obj_id_upd_i = NA_integer_)
  }
  # -------------------------------------------------------------------------

  # Implement NSWRMs of type 'land_use' change ------------------------------
  # The vector includes all NSWRM types which are represented by a land use
  # change
  nswrm_land_use <- unique(nswrm_loc_sel$nswrm[nswrm_loc_sel$type == 'land_use'])

  # Loop over all land use change types to perform the land use changes in the
  # SWAT+ input tables.
  for (nswrm_i in nswrm_land_use) {
    hru_id <- unique(unlist(nswrm_loc_sel$obj_id[nswrm_loc_sel$nswrm == nswrm_i]))
    def_nswrm <- filter(nswrm_defs$land_use, nswrm == nswrm_i)
    swat_inputs <- update_landuse(swat_inputs,
                                  hru_id   = hru_id,
                                  nswrm    = def_nswrm$nswrm,
                                  lum_plnt = def_nswrm$plnt_com,
                                  lum_mgt  = def_nswrm$mgt,
                                  lum_cn2  = def_nswrm$cn2,
                                  lum_cpr  = def_nswrm$cons_prac,
                                  lum_ovn  = def_nswrm$ov_mann,
                                  lum_tile = def_nswrm$tile,
                                  lum_grww = def_nswrm$grww,
                                  lum_vfs = def_nswrm$vfs,
                                  lum_bmp = def_nswrm$bmp)

    if(!def_nswrm$lum_dtl %in% c('::keep::', 'null')) {
      swat_inputs <- add_dtl_op(swat_inputs, hru_id, def_nswrm$lum_dtl)
    }

    swat_inputs$implemented_nswrms <-
      update_implemented_nswrms(swat_inputs$implemented_nswrms,
                                nswrm_i = nswrm_i,
                                obj_typ_i = 'hru',
                                obj_id_i = hru_id,
                                obj_typ_upd_i = NA_character_,
                                obj_id_upd_i = NA_integer_)
    }
  # -------------------------------------------------------------------------

  # Add wetland water storage to land objects -------------------------------
  wet_loc_sel <- filter(nswrm_loc_sel, type == 'wetland')
  if(nrow(wet_loc_sel) > 0) {
    wet_def_sel <- filter(nswrm_defs$wetland, hru_id %in% unlist(wet_loc_sel$obj_id))
    wet_wet_sel  <- select(wet_def_sel, rel:nut)
    hyd_wet_sel  <- select(wet_def_sel, hru_ps:hru_frac)

    swat_inputs <- implement_wetlands(swat_inputs,
                                      hru_id      = wet_def_sel$hru_id,
                                      to_cha_id   = wet_def_sel$cha_to_id,
                                      lu_mgt_sel  = wet_def_sel$lu_mgt,
                                      wet_wet_sel = wet_wet_sel,
                                      hyd_wet_sel = hyd_wet_sel)

    swat_inputs$implemented_nswrms <-
      update_implemented_nswrms(swat_inputs$implemented_nswrms,
                                nswrm_i = 'wetland',
                                obj_typ_i = 'hru',
                                obj_id_i = wet_def_sel$hru_id,
                                obj_typ_upd_i = NA_character_,
                                obj_id_upd_i = NA_integer_)
  }
  # -------------------------------------------------------------------------

  # Replace land objects with constructed wetland objects -------------------
  cwtl_loc_sel <- filter(nswrm_loc_sel, type == 'constr_wetland')
  if(nrow(cwtl_loc_sel) > 0) {
    cwtl_def_match <- map_lgl(nswrm_defs$constr_wetland$hru_id,
                              ~ match_ids(cwtl_loc_sel$obj_id, .x))
    cwtl_def_sel <- nswrm_defs$constr_wetl[cwtl_def_match,]
    res_res_sel  <- select(cwtl_def_sel, rel:nut)
    hyd_res_sel  <- select(cwtl_def_sel, area_ps:shp_co2)

    swat_inputs <- replace_by_ponds(swat_inputs,
                                    hru_id      = cwtl_def_sel$hru_id,
                                    to_cha_id   = cwtl_def_sel$cha_to_id,
                                    from_cha_id = cwtl_def_sel$cha_from_id,
                                    res_res_pnd = res_res_sel,
                                    hyd_res_pnd = hyd_res_sel,
                                    type = 'constr_wetland')

    ids <- link_hru_res_ids(cwtl_def_sel$hru_id, swat_inputs$reservoir.res, 'cwl')

    swat_inputs$implemented_nswrms <-
      update_implemented_nswrms(swat_inputs$implemented_nswrms,
                                nswrm_i = 'constr_wetland',
                                obj_typ_i = 'hru',
                                obj_id_i = ids$hru_ids,
                                obj_typ_upd_i = 'res',
                                obj_id_upd_i = ids$res_ids)
  }
  # -------------------------------------------------------------------------

  # Replace land objects with pond objects ----------------------------------
  pond_loc_sel <- filter(nswrm_loc_sel, type == 'pond')
  if(nrow(pond_loc_sel) > 0) {
    pond_def_match <- map_lgl(nswrm_defs$pond$hru_id,
                              ~ match_ids(pond_loc_sel$obj_id, .x))
    pond_def_sel <- nswrm_defs$pond[pond_def_match,]
    res_res_sel  <- select(pond_def_sel, rel:nut)
    hyd_res_sel  <- select(pond_def_sel, area_ps:shp_co2)

    swat_inputs <- replace_by_ponds(swat_inputs,
                                    hru_id      = pond_def_sel$hru_id,
                                    to_cha_id   = pond_def_sel$cha_to_id,
                                    from_cha_id = pond_def_sel$cha_from_id,
                                    res_res_pnd = res_res_sel,
                                    hyd_res_pnd = hyd_res_sel,
                                    type = 'pond')

    ids <- link_hru_res_ids(pond_def_sel$hru_id, swat_inputs$reservoir.res, 'pnd')

    swat_inputs$implemented_nswrms <-
      update_implemented_nswrms(swat_inputs$implemented_nswrms,
                                nswrm_i = 'pond',
                                obj_typ_i = 'hru',
                                obj_id_i = ids$hru_ids,
                                obj_typ_upd_i = 'res',
                                obj_id_upd_i = ids$res_ids)
  }
  # -------------------------------------------------------------------------

  return(swat_inputs)
}

#' Update the table implemented_nswrms which tracks the objects (currently HRUs)
#' in which measures were implemented.
#'
#' @param tbl Existing implemented_nswrms table (if not existing this value is NULL)
#' @param nswrm_i String type of the implemented measure.
#' @param obj_typ_i String giving the type of spatial objects in which a measure
#'   is implemented (e.g. 'hru').
#' @param obj_id_i Vector of integer ID values of the objects in which the
#'   measure is implemented.
#' @param obj_typ_upd_i  String giving the type of spatial object which replaces
#'   the existing spatial objects with the measure implementation (e.g. 'res' in
#'   case of a pond).
#' @param obj_id_upd_i Vector of integer ID values of the new objects which
#'   replaced the initial spatial objects (e.g. res IDs of new reservoirs).
#'
#' @returns A tibble with the columns `nswrm` for the NSWRM types, `obj_typ` with
#'   the type of spatial objects, `obj_id` with the IDs of the spatial objects
#'   in which a measure was implemented, `obj_typ_new` with the type of the new
#'   spatial objects, and `obj_id_new` with the IDs of the new objects.
#'
#' @importFrom dplyr filter
#' @importFrom tibble add_row tibble
#'
#' @keywords internal
#'
update_implemented_nswrms <- function(tbl, nswrm_i, obj_typ_i, obj_id_i,
                                      obj_typ_upd_i, obj_id_upd_i) {
  if(is.null(tbl)) {
    tbl <- tibble(nswrm = character(),
                  obj_typ = character(),
                  obj_id = integer(),
                  obj_typ_new = character(),
                  obj_id_new = integer())
  }

  tbl <- filter(tbl, !(obj_id %in% obj_id_i & obj_typ == obj_typ_i))

  tbl <- add_row(tbl,
                 nswrm = nswrm_i,
                 obj_id = obj_id_i,
                 obj_typ = obj_typ_i,
                 obj_id_new = obj_id_upd_i,
                 obj_typ_new = obj_typ_upd_i
                 )

  return(tbl)
}

#' Link the HRU IDs which were replaced by a pond or a constructed wetland with
#' the IDs of the new objects in the reservoir.res input file.
#'
#' @param hru_ids List of vectors of HRU IDs
#' @param res_res Updated reservoir.res SWAT+ input table
#' @param type Type of measures (either `'pnd'` or `'cwl'`)
#'
#' @returns A list with the vectors of `hru_ids` and corresponding `res_ids`.
#'   The vectors have the same length,
#'
#' @importFrom purrr map_chr map2
#'
#' @keywords internal
#'
link_hru_res_ids <- function(hru_ids, res_res, type) {
  res_lbls <- map_chr(hru_ids, ~ paste0(type, .x[1]))
  res_ids <- res_res$id[match(res_lbls, res_res$name)]
  res_ids <- map2(res_ids, hru_ids, ~ rep(.x, length(.y)))
  return(list(hru_ids = unlist(hru_ids),
              res_ids = unlist(res_ids)))
}
