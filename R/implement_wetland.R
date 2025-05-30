#' Implement wetlands by adding surface storage to land objects and removing
#' existing drainage.
#'
#' Surface storage is added to the land objects defined by the `hru_id`, by
#' parameterizing a wetland in `wetland.wet` and `hydrology.wet` and adding this
#' wetland as surface storage in `hru-data.hru`. If an HRU was drained, the
#' drainage is removed and a new entry in `landuse.lum` is generated for that
#' HRU. Optionally, a `to_cha_id` can be defined if water from wetlands should be
#' routed directly into a channel.
#'
#' @param swat_inputs List with SWAT+ input files.
#'
#' @param hru_id HRU IDs to which surface water storage (wetland) will be added.
#'
#'   The input can be a single numeric value to replace a single HRU, or a
#'   vector of numeric values if multiple HRUs are modified.
#'
#' @param to_cha_id (Optional) channel IDs into which water from wetland is
#'   routed. (all water from that HRU is then routed into the defined channel
#'   instead of the initially defined neighouring hydrological objects)
#'
#'   The input can be a single numeric value or a vector of values. `to_cha_id`
#'   must be the same length as `hru_id`. Each channel ID corresponds to the
#'   respective `hru_id`.
#'
#' @param lu_mgt_sel Selected land use which should be employed for the wetland.
#'   (User defined in the wetlands definition table or NA if not set)
#'
#' @param wet_wet_sel Table with rel, sed and nut pointers for the added
#'   wetlands. The pointers will be written into wetlands.wet
#'
#' @param hyd_wet_sel Table with hydrology.wet parameters for the implemented
#'   wetlands.
#'
#' @returns The `swat_inputs` list with updated input tables which include all
#'   necessary changes to add surface water storage to HRUs and remove tile
#'   flow.
#'
#' @keywords internal
#'
implement_wetlands <- function(swat_inputs, hru_id, to_cha_id,
                               lu_mgt_sel, wet_wet_sel, hyd_wet_sel) {
  # Check if an HRU is already a wetland
  is_wetl <- check_is_wetland(swat_inputs, hru_id)

  # Exclude that HRUs/channels from the ones which will be replaced/modified/
  hru_id    <- hru_id[!is_wetl]
  to_cha_id <- to_cha_id[!is_wetl]

  # If there are HRUs remaining where wetlands should be added loop over all
  # land objects and update the respective input files.
  if(length(hru_id) > 0) {
    for (i in 1:length(hru_id)) {
      hru_i <- hru_id[i]
      # Generate a character string for the HRU ID for naming in the input files.
      hru_i_chr <- add_lead_zeros(hru_i, swat_inputs$hru_data.hru$id)

      to_cha_i   <- to_cha_id[i]

      wet_wet_i <- wet_wet_sel[i, ]
      hyd_wet_i <- hyd_wet_sel[i, ]
      lu_mgt_i <- lu_mgt_sel[i]

      # Update the wetland.wet input file by adding the new wetland for hru_i
      swat_inputs$wetland.wet   <- update_wet_wet(swat_inputs$wetland.wet,
                                                  wet_wet_i,
                                                  hru_i_chr)
      # Update hydrology.wet by adding the parameters for the new wetland in hru_i
      swat_inputs$hydrology.wet <- update_hyd_wet(swat_inputs$hydrology.wet,
                                                  hyd_wet_i,
                                                  hru_i_chr)

      has_drn <- is_hru_drained(swat_inputs, hru_i)

      # Copy and rename the landuse.lum definition of hru_i and remove any tile.
      swat_inputs$landuse.lum   <- update_lum_wetl(swat_inputs$landuse.lum,
                                                   swat_inputs$hru_data.hru,
                                                   lu_mgt_i,
                                                   hru_i,
                                                   hru_id_chr)
      # Update hru-data.hru by adding the surface storage and updating lu_mgt.
      lum_name_i <- swat_inputs$landuse.lum$name[nrow(swat_inputs$landuse.lum)]
      swat_inputs$hru_data.hru  <- update_hru_hru_wetl(swat_inputs$hru_data.hru,
                                                       lum_name_i,
                                                       hru_i,
                                                       hru_i_chr)
    if(!is.na(to_cha_i) | has_drn) {
      swat_inputs$file_updated['rout_unit.con'] <- TRUE
      swat_inputs$rout_unit.con <- update_rtu_con_wetl(swat_inputs$rout_unit.con,
                                                       to_cha_i,
                                                       has_drn,
                                                       hru_i)
    }
    }
    # Set the input files which are adjusted by pond replacement to 'modified'
    # so that they will be written when writing output files.
    file_upd <- c('wetland.wet', 'hydrology.wet', 'landuse.lum', 'hru_data.hru')
    swat_inputs$file_updated[file_upd] <- TRUE
  }

  return(swat_inputs)
}

#' Implement end of field wetlands.
#'
#' Surface storage is added to the land objects defined by the `hru_id`, by
#' parameterizing a wetland in `wetland.wet` and `hydrology.wet` and adding this
#' wetland as surface storage in `hru-data.hru`. If an HRU was drained, the
#' drainage is removed and a new entry in `landuse.lum` is generated for that
#' HRU. Optionally, a `to_cha_id` can be defined if water from wetlands should be
#' routed directly into a channel.
#'
#' @param swat_inputs List with SWAT+ input files.
#'
#' @param hru_id HRU IDs to which surface water storage (wetland) will be added.
#'
#'   The input can be a single numeric value to replace a single HRU, or a
#'   vector of numeric values if multiple HRUs are modified.
#'
#' @param to_cha_id (Optional) channel IDs into which water from wetland is
#'   routed. (all water from that HRU is then routed into the defined channel
#'   instead of the initially defined neighouring hydrological objects)
#'
#'   The input can be a single numeric value or a vector of values. `to_cha_id`
#'   must be the same length as `hru_id`. Each channel ID corresponds to the
#'   respective `hru_id`.
#'
#' @param lu_mgt_sel Selected land use which should be employed for the wetland.
#'   (User defined in the wetlands definition table or NA if not set)
#'
#' @param wet_wet_sel Table with rel, sed and nut pointers for the added
#'   wetlands. The pointers will be written into wetlands.wet
#'
#' @param hyd_wet_sel Table with hydrology.wet parameters for the implemented
#'   wetlands.
#'
#' @returns The `swat_inputs` list with updated input tables which include all
#'   necessary changes to add surface water storage to HRUs and remove tile
#'   flow.
#'
#' @keywords internal
#'
implement_eofwetl <- function(swat_inputs, hru_id,
                              lu_mgt_sel, wet_wet_sel, hyd_wet_sel) {
  # Check if an HRU is already a wetland
  is_wetl <- check_is_wetland(swat_inputs, hru_id)

  hru_names <- swat_inputs$hru_data.hru$name[swat_inputs$hru_data.hru$id %in% hru_id]
  eof_names <- str_replace(hru_names, 'hru', 'eof')
  is_eof <- eof_names %in% swat_inputs$hru_data.hru$name

  # Exclude that HRUs/channels from the ones which will be replaced/modified/
  hru_id    <- hru_id[!(is_wetl | is_eof)]

  # If there are HRUs remaining where wetlands should be added loop over all
  # land objects and update the respective input files.
  if(length(hru_id) > 0) {
      # Generate a character string for the HRU ID for naming in the input files.
      hru_id_chr <- swat_inputs$hru_data.hru %>%
        filter(id %in% hru_id) %>%
        .$name %>%
        str_remove(., 'hru')
      # hru_id_chr <- add_lead_zeros(hru_id, swat_inputs$hru_data.hru$id)

      # Update hru-data.hru by adding the surface storage and updating lu_mgt.
      # lum_name_i <- swat_inputs$landuse.lum$name[nrow(swat_inputs$landuse.lum)]
      swat_inputs$hru_data.hru   <- update_hru_hru_eof(swat_inputs$hru_data.hru,
                                                       hru_id,
                                                       lu_mgt_sel)

      swat_inputs$hru.con        <- update_hru_con_eof(swat_inputs$hru.con,
                                                            hru_id,
                                                            hyd_wet_sel$hru_frac)

      swat_inputs$hydrology.wet  <- update_hyd_wet_eof(swat_inputs$hydrology.wet,
                                                       hru_id_chr,
                                                       hyd_wet_sel)

      swat_inputs$wetland.wet    <- update_wet_wet_eof(swat_inputs$wetland.wet,
                                                       hru_id_chr,
                                                       wet_wet_sel)

      hru_id_eof <- swat_inputs$hru_data.hru %>%
        filter(name %in% paste0('eof', hru_id_chr)) %>%
        mutate(id_init = hru_id) %>%
        select(id, id_init)

      swat_inputs$rout_unit.def  <- update_def_eof(swat_inputs$rout_unit.def,
                                                   hru_id_eof)
      swat_inputs$ls_unit.def    <- update_def_eof(swat_inputs$ls_unit.def,
                                                   hru_id_eof)
      swat_inputs$rout_unit.ele  <- update_ele_eof(swat_inputs$rout_unit.ele,
                                                   hru_id_chr,
                                                   hyd_wet_sel)
      swat_inputs$ls_unit.ele    <- update_ele_eof(swat_inputs$ls_unit.ele,
                                                   hru_id_chr,
                                                   hyd_wet_sel)

      swat_inputs$object.cnt     <- update_obj_cnt_eof(swat_inputs$object.cnt,
                                                       length(hru_id))

    # Set the input files which are adjusted by pond replacement to 'modified'
    # so that they will be written when writing output files.
    file_upd <- c('hru_data.hru', 'hru.con', 'hydrology.wet', 'wetland.wet',
                  'rout_unit.def', 'rout_unit.ele', 'ls_unit.def', 'ls_unit.ele',
                  'object.cnt')
    swat_inputs$file_updated[file_upd] <- TRUE
  }

  return(swat_inputs)
}


#' Check if HRUs are already wetlands..
#'
#' @param swat_inputs List with SWAT+ input files.
#' @param hru_id HRU IDs to which surface water storage (wetland) will be added.
#'
#' @returns A boolean vector to indicate which HRUs are already wetlands.
#'
#' @importFrom dplyr filter %>%
#'
#' @keywords internal
#'
check_is_wetland <- function(swat_inputs, hru_id) {
  wet_ids <- filter(swat_inputs$hru_data.hru, surf_stor != 'null') %>%
    .$id
  is_wet <- hru_id %in% wet_ids
  if (any(is_wet)) {
    warning('The HRUs with the IDs ', paste(hru_id[is_wet], collapse = ', '),
            ' are already wetlands and will be skipped.')
  }

  return(is_wet)
}

#' Check if an HRU has tile drainage implemented or not.
#'
#' @param swat_inputs List with SWAT+ input files.
#' @param hru_id HRU IDs to which surface water storage (wetland) will be added.
#'
#' @returns A boolean vector to indicate which HRUs are drained.
#'
#' @keywords internal
#'
is_hru_drained <- function(swat_inputs, hru_id) {
  lum_i <- swat_inputs$hru_data.hru$lu_mgt[swat_inputs$hru_data.hru$id == hru_id]
  swat_inputs$landuse.lum$tile[swat_inputs$landuse.lum$name == lum_i] != 'null'
}

#' Update the wetland.wet input table.
#'
#' Add a new line in the wetland.wet table. The name of the added entry is 'wet'
#' and the HRU ID. The wetland is initialized with the default 'initwet1'. The
#' pointer to the right entry in hydrology.wet is initialized ('hydwet' + HRU
#' ID). The (user defined) pointers to release, sediments, and nutrients are
#' added.
#'
#' @param wet_wet wetland.wet input table
#' @param wet_wet_i Table with rel, sed, and nut pointers for added pond.
#' @param hru_id_chr HRU IDs to which surface water storage (wetland) will be
#'   added. The input is character with leading zeros.
#'
#' @returns Updated wetland.wet input table.
#'
#' @importFrom dplyr bind_cols bind_rows %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
update_wet_wet <- function(wet_wet, wet_wet_i, hru_id_chr) {
  wet_add <- tibble(id   = max(wet_wet$id, 0) + 1,
                    name = paste0('wet', hru_id_chr),
                    init = wet_wet_i$init,
                    hyd  = paste0('hydwet', hru_id_chr),
                    rel  = wet_wet_i$rel,
                    sed  = wet_wet_i$sed,
                    nut  = wet_wet_i$nut)

  wet_wet <- bind_rows(wet_wet, wet_add)

  return(wet_wet)
}

#' Update the hydrology.wet input table.
#'
#' Hydrology parameters for the implemented water wetland water storage are
#' added to hydrology.hyd. The added parameters are either user defined
#' (provided with the wetlands definition file) or default if not provided (same
#' values as set default by SWAT+Editor).
#'
#' @param hyd_wet hydrology.wet input table.
#' @param hyd_wet_i hydrology.wet parameters table for added surface water
#'   storage.
#' @param hru_id_chr HRU IDs to which surface water storage (wetland) will be
#'   added. The input is character with leading zeros.
#'
#' @returns Updated hydrology.wet input table.
#'
#' @importFrom dplyr bind_cols bind_rows %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
update_hyd_wet <- function(hyd_wet, hyd_wet_i, hru_id_chr) {
  hyd_add <- tibble(name = paste0('hydwet', hru_id_chr)) %>%
    bind_cols(., hyd_wet_i)

  hyd_wet <- bind_rows(hyd_wet, hyd_add)

  return(hyd_wet)
}

#' Update the landuse.lum input table.
#'
#' By default drainage is removed for HRUs where wetlands are implemented. If
#' the landuse of a transformed HRU has tile drainage, a new landuse.lum entry
#' is generated (if it does not already exist). The landuse which is used by the
#' wetland HRU is always moved to the bottom of the landuse.lum table ( makes it
#' easier for following functions to identify the employed land use).
#'
#' @param luse_lum landuse.lum input table.
#' @param hru_hru hru-data.hru input table.
#' @param lu_mgt_sel Selected land use which should be employed for the wetland.
#'   (User defined in the wetlands definition table or NA if not set)
#' @param hru_id HRU IDs to which surface water storage (wetland) will be added.
#' @param hru_id_chr HRU IDs to which surface water storage (wetland) will be
#'   added. The input is character with leading zeros.
#'
#' @returns Updated landuse.lum input table.
#'
#' @importFrom dplyr bind_rows filter mutate %>%
#' @importFrom stringr str_remove
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
update_lum_wetl <- function(luse_lum, hru_hru, lu_mgt_sel, hru_id, hru_id_chr) {
  if(is.na(lu_mgt_sel)) {
    lu_mgt_sel <- hru_hru$lu_mgt[hru_hru$id == hru_id]
  }
  lum_i_name <- lu_mgt_sel %>%
    str_remove(., '_lum$') %>%
    str_remove(., '_drn$') %>%
    paste0(., '_wet')

  if(lum_i_name %in% luse_lum$name) {
    id_i <- which(luse_lum$name == lum_i_name)
    ids  <- 1:nrow(luse_lum)
    luse_lum <- luse_lum[c(ids[ids!=id_i], id_i), ]
  } else {
    lum_i_upd <- luse_lum %>%
      filter(., name == lu_mgt_sel) %>%
      mutate(.,
             tile = 'null',
             name = lum_i_name)
    luse_lum <- bind_rows(luse_lum, lum_i_upd)
  }

  return(luse_lum)
}

#' Update the hru-data.hru input table.
#'
#' The new landuse pointer to the entry in landuse.lum and the pointer to the
#' pointer to the now surface storage in wetland.wet are added to the HRU
#'
#' @param hru_hru hru-data.hru input table.
#' @param lum_i_name Name of the land use in landuse.lum which is implemented
#'   for the wetland.
#' @param hru_id HRU IDs to which surface water storage (wetland) will be added.
#' @param hru_id_chr HRU IDs to which surface water storage (wetland) will be
#'   added. The input is character with leading zeros.
#'
#' @returns Updated hru-data.hru input table.
#'
#' @keywords internal
#'
update_hru_hru_wetl <- function(hru_hru, lum_i_name, hru_id, hru_id_chr) {
  hru_hru$surf_stor[hru_hru$id == hru_id] <- paste0('wet', hru_id_chr)
  hru_hru$lu_mgt[hru_hru$id == hru_id] <- lum_i_name
  return(hru_hru)
}

#' Update the rout_unit.con input table.
#'
#' If the new wetland should route directly into a channel instead of routing
#' into its neighboring spatial object, or if the HRU has tile drainage which
#' should be removed then the rout_unit.con is updated accordingly. In the case
#' of routing into a channel all connections (except aquifer recharge), are
#' removed for the wetland HRU and are replaced by a connection to the channel
#' with the ID `to_cha_id`. In the case of tile drainage only the tile flow
#' connectivity is removed.
#'
#' @param rtu_con rout_unit.con input table.
#' @param to_cha_id ID of the channel into which all water should be routed.
#' @param has_drn Boolean. Does the HRU have tile drainage or not?
#' @param hru_id HRU IDs to which surface water storage (wetland) will be added.
#'
#' @returns Updated rout_unit.con table.
#'
#' @keywords internal
#'
update_rtu_con_wetl <- function(rtu_con, to_cha_id, has_drn, hru_id) {
  if(!is.na(to_cha_id)) {
    rtu_con <- remove_connection(rtu_con, hru_id,
                                 hyd_typ_rmv = c('tot', 'sur', 'lat', 'til'))
    rtu_con <- add_connection(rtu_con, hru_id,
                              obj_typ_add = 'sdc', obj_id_add = to_cha_id,
                              hyd_typ_add = 'tot', frac_add = 1)
  } else if(has_drn) {
    rtu_con <- remove_connection(rtu_con, hru_id, hyd_typ_rmv = 'til')
  }

  return(rtu_con)
}

#' Remove connections from a connectivity input table.
#'
#' Remove all connections which route either into a certain object type
#' (`obj_typ_rmv`) or which are of a certain hydrological type (`hyd_typ_rmv`).
#' The connections are removed for the entry with the ID `id_i`.
#'
#' @param obj_con The object connectivity input table
#' @param id_i Object ID for which connections should be removed.
#' @param obj_typ_rmv Vector of object types into which the object with `id_i`
#'   routes and which should be removed.
#' @param hyd_typ_rmv Vector of hydrological flow types into the object with
#'   `id_i` routes and which should be removed.
#'
#' @returns The updated connectivity input table.
#'
#' @importFrom dplyr bind_cols filter group_by mutate select ungroup %>%
#' @importFrom tibble add_row
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @keywords internal
#'
remove_connection <- function(obj_con, id_i,
                              obj_typ_rmv = NULL, hyd_typ_rmv = NULL) {
  obj_i <- obj_con %>%
    filter(id == id_i) %>%
    select(id:out_tot)

  cons_i <- obj_con %>%
    filter(id == id_i) %>%
    select(obj_typ_1:ncol(.)) %>%
    pivot_longer(.,
                 cols = obj_typ_1:ncol(.),
                 names_to = c('.value', 'i'),
                 names_pattern = "(.*)_(.*)")

  n_el <- nrow(cons_i)

  if (!is.null(obj_typ_rmv)) {
    cons_i <- filter(cons_i, !obj_typ %in% obj_typ_rmv)
  }
  if (!is.null(hyd_typ_rmv)) {
    cons_i <- filter(cons_i, !hyd_typ %in% hyd_typ_rmv)
  }

  n_rmv <- n_el - nrow(cons_i)

  cons_i <- cons_i %>%
    add_row(., obj_typ = rep('', n_rmv), hyd_typ = rep('', n_rmv)) %>%
    mutate(., i = 1:nrow(.)) %>%
    group_by(hyd_typ) %>%
    mutate(frac = frac/sum(frac)) %>%
    ungroup(.) %>%
    pivot_wider(.,
                names_from = i,
                values_from = c('obj_typ', 'obj_id', 'hyd_typ', 'frac'),
                names_sep = '_') %>%
    select(., paste0(c('obj_typ_', 'obj_id_', 'hyd_typ_', 'frac_'),
                     rep(1:n_el, each = 4)))
  obj_con_i <- bind_cols(obj_i, cons_i) %>%
    mutate(out_tot = out_tot - n_rmv)

  obj_con[id_i,] <- obj_con_i

  return(obj_con)
}

#' Add a connection to a connectivity input table.
#'
#' @param obj_con The object connectivity input table
#' @param obj_typ_add Object type of the newly added connection
#' @param obj_id_add Object id into which the newly added connection is routed.
#' @param hyd_typ_add Type of hydrological flux of the newly added connection.
#' @param frac_add Flow fraction of the newly added connection (will be
#'   normalized to 1 in case the fractions sum with existing fluxes do not add
#'   up to 1).
#' @param position Position in the input table where the new connection is
#'   added.
#'
#' @returns The updated connectivity input table.
#'
#' @importFrom dplyr bind_cols filter group_by mutate select ungroup %>%
#' @importFrom tibble add_row
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @keywords internal
#'
add_connection <- function(obj_con, id_i, obj_typ_add, obj_id_add, hyd_typ_add,
                           frac_add, position = Inf) {
  obj_i <- obj_con %>%
    filter(id == id_i) %>%
    select(id:out_tot)

  cons_i <- obj_con %>%
    filter(id == id_i) %>%
    select(obj_typ_1:ncol(.)) %>%
    pivot_longer(.,
                 cols = obj_typ_1:ncol(.),
                 names_to = c('.value', 'i'),
                 names_pattern = "(.*)_(.*)") %>%
    mutate(i = as.integer(i))

  n_el <- nrow(cons_i)
  cons_i <- filter(cons_i, obj_typ != '')
  position <- max(min(position, nrow(cons_i)), 1)

  n_na <- n_el - 1 - nrow(cons_i)

  if (n_na >= 0) {
    cons_i <- cons_i %>%
      add_row(., obj_typ = obj_typ_add, obj_id = obj_id_add,
                 hyd_typ = hyd_typ_add, frac = frac_add,
                 .before = position) %>%
      add_row(., obj_typ = rep('', n_na), hyd_typ = rep('', n_na))
  } else {
    cons_i <- cons_i %>%
      add_row(., obj_typ = obj_typ_add, obj_id = obj_id_add,
                 hyd_typ = hyd_typ_add, frac = frac_add,
                 .before = position)

    n_el <- n_el + 1

    obj_con[paste0('obj_typ_', n_el)] <- NA_character_
    obj_con[paste0('obj_id_',  n_el)] <- NA_integer_
    obj_con[paste0('hyd_typ_', n_el)] <- NA_character_
    obj_con[paste0('frac_',    n_el)] <- NA_real_
  }

  cons_i <- cons_i %>%
    mutate(i = 1:nrow(.)) %>%
    group_by(hyd_typ) %>%
    mutate(frac = frac/sum(frac)) %>%
    ungroup(.) %>%
    pivot_wider(.,
                names_from = i,
                values_from = c('obj_typ', 'obj_id', 'hyd_typ', 'frac'),
                names_sep = '_') %>%
    select(., paste0(c('obj_typ_', 'obj_id_', 'hyd_typ_', 'frac_'),
                     rep(1:(n_el), each = 4)))

  obj_con_i <- bind_cols(obj_i, cons_i) %>%
    mutate(out_tot = out_tot + 1)

  obj_con[id_i,] <- obj_con_i

  return(obj_con)
}

update_hru_hru_eof <- function(hru_data, hru_id, lu_mgt_sel) {
  hru_eof <- hru_data %>%
    filter(id %in% hru_id) %>%
    mutate(name     = str_replace(name, 'hru', 'eof'),
           surf_stor = name,
           lu_mgt   = lu_mgt_sel,
           id       = nrow(hru_data) + 1:nrow(.))
  hru_data <- bind_rows(hru_data, hru_eof)

  return(hru_data)
}


update_hru_con_eof <- function(hru_con, hru_id, hru_frac) {
  hru_con_init <- hru_con %>%
    filter(id %in% hru_id)

  hru_con_eof <- hru_con_init %>%
    mutate(id     = nrow(hru_con) + 1:nrow(.),
           name   = str_replace(name, 'hru', 'eof'),
           area   = hru_frac * area,
           obj_id = id)

  hru_con_init <- hru_con_init %>%
    mutate(area      = (1 - hru_frac) * area,
           out_tot   = 1,
           obj_typ_1 = 'hru',
           obj_id_1  = hru_con_eof$id,
           hyd_typ_1 = 'tot',
           frac_1    = 1.0)

  hru_con <- bind_rows(hru_con_init, hru_con) %>%
    distinct(., id, .keep_all = TRUE) %>%
    arrange(id) %>%
    bind_rows(hru_con_eof)

  return(hru_con)
}

update_hyd_wet_eof <- function(hyd_wet, hru_id_chr, hyd_wet_sel) {
  hyd_wet_eof <- hyd_wet_sel[rep(1, length(hru_id_chr)),]
  hyd_wet_eof <- hyd_wet_eof %>%
    mutate(name = paste0('hydeof', hru_id_chr),
           .before = 1) %>%
    mutate(hru_frac = 1)

  hyd_wet <- bind_rows(hyd_wet, hyd_wet_eof)

  return(hyd_wet)
}

update_wet_wet_eof <- function(wet_wet, hru_id_chr, wet_wet_sel) {
  wet_wet_eof <- wet_wet_sel[rep(1, length(hru_id_chr)),] %>%
    mutate(id = nrow(wet_wet) + 1:nrow(.),
           name = paste0('eof', hru_id_chr), .before = 1) %>%
    mutate(hyd = paste0('hydeof', hru_id_chr), .before = rel)

  wet_wet <- bind_rows(wet_wet, wet_wet_eof)

  return(wet_wet)
}

update_def_eof <- function(def_tbl, hru_id_eof) {
  for(i in 1:nrow(hru_id_eof)) {
    i_ele <- which(map_lgl(def_tbl$elem,
                           ~ hru_id_eof$id_init[i] %in% .x))
    ele_ids_i <- def_tbl$elem[[i_ele]]
    ele_ids_i[ele_ids_i == hru_id_eof$id_init[i]] <- hru_id_eof$id[i]
    def_tbl$elem[[i_ele]] <- sort(ele_ids_i)
  }

  return(def_tbl)
}

update_ele_eof <- function(ele_tbl, hru_id_chr, hyd_wet_sel) {
  hru_names <- paste0('hru', hru_id_chr)

  ele_eof <- ele_tbl %>%
    filter(name %in% paste0('hru', hru_id_chr)) %>%
    mutate(id     = nrow(ele_tbl) + 1:nrow(.),
           name   = str_replace(name, 'hru', 'eof'))

  if('frac' %in% names(ele_tbl)) {
    ele_eof <- ele_eof %>%
      mutate(obj_id = id,
             frac = hyd_wet_sel$hru_frac * frac)
    ele_tbl <- ele_tbl %>%
      mutate(frac = ifelse(name %in% hru_names,
                           (1 - hyd_wet_sel$hru_frac) * frac,
                           frac))
  } else {
    ele_eof <- ele_eof %>%
      mutate(obj_typ_no = id,
             bsn_frac = hyd_wet_sel$hru_frac * bsn_frac,
             sub_frac = hyd_wet_sel$hru_frac * sub_frac,
             reg_frac = hyd_wet_sel$hru_frac * reg_frac,
      )
    ele_tbl <- ele_tbl %>%
      mutate(bsn_frac = ifelse(name %in% hru_names,
                               (1 - hyd_wet_sel$hru_frac) * bsn_frac,
                               bsn_frac),
             sub_frac = ifelse(name %in% hru_names,
                               (1 - hyd_wet_sel$hru_frac) * sub_frac,
                               reg_frac),
             reg_frac = ifelse(name %in% hru_names,
                               (1 - hyd_wet_sel$hru_frac) * reg_frac,
                               reg_frac))
  }

  ele_tbl <- bind_rows(ele_tbl, ele_eof)

  return(ele_tbl)
}

#' Update the object.cnt input table.
#'
#' The total object count and the count of reservoirs are increased by one.
#'
#' @param obj_cnt object.cnt input table.
#'
#' @returns Updated object.cnt input table.
#'
#' @keywords internal
#'
update_obj_cnt_eof <- function(obj_cnt, n_hru) {
  obj_cnt$obj <- obj_cnt$obj + n_hru
  obj_cnt$hru <- obj_cnt$hru + n_hru

  return(obj_cnt)
}
