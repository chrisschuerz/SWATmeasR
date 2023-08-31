#' Implement ponds by replacing land objects.
#'
#' The land objects defined by the `hru_id` are assigned an area of (almost) 0.
#' The connection fractions for those land objects are set to 0. Reservoirs with
#' the same area/lat/long/elev are generated (at the moment with default
#' reservoir dimensions) are implemented in the reservoir input files. All land
#' objects which initially routed water to the land objects with `hru_id` now
#' route water to the new reservoirs. The new reservoirs must route water to one
#' channel each defined with `to_cha_id`. Optionally a `from_cha_id` can be
#' defined if channels should be rerouted into the new reservoir.
#'
#' @param swat_inputs List with SWAT+ input files.
#' @param hru_id HRU IDs which will be replaced by ponds.
#'
#'   The input can be a single numeric value to replace a single HRU, or a
#'   vector of numeric values if multiple HRUs should be replaced by ponds.
#'
#' @param to_cha_id Channel IDs into which implemented ponds route water.
#'
#'   The input can be a single numeric value or a vector of values. `to_cha_id`
#'   must be the same length as `hru_id`. Each channel ID corresponds to the
#'   respective `hru_id`.
#'
#' @param from_cha_id (optional) channel IDs which send water to the implemented
#'   ponds.
#'
#'   The input is default `NA`. Then the channel routing remains unchanged.
#'   Multiple channels can be rerouted into a pond. If `hru_id` is a single
#'   value `from_cha_id` can be a single numeric value to reroute one channel,
#'   or a vector to reroute multiple channels into the implemented pond. If
#'   `hru_id` is a vector, `from_cha_id` must be a list with the same length, as
#'   `hru_id`. If for a respective `hru_id` no channels should be rerouted that
#'   list element is `NA`, otherwise the list element can be a value or a
#'   vector.
#'
#' @returns The `swat_inputs` list with updated input tables which include all
#'   necessary changes to replace land objects by ponds.
#'
#' @keywords internal
#'
replace_by_ponds <- function(swat_inputs, hru_id, to_cha_id, from_cha_id = NULL) {
  # Check if an HRU was already replaced by a pond
  is_pond <- check_arguments_pond(swat_inputs, hru_id, to_cha_id, from_cha_id)
  # Exclude that HRUs/channels from the ones which will be replaced/modified/
  hru_id    <- hru_id[!is_pond]
  to_cha_id <- to_cha_id[!is_pond]
  if(is.list(from_cha_id)) {
    from_cha_id   <- from_cha_id[!is_pond]
  }

  # If there are HRUs remaining which can be replaced by ponds loop over all
  # land objects and update the respective input files.
  if(length(hru_id) > 0) {
    rtu_con_chg <- get_chg_ids_pond(swat_inputs$rout_unit.con, hru_id)

    for (i in 1:length(hru_id)) {
      hru_i <- hru_id[i]
      to_cha_i <- to_cha_id[i]
      from_cha_i <- unlist(from_cha_id[i])
      area_i <-
        swat_inputs$rout_unit.con[swat_inputs$rout_unit.con$id == hru_i,]$area

      swat_inputs$object.cnt <- update_obj_cnt_pond(swat_inputs$object.cnt)

      swat_inputs$reservoir.res <- update_res_res_pond(swat_inputs$reservoir.res,
                                                       hru_i)
      res_id <- swat_inputs$reservoir.res$id[nrow(swat_inputs$reservoir.res)]
      swat_inputs$reservoir.con <- update_res_con_pond(swat_inputs$reservoir.con,
                                                       swat_inputs$rout_unit.con,
                                                       hru_i,
                                                       to_cha_i,
                                                       res_id)
      swat_inputs$hydrology.res <- update_hyd_res_pond(swat_inputs$hydrology.res,
                                                       hru_i,
                                                       area_i)

      swat_inputs$rout_unit.con <- update_rtu_con_pond(swat_inputs$rout_unit.con,
                                                       rtu_con_chg,
                                                       hru_i,
                                                       res_id)
      swat_inputs$hru.con <- update_hru_con_pond(swat_inputs$hru.con, hru_i)

      swat_inputs$chandeg.con <- update_cha_con_pond(swat_inputs$chandeg.con,
                                                     from_cha_i,
                                                     res_id)
    }
    # Set the input files which are adjusted by pond replacement to 'modified'
    # so that they will be written when writing output files.
    swat_inputs$file_updated[c('object.cnt', 'reservoir.res', 'reservoir.con',
                               'hydrology.res', 'rout_unit.con', 'hru.con',
                               'chandeg.con')] <- TRUE
  }

  return(swat_inputs)
}

#' Check the function input arguments.
#'
#' Lengths and data types are checked. Routine checks if HRUs were already
#' replaced by ponds, triggers a warning if yes and removes the HRUs from the
#' replacement set.
#'
#' @param swat_inputs List with SWAT+ input files.
#' @param hru_id HRU IDs which will be replaced by ponds.
#' @param to_cha_id Channel IDs into which implemented ponds route water.
#' @param from_cha_id (optional) channel IDs which send water to the implemented
#'   ponds.
#'
#' @returns A boolean vector to indicate which HRUs are already replaced by
#'   ponds.
#'
#' @keywords internal
#'
check_arguments_pond <- function(swat_inputs, hru_id, to_cha_id, from_cha_id) {
  stopifnot(is.numeric(hru_id))
  stopifnot(is.numeric(to_cha_id))
  stopifnot(length(hru_id) == length(to_cha_id))

  if(!is.null(from_cha_id)) {
    if(length(hru_id) == 1) {
      if(is.list(from_cha_id) & length(from_cha_id) != 1) {
        stop("If a single HRUs should be replaced by a pond, then 'from_cha_id' ",
             "must be either a vector or a list with length = 1.")
      }
    } else if (length(hru_id) > 1 & !is.list(from_cha_id)){
      stop("If multiple HRUs should be replaced by ponds, then 'from_cha_id' ",
           "must be of type 'list'.")
    } else if (length(hru_id) != length(to_cha_id)) {
      stop("If multiple HRUs should be replaced by ponds, then the list ",
           "'from_cha_id' must have the same length as 'hru_id'.")
    }
  }

  res_names <- swat_inputs$reservoir.res$name
  pnd_ids <- as.integer(gsub('pnd', '', res_names[grepl('pnd[:0-9:]+',
                                                        res_names)]))
  is_pond <- hru_id %in% pnd_ids
  if (any(is_pond)) {
    warning('The HRUs with the IDs ', paste(hru_id[is_pond], collapse = ', '),
            ' were already replaced by ponds and are skipped.')
  }

  return(is_pond)
}

#' Get the IDs of the units for which the connectivities must be changed.
#'
#' The units in the connectivity input table are identified which initially
#' routed water into the HRUs which will be replaced by ponds.
#'
#'
#' @param con_tbl A SWAT+ .con input table (connectivity)
#' @param hru_ids Numeric vector of HRU IDs which will be replaced by ponds.
#'
#' @returns A tibble with the objects for which the connectivities must be
#'   changed. The table has the following columns:
#'
#'   - `obj_id` ID of the object for which the connectivity must be changes
#'   - `id` ID of the HRU which will be replaced by pond
#'   - `con_id` ID of the connectivity in the connectivity table which must be
#'     changed.
#'
#' @importFrom dplyr arrange filter mutate select %>%
#' @importFrom purrr map map_df set_names
#' @importFrom stringr str_remove
#' @importFrom tidyselect ends_with
#'
#' @keywords internal
#'
get_chg_ids_pond <- function(con_tbl, hru_ids) {
  n_con <- as.integer(str_remove(names(con_tbl)[ncol(con_tbl)], 'frac_'))

  con_ids <- map(1:n_con, ~ select(con_tbl, id, ends_with(paste0('_', .x)))) %>%
    map_df(., ~ set_names(.x,
                          c('id', 'obj_typ', 'obj_id', 'hyd_typ', 'frac'))) %>%
    arrange(id) %>%
    mutate(con_id = rep(1:n_con, nrow(con_tbl)))

  con_chg <- map_df(hru_ids,
                    ~ filter(con_ids, obj_typ == 'ru' & obj_id == .x)) %>%
    select(obj_id, id, con_id)

  return(con_chg)
}

#' Update the rout_unit.con input table.
#'
#' All connections of land objects which initially routed into the replaced land
#' object (HRU) are replaced by a connection into the newly generated reservoir.
#' For all connections of the replaced land object the fractions are set to 0.
#' To elimiate the effect of e.g. ET from that object its area is set to the
#' minimum value (0.00001).
#'
#' @param rtu_con rout_unit.con input table.
#' @param rtu_con_chg Lookup table for which connectivities must be changed.
#' @param hru_id ID of the HRU which is replaced by a pond.
#' @param res_id New ID of the pond (reservoir) which replaces the land object.
#'
#' @returns Updated rout_unit.con table.
#'
#' @importFrom dplyr filter
#'
#' @keywords internal
#'
update_rtu_con_pond <- function(rtu_con, rtu_con_chg, hru_id, res_id) {
  chg_i <- filter(rtu_con_chg, obj_id == hru_id)

  if(nrow(chg_i) > 0) {
    for (i in 1:nrow(chg_i)) {
      # Change object type from routing unit to reservoir (= land to pond)
      rtu_con[rtu_con$id == chg_i$id[i],
              paste0('obj_typ_', chg_i$con_id[i])] <- 'res'

      # Replace the initial land ID with the new reservoir ID
      rtu_con[rtu_con$id == chg_i$id[i],
              paste0('obj_id_', chg_i$con_id[i])] <- res_id
    }

    # Set all fractions for connections from the replaced object to 0
    rtu_con[rtu_con$id == hru_id,
            paste0('frac_', 1:((ncol(rtu_con)-13)/4))] <- 0

    #Set area of replaced object to minimum value
    rtu_con[rtu_con$id == hru_id, ]$area <- 0.00001
  }

  return(rtu_con)
}

#' Update the chandeg.con input table.
#'
#' For the channels which should now route into the new reservoir replace all
#' connections which the channels initially had and add the connection to the
#' new reservoir.
#'
#' @param cha_con chandeg.con input table
#' @param from_cha_id Vector of IDs for the channels which should be routed to
#'   the new reservoir.
#' @param res_id New ID of the pond (reservoir) which replaces the land object.
#'
#' @returns Updated chandeg.con input table.
#'
#' @keywords internal
#'
update_cha_con_pond <- function(cha_con, from_cha_id, res_id) {
  # Get the maximum number of connections in chandeg.con
  n_con <- (ncol(cha_con)-13)/4

  # Rewrite the first (and only) connection of the channels
  # which are now fully routed into the new reservoir.
  cha_con[cha_con$id %in% from_cha_id,]$out_tot <- 1L
  cha_con[cha_con$id %in% from_cha_id,]$obj_typ_1 <- 'res'
  cha_con[cha_con$id %in% from_cha_id,]$obj_id_1 <- res_id
  cha_con[cha_con$id %in% from_cha_id,]$frac_1 <- 1.0

  # Delete all other connections which of the channels which are now
  # routed into the reservoir.
  cha_con[cha_con$id %in% from_cha_id,14 + 4*(1:(n_con-1))] <- ''
  cha_con[cha_con$id %in% from_cha_id,15 + 4*(1:(n_con-1))] <- NA
  cha_con[cha_con$id %in% from_cha_id,16 + 4*(1:(n_con-1))] <- ''
  cha_con[cha_con$id %in% from_cha_id,17 + 4*(1:(n_con-1))] <- NA

  return(cha_con)
}

#' Update the reservoir.con input table.
#'
#' Take the properties of the routing unit (area, lat, lon, elev...) and use
#' them for the newly generated reservoir. Add those properties as a new line
#' in reservoir.con and add the connection to the channel.
#'
#' @param res_con reservoir.con input table
#' @param rtu_con rout_unit.con input table.
#' @param hru_id ID of the HRU which is replaced by a pond.
#' @param to_cha_id ID of channel to which water from reservoir is routed.
#' @param res_id New ID of the pond (reservoir) which replaces the land object.
#'
#' @returns Updated reservoir.con input table with new line for the new
#'   reservoir.
#'
#' @importFrom dplyr bind_rows filter mutate select %>%
#'
#' @keywords internal
#'
update_res_con_pond <- function(res_con, rtu_con, hru_id, to_cha_id, res_id) {
  rtu_to_res <- rtu_con %>%
    filter(., id == hru_id) %>%
    select(., id:out_tot) %>%
    mutate(id = res_id,
           name = paste0('pnd', hru_id),
           obj_id = id,
           out_tot = 1L,
           obj_typ_1 = 'sdc',
           obj_id_1 = to_cha_id,
           hyd_typ_1 = 'tot',
           frac_1 = 1.0)

  res_con <- bind_rows(res_con, rtu_to_res)

  return(res_con)
}

#' Update the reservoir.res input table.
#'
#' Add a new line in the reservoir.res table with the ID and name of the new
#' reservoir. Only the hydrology column `hyd` and the ID are updated. The other
#' pointers are set to the default entries (currently the case for all
#' reservoirs).
#'
#' @param res_res reservoir.res input table
#' @param hru_id  ID of the HRU which is replaced by a pond.
#'
#' @returns Updated reservoir.res input table.
#'
#' @importFrom dplyr bind_rows mutate %>%
#'
#' @keywords internal
#'
update_res_res_pond <- function(res_res, hru_id) {
  res_add <- res_res[nrow(res_res), ] %>%
    mutate(id = id + 1,
           name = paste0('pnd', hru_id),
           hyd  = name)
  res_res <- bind_rows(res_res, res_add)

  return(res_res)
}

#' Update the hydrology.res input table.
#'
#' Hydrology parameters for the new reservoir are added to hydrology.res.
#' Most of the parameters are default for all reservoirs. Default the area and
#' volume parameters are simple functions of the area. The area is the same as
#' the replaced land object.
#'
#' @param hyd_res hydrology.res input table.
#' @param hru_id  ID of the HRU which is replaced by a pond.
#' @param area Area of the replaced land object in ha.
#'
#' @returns Updated hydrology.res input table.
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
update_hyd_res_pond <- function(hyd_res, hru_id, area) {
  # The implemented parameters are still default paremters. In a future version
  # the parameters should be input by the user via the pond definition file.
  hyd_add <- tibble(name = paste0('pnd', hru_id),
                    yr_op = 1,
                    mon_op = 1,
                    area_ps = area,
                    vol_ps = 10* area_ps,
                    area_es = 1.15* area_ps,
                    vol_es = 1.15*vol_ps,
                    k = 0,
                    evap_co = 0.6,
                    shp_co1 = 0,
                    shp_co2 = 0)
  hyd_res <- bind_rows(hyd_res, hyd_add)

  return(hyd_res)
}

#' Update the hru.con input table.
#'
#' To remove HRU from land process calculations the area is set to the minimum
#' value of 0.00001.
#'
#' @param hru_con hru.con input table.
#' @param hru_id ID of the HRU which is replaced by a pond.
#'
#' @returns Updated hru.con input table.
#'
#' @keywords internal
#'
update_hru_con_pond <- function(hru_con, hru_id) {
  hru_con[hru_con$id == hru_id, ]$area <- 0.00001
  return(hru_con)
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
update_obj_cnt_pond <- function(obj_cnt) {
  obj_cnt$obj <- obj_cnt$obj + 1
  obj_cnt$res <- obj_cnt$res + 1

  return(obj_cnt)
}
