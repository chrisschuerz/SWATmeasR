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
#' @param from_cha_id Channel IDs which send water to the implemented
#'   ponds.
#'
#' @param res_res_pnd Table with rel, sed and nut pointers for the added ponds.
#'   ponds.
#'
#' @param hyd_res_pnd Table with hydrology.res parameters for the added ponds.
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
#' @param type Either one of the two pond like types 'pond' or 'constr_wetland'.
#'
#' @returns The `swat_inputs` list with updated input tables which include all
#'   necessary changes to replace land objects by ponds.
#'
#' @keywords internal
#'
replace_by_ponds <- function(swat_inputs, hru_id, to_cha_id, from_cha_id,
                             res_res_pnd, hyd_res_pnd, type) {
  label <- ifelse(type == 'pond', 'pnd', 'cwl')

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
      hru_i <- unlist(hru_id[i])
      to_cha_i <- unlist(to_cha_id[i])
      from_cha_i <- unlist(from_cha_id[i])
      res_res_pnd_i <- res_res_pnd[i, ]
      hyd_res_pnd_i <- hyd_res_pnd[i, ]

      swat_inputs$object.cnt <- update_obj_cnt_pond(swat_inputs$object.cnt)
      swat_inputs$file.cio   <- update_cio_pond(swat_inputs$file.cio)

      swat_inputs$reservoir.res <- update_res_res_pond(swat_inputs$reservoir.res,
                                                       res_res_pnd_i,
                                                       hru_i,
                                                       label)
      res_id <- swat_inputs$reservoir.res$id[nrow(swat_inputs$reservoir.res)]
      swat_inputs$reservoir.con <- update_res_con_pond(swat_inputs$reservoir.con,
                                                       swat_inputs$rout_unit.con,
                                                       hru_i,
                                                       to_cha_i,
                                                       res_id,
                                                       label)
      swat_inputs$hydrology.res <- update_hyd_res_pond(swat_inputs$hydrology.res,
                                                       hyd_res_pnd_i,
                                                       hru_i,
                                                       area_i,
                                                       label)

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
    swat_inputs$file_updated[c('object.cnt', 'file.cio', 'reservoir.res', 'reservoir.con',
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
  # stopifnot(is.numeric(hru_id))
  # stopifnot(is.numeric(to_cha_id))
  # stopifnot(length(hru_id) == length(to_cha_id))

  # if(!is.null(from_cha_id)) {
  #   if(length(hru_id) == 1) {
  #     if(is.list(from_cha_id) & length(from_cha_id) != 1) {
  #       stop("If a single HRUs should be replaced by a pond, then 'from_cha_id' ",
  #            "must be either a vector or a list with length = 1.")
  #     }
  #   } else if (length(hru_id) > 1 & !is.list(from_cha_id)){
  #     stop("If multiple HRUs should be replaced by ponds, then 'from_cha_id' ",
  #          "must be of type 'list'.")
  #   } else if (length(hru_id) != length(to_cha_id)) {
  #     stop("If multiple HRUs should be replaced by ponds, then the list ",
  #          "'from_cha_id' must have the same length as 'hru_id'.")
  #   }
  # }

  res_names <- swat_inputs$reservoir.res$name
  pnd_ids <- as.integer(gsub('pnd', '', res_names[grepl('pnd[:0-9:]+',
                                                        res_names)]))
  cwl_ids <- as.integer(gsub('cwl', '', res_names[grepl('cwl[:0-9:]+',
                                                        res_names)]))
  is_pond <- map_int(hru_id, min) %in% pnd_ids
  is_cwtl <- map_int(hru_id, min) %in% cwl_ids

  if (any(is_pond)) {
    warning('The HRUs with the IDs ', paste(unlist(hru_id[is_pond]), collapse = ', '),
            ' were already replaced by ponds and are skipped.')
  }
  if (any(is_cwtl)) {
    warning('The HRUs with the IDs ', paste(unlist(hru_id[is_cwtl]), collapse = ', '),
            ' were already replaced by construced wetlands and are skipped.')
  }
  is_not_updated <- is_pond | is_cwtl

  return(is_not_updated)
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

  con_chg <- con_ids %>%
    filter(., obj_typ == 'ru' & obj_id %in% unlist(hru_ids)) %>%
    select(., obj_id, id, con_id)

  return(con_chg)
}

#' Update the file.cio input table.
#'
#' The reservoir input tables are added for the case they were missing, because
#' no reservoirs were implemented in the initial model setup.
#'
#' @param file_cio List of file.cio lines.
#'
#' @returns Updated file.cio table.
#'
#' @keywords internal
#'
update_cio_pond <- function(file_cio) {
  file_cio$connect[9] <- 'reservoir.con'
  file_cio$reservoir[3] <- 'reservoir.res'
  file_cio$reservoir[4] <- 'hydrology.res'
  return(file_cio)
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
  chg_i <- filter(rtu_con_chg, obj_id %in% hru_id)

  if(nrow(chg_i) > 0) {
    for (i in 1:nrow(chg_i)) {
      # Change object type from routing unit to reservoir (= land to pond)
      rtu_con[rtu_con$id == chg_i$id[i],
              paste0('obj_typ_', chg_i$con_id[i])] <- 'res'

      # Replace the initial land ID with the new reservoir ID
      rtu_con[rtu_con$id == chg_i$id[i],
              paste0('obj_id_', chg_i$con_id[i])] <- res_id
    }
  }

  # Set the number of connections for the replaced object to 0
  rtu_con[rtu_con$id %in% hru_id, ]$out_tot <- 0

  # Get the maximum number of connections
  n_con <- (ncol(rtu_con)-13)/4
  # Set all object types for connections from the replaced object to an empty string
  rtu_con[rtu_con$id %in% hru_id, paste0('obj_typ_', 1:n_con)] <- ''
  # Set all object ids for connections from the replaced object to NA
  rtu_con[rtu_con$id %in% hru_id, paste0('obj_id_', 1:n_con)] <- NA_integer_
  # Set all hydrology types for connections from the replaced object to an empty string
  rtu_con[rtu_con$id %in% hru_id, paste0('hyd_typ_', 1:n_con)] <- ''
  # Set all fractions for connections from the replaced object to NA
  rtu_con[rtu_con$id %in% hru_id, paste0('frac_', 1:n_con)] <- NA_real_
  #Set area of replaced object to minimum value
  rtu_con[rtu_con$id %in% hru_id, ]$area <- 0.00001

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
  cha_con[cha_con$id %in% from_cha_id,]$hyd_typ_1 <- 'tot'
  cha_con[cha_con$id %in% from_cha_id,]$frac_1 <- 1.0

  # Delete all other connections which of the channels which are now
  # routed into the reservoir.
  if(n_con > 1) {
    cha_con[cha_con$id %in% from_cha_id, paste0('obj_typ_', 2:n_con)] <- ''
    cha_con[cha_con$id %in% from_cha_id, paste0('obj_id_', 2:n_con)] <- NA_integer_
    cha_con[cha_con$id %in% from_cha_id, paste0('hyd_typ_', 2:n_con)] <- ''
    cha_con[cha_con$id %in% from_cha_id, paste0('frac_', 2:n_con)] <- NA_real_
  }

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
#' @param label Either 'pnd' for pond or 'cwl' for constructed wetlands.
#'
#' @returns Updated reservoir.con input table with new line for the new
#'   reservoir.
#'
#' @importFrom dplyr bind_rows filter mutate select summarise %>%
#'
#' @keywords internal
#'
update_res_con_pond <- function(res_con, rtu_con, hru_id, to_cha_id, res_id, label) {
  rtu_to_res <- rtu_con %>%
    filter(., id %in% hru_id) %>%
    select(., id:out_tot) %>%
    mutate(id = res_id,
           name = paste0(label, min(hru_id))) %>%
    summarise(id = id[1],
              name = name[1],
              gis_id = gis_id[1],
              area = sum(area),
              lat = lat[1],
              lon = lon[1],
              elev = mean(elev),
              obj_id = id[1],
              wst = wst[1],
              cst = cst[1],
              ovfl = ovfl[1],
              rule = rule[1],
              out_tot = 2) %>%
    mutate(obj_typ_1 = 'sdc',
           obj_id_1 = to_cha_id,
           hyd_typ_1 = 'tot',
           frac_1 = 1.0,
           obj_typ_2 = 'aqu',
           obj_id_2 = 1,
           hyd_typ_2 = 'rhg',
           frac_2 = 1.0)

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
#' @param res_res_pnd table with rel, sed, and nut pointers for added pond.
#' @param hru_id  ID of the HRU which is replaced by a pond.
#' @param label Either 'pnd' for pond or 'cwl' for constructed wetlands.
#'
#' @returns Updated reservoir.res input table.
#'
#' @importFrom dplyr bind_cols bind_rows %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
update_res_res_pond <- function(res_res, res_res_pnd, hru_id, label) {
  res_add <- tibble(id = max(res_res$id, 0) + 1,
                    name = paste0(label, min(hru_id)),
                    init = res_res_pnd$init,
                    hyd  = name,
                    rel  = res_res_pnd$rel,
                    sed  = res_res_pnd$sed,
                    nut  = res_res_pnd$nut)

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
#' @param hyd_res_pnd hydrology.res parameters table for added pond.
#' @param hru_id  ID of the HRU which is replaced by a pond.
#' @param area Area of the replaced land object in ha.
#' @param label Either 'pnd' for pond or 'cwl' for constructed wetlands.
#'
#' @returns Updated hydrology.res input table.
#'
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
update_hyd_res_pond <- function(hyd_res, hyd_res_pnd, hru_id, area, label) {
  # The implemented parameters are still default paremters. In a future version
  # the parameters should be input by the user via the pond definition file.
  hyd_add <- tibble(name = paste0(label, min(hru_id)),
                    yr_op = 1,
                    mon_op = 1) %>%
    bind_cols(., hyd_res_pnd)

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
  hru_con[hru_con$id %in% hru_id, ]$area <- 0.00001
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
