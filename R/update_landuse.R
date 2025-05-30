#' Update the landuse of selected HRUs
#'
#' The land use ('lu-mgt') in hru-data.hru of the land objects defined by the
#' `hru_id` is overwritten. The new land use label is defined by `nswrm`. For
#' `nswrm` a new land use is defined in landuse.lum if no land use with this
#' name already exists in landuse.lum which has the exact same parametrization
#' as defined with the additional input arguments. If an identical landuse
#' already exists, this landuse is assigned to the HRUs. If not a new one is
#' added with the defined parameters (`lum_plnt`, `lum_mgt`, `lum_cn2`,
#' `lum_cpr`, `lum_ovn`, and `lum_tile`).
#'
#' @param swat_inputs List with SWAT+ input files.
#' @param hru_id HRU IDs for which the land use is updated.
#' @param nswrm NSWRM to be implemented. This will be the new land use label in
#'   the updated landuse.lum. `nswrm` must be a single character string.
#' @param lum_plnt The plant community which is assigned to the land use. A
#'   single character string must be provided. The passed value must be defined
#'   in the 'plant.ini' intput file. If `lum_plnt = '::keep::`, then the values
#'   of 'plnt_com' of each HRU will be preserved.  If `lum_plnt = 'null'` then
#'   'plnt_com' will be set 'null' in the landuse.lum.
#' @param lum_mgt The management which is assigned to the land use. A single
#'   character string must be provided. The passed value must be defined in the
#'   'management.sch' intput file. If `lum_mgt = '::keep::`, then the values of
#'   'mgt' of each HRU will be preserved.  If `lum_mgt = 'null'` then 'mgt' will
#'   be set 'null' in the landuse.lum.
#' @param lum_cn2 The cn2 parameter set which is assigned to the land use. A
#'   single character string must be provided. The passed value must be defined
#'   in the 'cntabe.lum' intput file. If `lum_cn2 = '::keep::`, then the values
#'   of 'cn2' of each HRU will be preserved.  If `lum_cn2 = 'null'` then 'cn2'
#'   will be set 'null' in the landuse.lum.
#' @param lum_cpr The conservation practice which is assigned to the land use. A
#'   single character string must be provided. The passed value must be defined
#'   in the 'cons_practice.lum' intput file. If `lum_cpr = ::keep::`, then the
#'   values of 'cons_prac' of each HRU will be preserved. If  `lum_cpr = 'null'`
#'   then 'cons_prac' will be set 'null' in the landuse.lum.
#' @param lum_ovn The Mannings n parametrization which is assigned to the land
#'   use. A single character string must be provided. The passed value must be
#'   defined in the 'ovn_table.lum' intput file. If `lum_ovn = '::keep::`,
#'   then the values of 'ov_mann' of each HRU will be preserved.  If
#'   `lum_ovn = 'null'` then 'ov_mann' will be set 'null' in the landuse.lum.
#' @param lum_tile The tile drainage parametrization which is assigned to the
#'   land use. A single character string must be provided. The passed value must
#'   be defined in the 'tiledrain.str' intput file. If `lum_tile = '::keep::`,
#'   then the values of 'tile' of each HRU will be preserved.  If
#'   `lum_tile = null'` then 'tile' will be set 'null' in the landuse.lum.
#' @param lum_grww Grassed waterway parametrization which is assigned to the
#'   land use. A single character string must be provided. The passed value must
#'   be defined in the 'grassedww.str' intput file. If `lum_tile = '::keep::`,
#'   then the values of 'grww' of each HRU will be preserved.  If
#'   `lum_grww = null'` then 'grww' will be set 'null' in the landuse.lum.
#' @param lum_vfs Filter strip parametrization which is assigned to the
#'   land use. A single character string must be provided. The passed value must
#'   be defined in the 'filterstrip.str' intput file. If `lum_vfs = '::keep::`,
#'   then the values of 'vfs' of each HRU will be preserved.  If
#'   `lum_vfs = null'` then 'vfs' will be set 'null' in the landuse.lum.
#' @param lum_bmp User BMP parametrization which is assigned to the
#'   land use. A single character string must be provided. The passed value must
#'   be defined in the 'bmpuser.str' intput file. If `lum_bmp = '::keep::`,
#'   then the values of 'bmp' of each HRU will be preserved.  If
#'   `lum_bmp = null'` then 'bmp' will be set 'null' in the landuse.lum.
#'
#' @returns The SWAT+ input tables list with the updated landuse.lum and
#'   hru-data.hru input tables.
#'
#' @importFrom dplyr bind_rows cur_group_id distinct filter group_by left_join mutate select %>%
#' @importFrom purrr map map_df map_lgl set_names
#' @importFrom stringr str_detect str_remove_all
#' @importFrom tibble add_row
#' @importFrom tidyselect starts_with
#'
#' @keywords internal
#'
update_landuse <- function(swat_inputs, hru_id, nswrm,
                           lum_plnt, lum_mgt, lum_cn2,
                           lum_cpr, lum_ovn, lum_tile,
                           lum_grww, lum_vfs, lum_bmp) {
  # General data type/structure checks
  stopifnot(is.numeric(hru_id))
  stopifnot(is.character(nswrm))
  stopifnot(length(nswrm) == 1)
  stopifnot(is.character(lum_plnt) | is.null(lum_plnt))
  stopifnot(length(lum_plnt) <= 1)
  stopifnot(is.character(lum_mgt) | is.null(lum_mgt))
  stopifnot(length(lum_mgt) <= 1)
  stopifnot(is.character(lum_cn2) | is.null(lum_cn2))
  stopifnot(length(lum_cn2) <= 1)
  stopifnot(is.character(lum_cpr) | is.null(lum_cpr))
  stopifnot(length(lum_cpr) <= 1)
  stopifnot(is.character(lum_ovn) | is.null(lum_ovn))
  stopifnot(length(lum_ovn) <= 1)
  stopifnot(is.character(lum_tile) | is.null(lum_tile))
  stopifnot(length(lum_tile) <= 1)
  stopifnot(is.character(lum_grww) | is.null(lum_grww))
  stopifnot(length(lum_grww) <= 1)
  stopifnot(is.character(lum_vfs) | is.null(lum_vfs))
  stopifnot(length(lum_vfs) <= 1)
  stopifnot(is.character(lum_bmp) | is.null(lum_bmp))
  stopifnot(length(lum_bmp) <= 1)

  hru_lum <- swat_inputs$hru_data.hru %>%
    filter(., id %in% hru_id) %>%
    select(id, lu_mgt) %>%
    set_names(c('id', 'name')) %>%
    left_join(., swat_inputs$landuse.lum, by = 'name') %>%
    distinct(.)

  if(lum_plnt != '::keep::') {
    hru_lum$plnt_com <- lum_plnt
  }
  if(lum_mgt != '::keep::') {
    hru_lum$mgt <- lum_mgt
  }
  if(lum_cn2 != '::keep::') {
    hru_lum$cn2 <- lum_cn2
  }
  if(lum_cpr != '::keep::') {
    hru_lum$cons_prac <- lum_cpr
  }
  if(lum_ovn != '::keep::') {
    hru_lum$ov_mann <- lum_ovn
  }
  if(lum_tile != '::keep::') {
    hru_lum$tile <- lum_tile
  }
  if(lum_grww != '::keep::') {
    hru_lum$grww <- lum_grww
  }
  if(lum_vfs != '::keep::') {
    hru_lum$vfs <- lum_vfs
  }
  if(lum_bmp != '::keep::') {
    hru_lum$bmp <- lum_bmp
  }

  # Find all alternative names with for the input argument 'nswrm'
  lum_exist <- filter(swat_inputs$landuse.lum, str_detect(name, nswrm))

  max_nswrm_id <- lum_exist$name %>%
    str_remove_all(., paste0(nswrm, '|_lum')) %>%
    as.integer(.) %>%
    max(0, .)

  hru_lum <- hru_lum %>%
    group_by(., plnt_com, mgt, cn2, cons_prac, urban, urb_ro, ov_mann, tile, sep, vfs, grww, bmp) %>%
    mutate(group_id = cur_group_id() + max_nswrm_id) %>%
    ungroup() %>%
    mutate(name = paste0(nswrm, group_id, '_lum')) %>%
    select(-group_id)

  hru_lumgt_upd <- select(hru_lum, id, name)

  lum_new <- hru_lum %>%
    select(., - id) %>%
    distinct(.)

  lum_match <- lum_new %>%
    left_join(., lum_exist, by = c("cal_group", "plnt_com", "mgt", "cn2",
                                   "cons_prac", "urban", "urb_ro", "ov_mann",
                                   "tile", "sep", "vfs", "grww", "bmp"),
              suffix = c("", ".exist")) %>%
    select(starts_with('name'))

  lum_new <- lum_new[is.na(lum_match$name.exist),]

  hru_lumgt_upd <- hru_lumgt_upd %>%
    left_join(., lum_match, by = "name") %>%
    mutate(lu_mgt_upd = ifelse(!is.na(name.exist), name.exist, name)) %>%
    select(id, lu_mgt_upd)

  if(nrow(hru_lumgt_upd) > 0) {
    swat_inputs$hru_data.hru <- swat_inputs$hru_data.hru %>%
      left_join(., hru_lumgt_upd, by = "id") %>%
      mutate(lu_mgt = ifelse(!is.na(lu_mgt_upd), lu_mgt_upd, lu_mgt)) %>%
      select(- lu_mgt_upd)
    swat_inputs$file_updated['hru_data.hru'] <- TRUE
  }

  if (nrow(lum_new) > 0) {
    swat_inputs$landuse.lum <- bind_rows(swat_inputs$landuse.lum, lum_new)
    swat_inputs$file_updated['landuse.lum'] <- TRUE
  }

  return(swat_inputs)
}


#' Add a landuse decision table operation to a management schedule
#'
#' @param swat_inputs List with SWAT+ input files.
#' @param hru_id HRU IDs for which the land use is updated.
#' @param op_names Text string which indicates the names of the decision table
#'   operations
#'
#' @returns The SWAT+ input tables list with the updated management.sch table.
#'
#' @importFrom dplyr %>%
#' @importFrom stringr str_remove_all str_split str_trim
#' @importFrom tibble add_row
#'
#' @keywords internal
add_dtl_op <- function(swat_inputs, hru_id, op_names) {
  op_names <- op_names %>%
    str_remove_all(., 'c\\(|\\)') %>%
    str_split(., ',') %>%
    unlist(.) %>%
    str_trim(.)

  op_names <- op_names[!op_names %in% c('::keep::', 'null')]

  n_op <- length(op_names)

  for (hru_i in hru_id) {
    lu_mgt_i <- swat_inputs$hru_data.hru$lu_mgt[swat_inputs$hru_data.hru$id == hru_i]
    sch_name <- swat_inputs$landuse.lum$mgt[swat_inputs$landuse.lum$name == lu_mgt_i]

    id_row <- which(swat_inputs$management.sch$name == sch_name)

    n_add <- 0

    for (op_i in op_names[n_op:1]) {
      if (!op_i %in% swat_inputs$management.sch$op_typ[id_row]) {
        swat_inputs$management.sch <- add_row(swat_inputs$management.sch,
                                              name = sch_name, op_typ = op_i,
                                              .before = id_row[1])
        n_add <- n_add + 1
      }
    }

    id_row <- which(swat_inputs$management.sch$name == sch_name)

    swat_inputs$management.sch$numb_ops[id_row] <- length(id_row) - n_add
    swat_inputs$management.sch$numb_auto[id_row] <-
      max(swat_inputs$management.sch$numb_auto[id_row], na.rm = TRUE) + n_add
  }

  swat_inputs$file_updated['management.sch'] <- TRUE

  return(swat_inputs)
}
