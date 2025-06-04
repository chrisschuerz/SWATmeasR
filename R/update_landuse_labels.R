#' Update the land use labels which were written by SWATfarmR
#'
#' The maximum length for labels in input files which point to entries in other
#' input files are limited to 16 characters. SWATfarmR writes labels for
#' landuses, plant communities, and managements by adding index values. They
#' can result in labels longer than 16 characters. This routine fixes this issue.
#'
#' @param project_path Path to the SWAT+ project folder (i.e. TxtInOut).
#'
#' @return Rewrites the input files hru-data.hru, landuse.lum, management.sch,
#'   and plant.ini with shorter labels.
#'
#' @importFrom dplyr bind_rows distinct left_join group_by group_split mutate n select %>%
#' @importFrom purrr list_c list_rbind map map_lgl map2 map2_df
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_detect str_remove str_remove_all str_replace
#' @importFrom tibble tibble add_row
#'
#' @keywords internal
#'
update_landuse_labels <- function(project_path) {
  mgt_sch <- read_tbl2(file_path = paste0(project_path, '/management.sch'),
                       def_names = c('name', 'numb_ops', 'numb_auto'),
                       par_names = c('op_typ', 'mon', 'day', 'hu_sch',
                                     paste0('op_data', 1:3)),
                       id_num    =  c(2:3, 5:7, 10))

  hru_data <- read_tbl(paste0(project_path, '/hru-data.hru'))

  landuse_lum <- read_tbl(paste0(project_path, '/landuse.lum'))

  plant_ini <- read_tbl2(file_path = paste0(project_path, '/plant.ini'),
                         def_names = c('pcom_name', 'plt_cnt', 'rot_yr_ini'),
                         par_names = c('plt_name', 'lc_status', 'lai_init',
                                       'bm_init', 'phu_init', 'plnt_pop',
                                       'yrs_init', 'rsd_init'),
                         id_num    =  c(2:3, 6:11))

  lu_lbl <- hru_data %>%
    select(lu_mgt) %>%
    mutate(lu_mgt_ini = str_remove(lu_mgt, '\\_[:digit:]+\\_[:digit:]+$')) %>%
    group_by(lu_mgt_ini) %>%
    mutate(n = n()) %>%
    mutate(has_suffix = str_detect(lu_mgt, '\\_[:digit:]+\\_[:digit:]+$')) %>%
    mutate(., suffix_upd = ifelse(n > 1 & has_suffix, 1, NA),
           suffix_upd = cumsum(suffix_upd)) %>%
    mutate(suffix_upd = ifelse(!is.na(suffix_upd), paste0('_', suffix_upd), '')) %>%
    mutate(lu_mgt_upd = str_remove(lu_mgt_ini, '\\_lum')) %>%
    mutate(n_chr = max(nchar(suffix_upd) + max(nchar(lu_mgt_upd)))) %>%
    mutate(lu_mgt_upd = ifelse(n_chr > 12,
                               str_remove_all(lu_mgt_upd, 'a|e|i|o|u'),
                               lu_mgt_upd)) %>%
    mutate(n_chr = max(nchar(suffix_upd) + max(nchar(lu_mgt_upd)))) %>%
    group_split() %>%
    map(., ~ remove_consonants(.x)) %>%
    list_rbind(.) %>%
    mutate(lu_mgt_upd = paste0(lu_mgt_upd, suffix_upd))

  lu_lbl <- lu_lbl %>%
    select(lu_mgt, lu_mgt_upd) %>%
    mutate(lu_mgt_upd = paste0(lu_mgt_upd, '_lum')) %>%
    distinct(.)

  mgt_lbl <- lu_lbl %>%
    mutate(name = str_replace(lu_mgt, 'lum', 'mgt'),
           schedule_upd = str_replace(lu_mgt_upd, 'lum', 'mgt')) %>%
    select(name, schedule_upd) %>%
    add_row(name = 'null', schedule_upd = 'null')

  pcm_lbl <- lu_lbl %>%
    mutate(pcom_name = str_replace(lu_mgt, 'lum', 'comm'),
           pcom_upd  = str_replace(lu_mgt_upd, 'lum', 'com')) %>%
    select(pcom_name, pcom_upd) %>%
    add_row(pcom_name = 'null', pcom_upd = 'null')

  # Added to account for both variants 'comm' and 'com'
  pcm_lbl <- mutate(pcm_lbl, pcom_name = str_replace(pcom_name, 'comm', 'com')) %>%
    bind_rows(pcm_lbl)

  hru_data <- hru_data %>%
    left_join(., lu_lbl, by = 'lu_mgt') %>%
    mutate(lu_mgt = lu_mgt_upd) %>%
    select(-lu_mgt_upd)

  auto_var <- str_detect(names(hru_data), 'V[:digit:]+')

  hru_data <- hru_data[,!auto_var]

  fmt_hru_hru <- c('%8d', '%-16s', rep('%16s', 8))
  write_tbl(hru_data,
            paste0(project_path, '/hru-data.hru'),
            fmt_hru_hru)

  landuse_lum <- landuse_lum %>%
    left_join(., lu_lbl, by = c('name' = 'lu_mgt')) %>%
    mutate(name = lu_mgt_upd) %>%
    select(-lu_mgt_upd) %>%
    left_join(., pcm_lbl, by = c('plnt_com' = 'pcom_name')) %>%
    mutate(plnt_com = pcom_upd) %>%
    select(-pcom_upd) %>%
    left_join(., mgt_lbl, by = c('mgt' = 'name')) %>%
    mutate(mgt = schedule_upd) %>%
    select(-schedule_upd)

  fmt_lu_lum <- c('%-20s', rep('%16s', 13))
  write_tbl(landuse_lum,
            paste0(project_path, '/landuse.lum'),
            fmt_lu_lum)

  mgt_sch <- mgt_sch %>%
    left_join(., mgt_lbl, by = 'name') %>%
    mutate(name = schedule_upd) %>%
    select(-schedule_upd)

  fmt_def_mgt <- c('%-24s', rep('%9.0f', 2))
  fmt_par_mgt <- c('%16s', rep('%8.0f', 2), '%12.5f',
                   rep('%16s', 2), '%12.5f')
  write_tbl2(mgt_sch,
             paste0(project_path, '/management.sch'),
             fmt_def = fmt_def_mgt,
             fmt_par = fmt_par_mgt)

  plant_ini <- plant_ini %>%
    left_join(., pcm_lbl, by = 'pcom_name') %>%
    mutate(pcom_name = pcom_upd) %>%
    select(-pcom_upd) %>%
    filter(!is.na(pcom_name))

  fmt_def_ini <- c('%-16s', '%8.0f', '%10.0f')
  fmt_par_ini <- c('%16s', '%12s', rep('%12.5f', 6))
  write_tbl2(plant_ini,
             paste0(project_path, '/plant.ini'),
             fmt_def = fmt_def_ini,
             fmt_par = fmt_par_ini)
}

#' Remove single consonants of words if they are longer than 12 characters
#'
#' @param tbl Tibble with labels and label lengths
#'
#' @importFrom stringr str_detect str_split
#'
#' @keywords internal
#'
remove_consonants <- function(tbl) {
  if (tbl$n_chr[1] > 12) {
    n <- tbl$n_chr[1] - 12
    txt_split <- str_split(tbl$lu_mgt_upd[1], '', simplify = TRUE)
    alph_pos <- which(str_detect(txt_split, '[:alpha:]'))
    pos_rmv <- seq(2, length(alph_pos), 2)
    pos_rmv <- pos_rmv[1:min(length(pos_rmv), n)]
    if (length(pos_rmv) < n) {
      n_diff <- n - length(pos_rmv)
      pos_rmv <- c(pos_rmv, seq(3, length(alph_pos), length.out = n_diff))
    }
    txt <- paste(txt_split[-alph_pos[pos_rmv]], collapse = '')
    tbl$lu_mgt_upd <- txt
  }
  return(tbl)
}
