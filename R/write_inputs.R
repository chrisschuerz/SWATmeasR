#' Write the SWAT+ input tables from the `swat_input` list object which were
#' updated by implementing NSWRMs to SWAT+ input text files.
#'
#' @param swat_inputs List of SWAT+ input tables.
#' @param file_updated Named boolean vector which indicated which file was
#'   updated and should therefore be written.
#' @param project_path Path to the SWAT project folder on the hard drive (i.e.
#'   txtinout folder).
#'
#' @returns Writes SWAT+ text input files into the `project_path`.
#'
#' @keywords internal
#'
write_swat_inputs <- function(swat_inputs, file_updated, project_path) {
  if(file_updated['object.cnt']) {
    fmt_obj_cnt <- c('%-16s', rep('%12.5f', 2), rep('%8d', 18))
    write_tbl(swat_inputs$object.cnt,
              paste0(project_path, '/object.cnt'),
              fmt_obj_cnt)
  }
  if(file_updated['file.cio']) {
    write_cio(swat_inputs$file.cio,
              paste0(project_path, '/file.cio'))
  }
  if(file_updated['landuse.lum']) {
    fmt_lu_lum <- c('%-20s', rep('%16s', 13))
    write_tbl(swat_inputs$landuse.lum,
              paste0(project_path, '/landuse.lum'),
              fmt_lu_lum)
  }
  if(file_updated['management.sch']) {
    fmt_def_mgt <- c('%-24s', rep('%9.0f', 2))
    fmt_par_mgt <- c('%16s', rep('%8.0f', 2), '%12.5f',
                     rep('%16s', 2), '%12.5f')
    write_tbl2(swat_inputs$management.sch,
               paste0(project_path, '/management.sch'),
               fmt_def = fmt_def_mgt,
               fmt_par = fmt_par_mgt)
  }
  if(file_updated['plant.ini']) {
    fmt_def_ini <- c('%-16s', '%8.0f', '%10.0f')
    fmt_par_ini <- c('%16s', '%12s', rep('%12.5f', 6))
    write_tbl2(swat_inputs$plant.ini,
               paste0(project_path, '/plant.ini'),
               fmt_def = fmt_def_ini,
               fmt_par = fmt_par_ini)
  }
  if(file_updated['hru_data.hru']) {
    fmt_hru_hru <- c('%8d', '%-16s', rep('%16s', 8))
    write_tbl(swat_inputs$hru_data.hru,
              paste0(project_path, '/hru-data.hru'),
              fmt_hru_hru)
  }
  if(file_updated['hru.con']) {
    n_con <- (ncol(swat_inputs$hru.con) - 13) / 4
    fmt_hru_con <- c('%8d', '%-16s', '%8d', rep('%12.5f', 4),
                     '%8d', '%16s', rep('%8s', 3), '%8d',
                     rep(c('%12s', '%8d', '%12s', '%12.5f'), n_con))
    write_tbl(swat_inputs$hru.con,
              paste0(project_path, '/hru.con'),
              fmt_hru_con)
  }
  if(file_updated['rout_unit.rtu']) {
    fmt_rtu_rtu <- c('%8d', rep('%16s', 5))
    write_tbl(swat_inputs$rout_unit.rtu,
              paste0(project_path, '/rout_unit.rtu'),
              fmt_rtu_rtu)
  }
  if(file_updated['rout_unit.con']) {
    n_con <- (ncol(swat_inputs$rout_unit.con) - 13) / 4
    fmt_rtu_con <- c('%8d', '%-16s', '%8d', rep('%12.5f', 4),
                     '%8d', '%16s', rep('%8s', 3), '%8d',
                     rep(c('%12s', '%8d', '%12s', '%12.5f'), n_con))
    write_tbl(swat_inputs$rout_unit.con,
              paste0(project_path, '/rout_unit.con'),
              fmt_rtu_con)
  }
  if(file_updated['rout_unit.def']) {
    fmt_rtu_def <- c('%8d', '%-16s', '%8d', '%8d')
    write_tbl(swat_inputs$rout_unit.def,
              paste0(project_path, '/rout_unit.def'),
              fmt_rtu_def)
  }
  if(file_updated['rout_unit.ele']) {
    fmt_rtu_ele <- c('%8d', '%-16s', '%16s', '%8d', '%12.5f', '%16d')
    write_tbl(swat_inputs$rout_unit.ele,
              paste0(project_path, '/rout_unit.ele'),
              fmt_rtu_ele)
  }
  if(file_updated['chandeg.con']) {
    n_con <- (ncol(swat_inputs$chandeg.con) - 13) / 4
    fmt_sdc_con <- c('%8d', '%-16s', '%8d', rep('%12.5f', 4),
                     '%8d', '%16s', rep('%8s', 3), '%8d',
                     rep(c('%12s', '%8d', '%12s', '%12.5f'), n_con))
    write_tbl(swat_inputs$chandeg.con,
              paste0(project_path, '/chandeg.con'),
              fmt_sdc_con)
  }
  if(file_updated['reservoir.res']) {
    fmt_res_res <- c('%8d', '%-16s', rep('%16s', 5))
    write_tbl(swat_inputs$reservoir.res,
              paste0(project_path, '/reservoir.res'),
              fmt_res_res)
  }
  if(file_updated['hydrology.res']) {
    fmt_hyd_res <- c('%-16s', '%8d', '%8d', rep('%12.5f', 8))
    write_tbl(swat_inputs$hydrology.res,
              paste0(project_path, '/hydrology.res'),
              fmt_hyd_res)
  }
  if(file_updated['reservoir.con']) {
    n_con <- (ncol(swat_inputs$reservoir.con) - 13) / 4
    fmt_res_con <- c('%8d', '%-16s', '%8d', rep('%12.5f', 4),
                     '%8d', '%16s', rep('%8s', 3), '%8d',
                     rep(c('%12s', '%8d', '%12s', '%12.5f'), n_con))
    write_tbl(swat_inputs$reservoir.con,
              paste0(project_path, '/reservoir.con'),
              fmt_res_con)
  }
  if(file_updated['wetland.wet']) {
    fmt_wet_wet <- c('%8d', '%-16s', rep('%16s', 5))
    write_tbl(swat_inputs$wetland.wet,
              paste0(project_path, '/wetland.wet'),
              fmt_wet_wet)
  }
  if(file_updated['hydrology.wet']) {
    fmt_hyd_wet <- c('%-16s', rep('%12.5f', 10))
    write_tbl(swat_inputs$hydrology.wet,
              paste0(project_path, '/hydrology.wet'),
              fmt_hyd_wet)
  }
}

#' Write SWAT+ input file which has a tabular structure.
#'
#' @param tbl SWAT input table in tibble (data.frame) format.
#' @param file_path Write path of the SWAT+ input file.
#' @param fmt Character vector of format strings to define the print format of
#'   each table column.
#'
#' @returns Writes a text file table in the file path.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map_int map2_df
#' @importFrom readr write_lines
#' @importFrom stringr str_remove str_replace str_replace_all
#'
#' @keywords internal
#'
write_tbl <- function(tbl, file_path, fmt) {
  tbl <- map2_df(tbl, fmt, ~ sprintf(.y, .x))

  fmt_names <- fmt %>%
    str_remove(., '\\.[:digit:]+') %>%
    str_replace(., 'f|d', 's')

  col_names <- colnames(tbl) %>%
    sprintf(fmt_names, .) %>%
    paste(., collapse = '  ')

  file_lines <- tbl %>%
    apply(., 1, paste, collapse = '  ') %>%
    str_replace_all(., '  NA', '    ')

  file_head <- paste('SWAT+ input file updated with SWATmeasR at', Sys.time())

  input_file <- c(file_head, col_names, file_lines)

  write_lines(input_file, file_path)
}

#' Write SWAT+ input file which has a tabular structure with a definition line
#' for each parameter table section (e.g. management.sch, plant.ini, soils.sol).
#'
#' @param tbl SWAT input table in tibble (data.frame) format.
#' @param file_path Write path of the SWAT+ input file.
#' @param fmt_def Character vector of format strings to define the print format
#'   of each table column which is part of the definition line.
#' @param fmt_par Character vector of format strings to define the print format
#'   of each table column which is part of the parameter table.
#'
#' @returns Writes a text file table in the file path.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr list_c map map_int map2_df
#' @importFrom readr write_lines
#' @importFrom stringr str_remove str_replace str_replace_all
#'
#' @keywords internal
#'
write_tbl2 <- function(tbl, file_path, fmt_def, fmt_par) {
  n_def <- length(fmt_def)
  n_col <- ncol(tbl)

  fmt_names <- c(fmt_def, fmt_par) %>%
    str_remove(., '\\.[:digit:]+') %>%
    str_replace(., 'f|d', 's')

  col_names <- colnames(tbl) %>%
    sprintf(fmt_names, .) %>%
    paste(., collapse = '  ')

  split_var <- factor(tbl[[1]])
  tbl <- tbl %>%
    map2_df(., c(fmt_def, fmt_par), ~sprintf(.y, .x)) %>%
    split(., split_var)

  def_lines <- tbl %>%
    map_df(., ~ .x[1,1:n_def]) %>%
    apply(., 1, paste, collapse = '  ')

  n_shift <- nchar(def_lines[1]) + 2

  par_lines <- tbl %>%
    map(., ~ .x[,(n_def+1):n_col]) %>%
    map(., ~ apply(.x, 1, paste, collapse = '  ')) %>%
    map(., ~ paste0(sprintf(paste0('%', n_shift, 's'), ''), .x))

  file_lines <- map2(def_lines, par_lines, ~ c(.x, .y)) %>%
    list_c(.) %>%
    str_replace_all(., '  NA', '    ')

  file_head <- paste('SWAT+ input file updated with SWATmeasR at', Sys.time())

  input_file <- c(file_head, col_names, file_lines)

  write_lines(input_file, file_path)
}

#' Write SWAT+ file.cio file.
#'
#' @param file_cio SWAT file.cio.
#' @param file_path Write path of the SWAT+ input file.
#'
#' @returns Writes the file.cio as a text file in the file path.
#'
#' @importFrom dplyr %>%
#' @importFrom purrr map map_chr
#' @importFrom readr write_lines
#'
#' @keywords internal
#'
write_cio <- function(file_cio, file_path, fmt) {
  file_lines <- file_cio %>%
    map(., ~ sprintf('%-20s', .x)) %>%
    map_chr(., ~ paste(.x, collapse = ' '))

  file_head <- paste('SWAT+ input file updated with SWATmeasR at', Sys.time())

  input_file <- c(file_head, file_lines)

  write_lines(input_file, file_path)
}
