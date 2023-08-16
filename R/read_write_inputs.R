#' Read SWAT+ input files and arrange them in a list object
#'
#' @param project_path Path to the SWAT project folder on the hard drive
#'   (i.e. txtinout folder).
#'
#' @returns A list object with the SWAT+ input files organized in tibbles.
#'
#' @export
#'
read_swat_inputs <- function(project_path) {
  input_list <- list(
    object.cnt        = read_tbl(paste0(project_path, '/object.cnt')),
    landuse.lum       = read_tbl(paste0(project_path, '/landuse.lum')),
    cntabe.lum        = read_tbl_n(paste0(project_path, '/cntable.lum'),
                                   id_col_sel = 1:5, id_num = 2:5),
    cons_practice.lum = read_tbl_n(paste0(project_path, '/cons_practice.lum'),
                                id_col_sel = 1:3, id_num = 2:3),
    ovn_table.lum     = read_tbl_n(paste0(project_path, '/ovn_table.lum'),
                                   id_col_sel = 1:4, id_num = 2:4),
    management.sch    = read_tbl2(
                          file_path = paste0(project_path, '/management.sch'),
                          def_names = c('name', 'numb_ops', 'numb_auto'),
                          par_names = c('op_typ', 'mon', 'day', 'hu_sch',
                                        paste0('op_data', 1:3)),
                          id_num    =  c(2:3, 5:7, 10)),
    plant.ini         = read_tbl2(
                          file_path = paste0(project_path, '/plant.ini'),
                          def_names = c('pcom_name', 'plt_cnt', 'rot_yr_ini'),
                          par_names = c('plt_name', 'lc_status', 'lai_init',
                                        'bm_init', 'phu_init', 'plnt_pop',
                                        'yrs_init', 'rsd_init'),
                          id_num    =  c(2:3, 6:11)),
    hru_data.hru      = read_tbl(paste0(project_path, '/hru-data.hru')),
    hru.con           = read_con_file(paste0(project_path, '/hru.con')),
    rout_unit.rtu     = read_tbl(paste0(project_path, '/rout_unit.rtu')),
    rout_unit.con     = read_con_file(paste0(project_path, '/rout_unit.con')),
    rout_unit.def     = read_tbl(paste0(project_path, '/rout_unit.def')),
    rout_unit.ele     = read_tbl(paste0(project_path, '/rout_unit.ele')),
    chandeg.con       = read_con_file(paste0(project_path, '/chandeg.con')),
    reservoir.res     = read_tbl(paste0(project_path, '/reservoir.res')),
    hydrology.res     = read_tbl(paste0(project_path, '/hydrology.res')),
    reservoir.con     = read_con_file(paste0(project_path, '/reservoir.con'))
  )

  file_names <- names(input_list)
  input_list$file_updated <- rep(FALSE, length(input_list))
  names(input_list$file_updated) <- file_names

  return(input_list)
}

#' Read SWAT+ input files which have a tabular structure
#'
#' @param file_path Path of the SWAT+ input file.
#' @param col_names optional column names vector.
#' @param n_skip Number of header rows to skip. Default is 1.
#'
#' @returns The SWAT+ input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr  %>%
#' @importFrom tibble add_column tibble
#'
#' @keywords internal
#'
read_tbl <- function(file_path, col_names = NULL, n_skip = 1) {
  if (file.exists(file_path)) {
    tbl <- fread(file_path, skip = n_skip + 1, header = FALSE)
    if (is.null(col_names)) {
      col_names <- fread(file_path, skip = n_skip, nrows = 1, header = F) %>%
        unlist(.) %>%
        unname(.) %>%
        add_suffix_to_duplicate(.)
    }
    if ('description' %in% col_names & ncol(tbl) == length(col_names) - 1) {
      tbl <- add_column(tbl, description = '')
    } else if (ncol(tbl) > length(col_names)) {
      col_names_add <- paste0('v_', 1:(ncol(tbl) - length(col_names)))
      col_names <- c(col_names, col_names_add)
      warning("Number of columns of '", basename(file_path),"' > column names.\n",
              "Column names ", paste(col_names_add, collapse = ', '),
              ' were assigned to columns at the end.')
    } else if (ncol(tbl) < length(col_names)) {
      col_names_rmv <- col_names[(ncol(tbl) + 1):length(col_names)]
      col_names <- col_names[1:ncol(tbl)]
      warning("Number of columns of '", basename(file_path),"' < column names.\n",
              "Column names ", paste(col_names_rmv, collapse = ', '),
              ' were removed.')
    }

    names(tbl) <- col_names
    tbl <- tibble(tbl)
  } else {
    if (is.null(col_names)) {
      stop("File '", basename(file_path), "' does not exist and no 'col_names' ",
           'were provided to generate empty table.')
    }

    tbl <- tibble(!!!rep(NA, length(col_names)),
                  .rows = 0, .name_repair = ~ col_names)
  }

  return(tbl)
}

#' Read SWAT+ management schedule file.
#'
#' @param file_path Path of the SWAT+ management.sch input file.
#'
#' @importFrom data.table fread
#' @importFrom dplyr bind_rows bind_cols mutate %>%
#' @importFrom purrr map map_int map_chr map2 map2_df map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_replace_all str_trim str_split
#' @importFrom tibble as_tibble
#'
#' @returns The SWAT+ management.sch input file as a tibble.
#'
#' @keywords internal
#'
read_tbl2 <- function(file_path, def_names, par_names, id_num = NULL) {
  n_def <- length(def_names)
  n_par <- length(par_names)

  file_line <- fread(file_path, skip = 2, sep = NULL, sep2 = NULL,
                     header = FALSE) %>%
    unlist(.) %>%
    unname(.) %>%
    str_trim(.) %>%
    str_replace_all(., '\t', ' ') %>%
    str_split(., '[:space:]+')

  n_elem <- map_int(file_line, length)
  file_line  <- file_line[n_elem != 1]
  n_elem <- n_elem[n_elem != 1]
  def_pos <- which(n_elem == n_def)

  pos_start <- def_pos + 1
  pos_end <- c(def_pos[2:length(def_pos)] - 1, length(file_line))
  no_entry <- pos_start > pos_end
  pos_start[no_entry] <- length(file_line) + 1
  pos_end[no_entry] <- length(file_line) + 1

  par_tbl <- map2(pos_start, pos_end, ~ file_line[.x:.y]) %>%
    map(., unlist) %>%
    map(., ~ as_mtx_null(.x, n_par)) %>%
    map(., ~ as_tibble(.x, .name_repair = ~ par_names))

  n_op <- map_int(par_tbl, nrow)

  par_tbl <- bind_rows(par_tbl)

  def_tbl <- map(def_pos, ~ file_line[[.x]]) %>%
    map(., unlist) %>%
    map(., ~ as_mtx_null(.x, n_def)) %>%
    map(., ~ as_tibble(.x, .name_repair = ~ def_names)) %>%
    map2(., n_op, ~ .x[rep(1, .y), ]) %>%
    bind_rows(.)

  tbl <- bind_cols(def_tbl, par_tbl)

  if(!is.null(id_num)) {
    tbl[,id_num] <- map_df(tbl[,id_num], as.numeric)
  }

  return(tbl)
}

#' Read SWAT+ management schedule file.
#'
#' @param file_path Path of the SWAT+ management.sch input file.
#'
#' @importFrom data.table fread
#' @importFrom dplyr bind_rows bind_cols mutate %>%
#' @importFrom purrr map map_int map_chr map2 map2_df map_df set_names
#' @importFrom readr read_lines
#' @importFrom stringr str_replace_all str_trim str_split
#' @importFrom tibble as_tibble
#'
#' @returns The SWAT+ management.sch input file as a tibble.
#'
#' @keywords internal
#'
read_tbl_n <- function(file_path, col_names = NULL, n_skip = 1,
                       id_col_sel = NULL, id_num = NULL) {
  if (is.null(col_names)) {
    col_names <- fread(file_path, skip = n_skip, nrows = 1, header = F) %>%
      unlist(.) %>%
      unname(.) %>%
      add_suffix_to_duplicate(.)
    if (!is.null(id_col_sel)) {
      col_names <- col_names[id_col_sel]
    }
  }

  file_line <- fread(file_path, skip = n_skip + 1, sep = NULL, sep2 = NULL,
                     header = FALSE) %>%
    unlist(.) %>%
    unname(.) %>%
    str_trim(.) %>%
    str_replace_all(., '\t', ' ') %>%
    str_split(., '[:space:]+')

    if (!is.null(id_col_sel)) {
      file_line <- map(file_line, ~ .x[id_col_sel])
    }

  tbl <- file_line %>%
    map(., unlist) %>%
    map(., ~ as_mtx_null(.x, length(file_line[[1]]))) %>%
    map_df(., ~ as_tibble(.x, .name_repair = ~ col_names))

  if(!is.null(id_num)) {
    tbl[,id_num] <- map_df(tbl[,id_num], as.numeric)
  }

  return(tbl)
}

#' Read a SWAT+ connecitivity (*.con) input file.
#'
#' @param file_path Path of the SWAT+ input file.
#'
#' @returns The connecitivity input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr across mutate
#' @importFrom purrr set_names
#' @importFrom stringr str_trim str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyselect matches starts_with
#'
#' @keywords internal
#'
read_con_file <- function(file_path) {
  con_mtx <- fread(file_path, skip = 2, sep = NULL, sep2 = NULL, header = F) %>%
    unlist(.) %>%
    unname(.) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+', simplify = T)

  obj_names <- c("id", "name", "gis_id", "area", "lat", "lon", "elev",
                 "obj_id", "wst", "cst", "ovfl", "rule", "out_tot")
  con_names <- c("obj_typ", "obj_id", "hyd_typ", "frac")
  n_con <- (dim(con_mtx)[2]-length(obj_names)) / length(con_names)
  if(n_con > 0) {
    rep_ids <- 1:n_con
  } else {
    rep_ids <- NULL
  }

  con_names <- paste(rep(con_names, n_con),
                     rep(rep_ids, each = length(con_names)),
                     sep = '_')

  con_tbl <- as_tibble(con_mtx, validate = NULL, .name_repair = 'minimal') %>%
    set_names(c(obj_names, con_names)) %>%
    mutate(area = as.numeric(area)) %>%
    mutate(across(matches('id'), as.integer),
           across(starts_with('frac'), as.numeric))

  return(con_tbl)
}

write_tbl <- function(tbl, file_path, fmt) {
  tbl <- map2_df(tbl, fmt, ~ sprintf(.y, .x))

  n_char <- map_int(tbl[1,], nchar)
  fmt_col_names <- paste0('%', n_char, 's')

  col_names <- colnames(tbl) %>%
    sprintf(fmt_col_names, .) %>%
    paste(., collapse = '  ')

  file_lines <- tbl %>%
    apply(., 1, paste, collapse = '  ') %>%
    str_replace_all(., '  NA', '    ')

  file_head <- 'input file modified to implement ponds'

  input_file <- c(file_head, col_names, file_lines)

  write_lines(input_file, file_path)
}

#' Add a running ID to duplicated names
#'
#' @param col_name Character vector of column names
#'
#' @returns the `col_name` character vector with IDs for duplicated names
#'
#' @keywords internal
#'
add_suffix_to_duplicate <- function(col_name){
  dupl <- table(col_name) %>%
    .[. > 1]

  if(length(dupl > 0)) {
    for(i in 1:length(dupl)) {
      col_name[col_name == names(dupl[i])] <-
        paste0(names(dupl[i]), c('', 1:(dupl[i]-1)))
    }
  }

  return(col_name)
}

#' Transform x to a matrix with 7 columns and fill up with NA values
#'
#' @param x character vector or NULL
#' @param n Number of elements
#'
#' @keywords internal
#'
as_mtx_null <- function(x, n) {
  if(is.null(x)) {
    matrix(rep(NA_character_, 7), ncol = n)
  } else {
    matrix(x, nrow = n) %>%
      t(.)
  }
}
