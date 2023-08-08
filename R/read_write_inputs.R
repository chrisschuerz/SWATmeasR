read_swat_inputs <- function(project_path) {
  input_list <- list(
    object.cnt    = read_tbl(paste0(project_path, '/object.cnt')),
    landuse.lum   = read_tbl(paste0(project_path, '/landuse.lum')),
    hru_data.hru  = read_tbl(paste0(project_path, '/hru-data.hru')),
    hru.con       = read_con_file(paste0(project_path, '/hru.con')),
    rout_unit.rtu = read_tbl(paste0(project_path, '/rout_unit.rtu')),
    rout_unit.con = read_con_file(paste0(project_path, '/rout_unit.con')),
    rout_unit.def = read_tbl(paste0(project_path, '/rout_unit.def')),
    rout_unit.ele = read_tbl(paste0(project_path, '/rout_unit.ele')),
    chandeg.con   = read_con_file(paste0(project_path, '/chandeg.con')),
    reservoir.res = read_tbl(paste0(project_path, '/reservoir.res')),
    hydrology.res = read_tbl(paste0(project_path, '/hydrology.res')),
    reservoir.con = read_con_file(paste0(project_path, '/reservoir.con'))
  )

  return(input_list)
}

#' Read SWAT+ input files which have a tabular structure
#'
#' @param file_path Path of the SWAT+ input file
#' @param col_names optional column names vector
#' @param n_skip Number of header rows to skip. Default is 1.
#'
#' @returns The SWAT+ input file as a tibble
#'
#' @importFrom data.table fread
#' @importFrom dplyr  %>%
#' @importFrom tibble add_column tibble
#'
#' @export
#'
#' @examples
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

#' Read a SWAT+ connecitivity (*.con) input file.
#'
#' @param file_path Path of the SWAT+ input file
#'
#' @returns The connecitivity input file as a tibble
#'
#' @importFrom data.table fread
#' @importFrom dplyr across mutate
#' @importFrom purrr set_names
#' @importFrom stringr str_trim str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyselect matches starts_with
#'
#' @export
#'
#' @examples
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
    mutate(across(matches('id'), as.integer),
           across(starts_with('frac'), as.numeric))

  return(con_tbl)
}

write_input_tbl <- function(tbl, file_path, fmt) {
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
#' @examples
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
