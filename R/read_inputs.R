#' Read SWAT+ input files and arrange them in a list object
#'
#' @param project_path Path to the SWAT project folder on the hard drive
#'   (i.e. txtinout folder).
#'
#' @returns A list object with the SWAT+ input files organized in tibbles.
#'
#' @keywords internal
#'
read_swat_inputs <- function(project_path) {
  input_list <- list(
    object.cnt        = read_tbl(paste0(project_path, '/object.cnt')),
    file.cio          = read_cio(paste0(project_path, '/file.cio')),
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
    rout_unit.def     = read_def(paste0(project_path, '/rout_unit.def')),
    rout_unit.ele     = read_tbl(paste0(project_path, '/rout_unit.ele')),
    ls_unit.def       = read_def(paste0(project_path, '/ls_unit.def')),
    ls_unit.ele       = read_tbl(paste0(project_path, '/ls_unit.ele')),
    chandeg.con       = read_con_file(paste0(project_path, '/chandeg.con')),
    reservoir.res     = read_tbl(paste0(project_path, '/reservoir.res'),
                                 col_names = c('id', 'name', 'init', 'hyd',
                                               'rel', 'sed', 'nut'),
                                 col_types = 'icccccc'),
    hydrology.res     = read_tbl(paste0(project_path, '/hydrology.res'),
                                 col_names = c('name', 'yr_op', 'mon_op',
                                               'area_ps', 'vol_ps',
                                               'area_es', 'vol_es', 'k',
                                               'evap_co', 'shp_co1', 'shp_co2'),
                                 col_types = 'ciidddddddd'),
    reservoir.con     = read_con_file(paste0(project_path, '/reservoir.con')
                                      # ,
                                      # col_names = c('id', 'name', 'gis_id',
                                      #               'area', 'lat', 'lon',
                                      #               'elev', 'obj_id', 'wst',
                                      #               'cst', 'ovfl', 'rule',
                                      #               'out_tot'),
                                      # col_types = 'iciddddiciiii'
                                      ),
    wetland.wet       = read_tbl(paste0(project_path, '/wetland.wet'),
                                 col_names = c('id', 'name', 'init', 'hyd',
                                               'rel', 'sed', 'nut'),
                                 col_types = 'icccccc'),
    hydrology.wet     = read_tbl(paste0(project_path, '/hydrology.wet'),
                                 col_names = c('name', 'hru_ps', 'dp_ps',
                                               'hru_es', 'dp_es', 'k', 'evap',
                                               'vol_area_co', 'vol_dp_a',
                                               'vol_dp_b', 'hru_frac'),
                                 col_types = 'cdddddddddd'),
    sediment.res      = read_tbl(paste0(project_path, '/sediment.res')),
    nutrients.res     = read_tbl(paste0(project_path, '/nutrients.res')),
    initial.res       = read_tbl(paste0(project_path, '/initial.res'),
                                 col_names = c('name', 'org_min', 'pest',
                                               'path', 'hmet', 'salt', 'description'),
                                 col_types = 'ccccccc'),
    res_rel.dtl_names = read_dtl_names(paste0(project_path, '/res_rel.dtl')),
    lum.dtl_names     = read_dtl_names(paste0(project_path, '/lum.dtl')),
    tiledrain.str     = read_tbl(paste0(project_path, '/tiledrain.str')),
    bmpuser.str     = read_tbl(paste0(project_path, '/bmpuser.str')),
    filterstrip.str     = read_tbl(paste0(project_path, '/filterstrip.str')),
    grassedww.str     = read_tbl(paste0(project_path, '/grassedww.str'))
  )

  file_names <- names(input_list)
  input_list$file_updated <- rep(FALSE, length(input_list))
  names(input_list$file_updated) <- file_names

  input_list$files_written <- FALSE

  return(input_list)
}

#' Read a SWAT+ input file which has a tabular structure.
#'
#' @param file_path Path of the SWAT+ input file.
#' @param col_names (optional) Column names vector.
#' @param n_skip Number of header rows to skip. Default is 1.
#'
#' @returns The SWAT+ input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr recode %>%
#' @importFrom purrr map2_df
#' @importFrom tibble add_column tibble
#'
#' @keywords internal
#'
read_tbl <- function(file_path, col_names = NULL, col_types = NULL, n_skip = 1) {
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
    col_name_rmv <- str_detect(names(tbl), 'V[:digit:]+$')
    tbl <- tbl[!col_name_rmv]
  } else {
    if (is.null(col_names)) {
      stop("File '", basename(file_path), "' does not exist and no 'col_names' ",
           'were provided to generate empty table.')
    }

    tbl <- tibble(!!!rep(NA, length(col_names)),
                  .rows = 0, .name_repair = ~ col_names)
  }

  if(!is.null(col_types)) {
    col_types <- unlist(strsplit(col_types, '')) %>%
      recode(., c = 'character', d = 'numeric', i = 'integer')
    tbl <- map2_df(tbl, col_types, ~ as(.x, .y))
  }

  return(tbl)
}

#' Read a SWAT+ input file which has a tabular structure with a definition line
#' for each parameter table section (e.g. management.sch, plant.ini, soils.sol).
#'
#' @param file_path Path of the SWAT+ input file.
#' @param def_names Vector of column names for the entries in the definition
#'   line.
#' @param par_names Vector of parameter names of the parameter table.
#' @param id_num ID vector to define the columns which are numerical values.
#'
#' @returns The SWAT+ management.sch input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr bind_rows bind_cols mutate %>%
#' @importFrom purrr map map_int map_chr map2 map2_df map_df set_names
#' @importFrom stringr str_replace_all str_trim str_split
#' @importFrom tibble as_tibble
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
  pos_end <- pos_end[!pos_end %in% c(NA, 0)]
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

#' Read the n column of a tabular SWAT+ input file which are defined by the
#' column positions `id_col_sel`. This is useful if e.g. last columns with
#' description cause issues with reading due to blanks in the description text.
#'
#' @param file_path Path of the SWAT+ input file.
#' @param col_names (optional) Character column names vector.
#' @param n_skip Number of header rows to skip. Default is 1.
#' @param id_col_sel Numeric vector which defines the column positions that are
#'   returned in the table.
#' @param id_num ID vector to define the columns which are numerical values.
#'
#' @returns The SWAT+ management.sch input file as a tibble.
#'
#' @importFrom data.table fread
#' @importFrom dplyr bind_rows bind_cols mutate %>%
#' @importFrom purrr map map_chr map_df
#' @importFrom stringr str_replace_all str_trim str_split
#' @importFrom tibble as_tibble
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
  obj_names <- c("id", "name", "gis_id", "area", "lat", "lon", "elev",
                 "obj_id", "wst", "cst", "ovfl", "rule", "out_tot")
  con_names <- c("obj_typ", "obj_id", "hyd_typ", "frac")

  if(file.exists(file_path)) {
    con_mtx <- fread(file_path, skip = 2, sep = NULL, sep2 = NULL, header = F) %>%
      unlist(.) %>%
      unname(.) %>%
      str_trim(.) %>%
      str_split(., '[:space:]+', simplify = T)

    n_con <- (dim(con_mtx)[2]-length(obj_names)) / length(con_names)
    if(n_con > 0) {
      rep_ids <- 1:n_con
    } else {
      rep_ids <- NULL
    }

    con_names <- paste(rep(con_names, n_con),
                       rep(rep_ids, each = length(con_names)),
                       sep = '_')

    col_types <- unlist(strsplit(c('iciddddiciiii', rep('cicd', n_con)), '')) %>%
      recode(., c = 'character', d = 'numeric', i = 'integer')

    con_tbl <- as_tibble(con_mtx, validate = NULL,
                         .name_repair = ~ c(obj_names, con_names)) %>%
      map2_df(., col_types, ~ as(.x, .y))

    # id_int <- c(1,3,8,13, 15 + (rep_ids - 1)*4)
    # con_tbl[ , id_int] <- map_df(con_tbl[ , id_int], as.integer)
    #
    # id_dbl <- c(4:7, 17 + (rep_ids - 1)*4)
    # con_tbl[ , id_dbl] <- map_df(con_tbl[ , id_dbl], as.numeric)
  } else {
    con_tbl <- tibble(!!!rep(NA, length(obj_names)),
                      .rows = 0, .name_repair = ~ obj_names)

    col_types <- unlist(strsplit('iciddddiciiii', '')) %>%
      recode(., c = 'character', d = 'numeric', i = 'integer')

    con_tbl <- map2_df(con_tbl, col_types, ~ as(.x, .y))
  }

  return(con_tbl)
}

#' Read a SWAT+ .def input file.
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
read_def <- function(file_path) {
  if(grepl('rout_unit.def', file_path)) {
    n_skip <- 2
    col_names <- c('id', 'name', 'elem_tot')
  } else {
    n_skip <- 3
    col_names <- c('id', 'name', 'area', 'elem_tot')
  }

  def_mtx <- fread(file_path, skip = n_skip,
                   sep = NULL, sep2 = NULL, header = F) %>%
    unlist(.) %>%
    unname(.) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+', simplify = T)

  def_tbl <- def_mtx[, 1:length(col_names)] %>%
    as_tibble(., .name_repair = ~col_names) %>%
    mutate(id = as.integer(id),
           elem_tot = as.integer(elem_tot))

  if('area' %in% colnames(def_tbl)) {
    def_tbl$area <- as.numeric(def_tbl$area)
  }

  def_elem <- def_mtx[, (length(col_names) + 1):ncol(def_mtx)] %>%
    t() %>%
    split(., rep(1:ncol(.), each = nrow(.))) %>%
    map(., paste_expression) %>%
    map(., ~eval(parse(text=.x))) %>%
    unname()

  def_tbl$elem <- def_elem

  return(def_tbl)
}

#' Build vector expression for parsing
#'
#' @param x character vector.
#'
#' @returns A character string of structure c(...).
#'
#' @keywords internal
#'
paste_expression <- function(x) {
  x[1] <- paste0('c(', x[1])
  x[length(x)] <- paste0(x[length(x)], ')')
  x <- paste(x, collapse = ',')
  x <- gsub(',-', ':', x)
  x <- gsub(',)', ')', x)
  return(x)
}

#' Read the names of decision table definitions from a SWAT+ dtl input file.
#'
#' @param file_path Path of the SWAT+ input file.
#'
#' @returns A character vector with the names of the defined decision rule sets.
#'
#' @importFrom dplyr  %>%
#' @importFrom purrr map_lgl map_chr
#' @importFrom stringr str_detect str_split str_trim
#'
#' @keywords internal
#'
read_dtl_names <- function(file_path) {
  dtl <- readLines(file_path) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+')

  is_dtl_def <- which(map_lgl(dtl, ~ length(.x) == 4 &
                                str_detect(.x[2], '[:digit:]+') &
                                str_detect(.x[3], '[:digit:]+') &
                                str_detect(.x[4], '[:digit:]+')))

  dtl_names <- map_chr(dtl[is_dtl_def], ~.x[1])

  return(dtl_names)
}

#' Read the SWAT+ model's file.cio.
#'
#' @param file_path Path of the SWAT+ file.cio
#'
#' @returns a named list with the lines of file.cio as character vectors.
#'
#' @importFrom dplyr  %>%
#' @importFrom purrr map_chr
#' @importFrom stringr str_split str_trim
#'
#' @keywords internal
#'
read_cio <- function(file_path) {
  file_cio <- readLines(file_path)
  # file_head <- file_cio[1]

  file_cio <- file_cio[2:length(file_cio)] %>%
    str_trim(.) %>%
    str_split(., '[:space:]+')
  entry_names <- map_chr(file_cio, ~ .x[1])
  names(file_cio) <- entry_names

  return(file_cio)
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

#' Transform x to a matrix with n columns and fill up with NA values
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
