#' Prepare the management input data from SWATfarmR project as inputs
#' for the definition of management related NSWRMs with
#' `measr_project$load_nswrm_definition()`.
#'
#' @param project_path Path to the SWAT project folder on the hard drive
#'   (i.e. txtinout folder).
#' @param status_quo Name of the SWATfarmR project which provides the status
#'   quo.
#' @param synonyms Optional table to define synonymous op_data1 definitions for
#'   op_types in status quo and a scenario.
#' @param write_path Optional path to write the prepared management inputs.
#' @param write_csv_mgts Should the management tables with corrected dates be
#'   written as csv files (default is `FALSE`). This can be useful to compare
#'   and check the corrected dates of the scenario management tables.
#'
#' @returns Writes an '.rds' file into the `write_path` which must be used as
#'   the input file to define 'management' related NSWRMs in
#'   `measr_project$load_nswrm_definition()`. The file name has the following
#'   structure '<yyyymmdd_hhmm>_management_scenarios.rds'. Optionally also
#'   the management schedules for the scenarios and the status quo are written
#'   as '.csv' files into the folder '<yyyymmdd_hhmm>_management_scenarios'.
#'
#' @importFrom dplyr bind_rows %>%
#' @importFrom purrr map map_chr map_lgl map2
#' @importFrom readr read_csv write_csv write_rds
#' @importFrom stringr str_remove
#'
#' @export
#'
prepare_management_scenario_inputs <- function(project_path, status_quo,
                                               scenarios = NULL, synonyms = NULL,
                                               start_year = NULL, end_year = NULL,
                                               write_path = project_path,
                                               write_csv_mgts = FALSE) {

  # Load synonyms if provided as a path -------------------------------------
  if(is.character(synonyms)) {
    synonyms <- read_csv(synonyms)
  }
  if(!is.null(synonyms)) {
    if (any(names(synonyms) != c('status_quo', 'scenario'))) {
      stop("The 'synonyms' input table must have the two columns",
           "'status_quo' and 'scenario'.")
    }
  }

  # Load scenario farmR -----------------------------------------------------
  #
  # Exclude the status quo from the scenario names.
  # If scenarios is not defined the routine uses all farmR projects which
  # were found in project_path.
  if(is.null(scenarios)) {
    farmr_names <- list.files(project_path, pattern = '.farm$') %>%
      str_remove(., '.farm$')
    farmr_names <- farmr_names[farmr_names != status_quo]
  } else {
    farmr_names <- str_remove(scenarios, '.farm$')
  }

  scen_files <- list()

  # For all scenarios the following workflow is executed:
  # - load each found farmR project
  # - If start/end year was not defined the maximum range of scheduled years
  #   from the first scenario is used for all farmR projects (if everything
  #   is correct all farmR projects should have the same date range)
  # - the scheduled operations are written to the project folder
  # - land use labels are shortened to 16 characters (although this is not
  #   necessary anymore and 24 characters are now possible, this is done
  #   to leave some space for adding ids to the text strings).
  # - The input tables hru-data, landuse.lum, management.sch, and plant.ini
  #   are read from the project folder and saved in a list object.
  #
  for (i in 1:length(farmr_names)) {
    cat('Preparing and loading input files for scenario', farmr_names[i], ':\n')
    farm_scen <- read_farmr(paste(project_path,
                                  paste0(farmr_names[i], '.farm'),
                                  sep = '/'))

    if(xor(is.null(start_year), is.null(end_year))) {
      stop("Either both 'start_year' and 'end_year' are defined or both are NULL.")
    }

    if(is.null(start_year) & i == 1) {
      start_year <- farm_scen$.data$scheduled_operations$scheduled_years$start_year
      end_year <- farm_scen$.data$scheduled_operations$scheduled_years$end_year
    }

    write_farmr_ops(farm_scen, start_year = start_year, end_year = end_year)

    update_landuse_labels(project_path)

    hru_data_scen <- read_tbl(paste0(project_path, '/hru-data.hru'))
    luse_lum_scen <- read_tbl(paste0(project_path, '/landuse.lum'))

    mgt_sch_scen  <- read_tbl2(file_path = paste0(project_path, '/management.sch'),
                               def_names = c('name', 'numb_ops', 'numb_auto'),
                               par_names = c('op_typ', 'mon', 'day', 'hu_sch',
                                             paste0('op_data', 1:3)),
                               id_num    =  c(2:3, 5:7, 10))
    plt_ini_scen <- read_tbl2(file_path = paste0(project_path, '/plant.ini'),
                              def_names = c('pcom_name', 'plt_cnt', 'rot_yr_ini'),
                              par_names = c('plt_name', 'lc_status', 'lai_init',
                                            'bm_init', 'phu_init', 'plnt_pop',
                                            'yrs_init', 'rsd_init'),
                              id_num    =  c(2:3, 6:11))

    scen_files[[farmr_names[i]]] <- list(hru_data = hru_data_scen,
                                         luse_lum = luse_lum_scen,
                                         mgt_sch  = mgt_sch_scen,
                                         plt_ini  = plt_ini_scen)
    cat('\n')
  }

  # Load status quo files ---------------------------------------------------
  #
  # The same workflow as for the scenarios is implemented for the status quo
  # farmR project.
  # The written farmR input files will be left in the SWAT+ project folder
  # to guarantee that the scheduled operations in the SWAT+ project match
  # the operations which are prepared for the scenarios.
  #
  cat('Preparing and loading input files for status quo:\n')
  farm_squo <- read_farmr(paste(project_path,
                          paste0(status_quo, '.farm'),
                          sep = '/'))

  write_farmr_ops(farm_squo, start_year = start_year, end_year = end_year)

  update_landuse_labels(project_path)

  hru_data_squo <- read_tbl(paste0(project_path, '/hru-data.hru'))
  luse_lum_squo <- read_tbl(paste0(project_path, '/landuse.lum'))

  mgt_sch_squo  <- read_tbl2(file_path = paste0(project_path, '/management.sch'),
                             def_names = c('name', 'numb_ops', 'numb_auto'),
                             par_names = c('op_typ', 'mon', 'day', 'hu_sch',
                                           paste0('op_data', 1:3)),
                             id_num    =  c(2:3, 5:7, 10))
  plt_ini_squo <- read_tbl2(file_path = paste0(project_path, '/plant.ini'),
                            def_names = c('pcom_name', 'plt_cnt', 'rot_yr_ini'),
                            par_names = c('plt_name', 'lc_status', 'lai_init',
                                          'bm_init', 'phu_init', 'plnt_pop',
                                          'yrs_init', 'rsd_init'),
                            id_num    =  c(2:3, 6:11))
  cat('\n')

  # Check if mgt definitions are missing in scenarios -----------------------
  #
  # All scenario are checked if they miss any lu_mgt labels in hru-data
  # which are provided in the status quo.
  # If all farmR projects were set up correctly the landuse labels in all
  # projects should be identical.
  mgt_not_in_scen <- map(scen_files,
                         ~ !.x$hru_data$lu_mgt %in% hru_data_squo$lu_mgt)
  mgt_scen_miss <- map_lgl(mgt_not_in_scen, ~ any(.x))

  if (any(mgt_scen_miss)) {
    mgts_miss <- map2(scen_files, mgt_not_in_scen,
                      ~ .x$hru_data$lu_mgt[.y]) %>%
      map_chr(., ~ paste(.x, collapse = ', '))
    mgts_miss <- mgts_miss[mgt_scen_miss]
    msg <- map2_chr(names(mgts_miss), mgts_miss, ~ paste0(.x, ': ', .y, '\n' ))
    stop('The following lu_mgt entries are missing in the scenarios:\n', msg)
  }

  # Date update routine -----------------------------------------------------
  #
  # Dates for scheduled operations which are not different in a scenario should
  # be the same as the respective operation in the status quo.
  # This routine goes through all scheduled operations and assigns the date of
  # the status quo schedule to operations in a scenario if it was identified as
  # the same operation.
  # In some cases operation types remain to be the same but the operation
  # parametrization changes, e.g. the tillage type. If an operation should
  # still be considered as the same operation, synonyms can be defined with
  # the synonyms input table.
  #
  names_squo <- factor(mgt_sch_squo$name)
  schedule_squo <- split(mgt_sch_squo, names_squo)

  for (scen_i in names(scen_files)) {
    cat('Updating schedule dates for scenario', scen_i, ':\n' )
    names_scen <- factor(scen_files[[scen_i]]$mgt_sch$name)
    schedule_scen <- split(scen_files[[scen_i]]$mgt_sch, names_scen)

    t0 <- now()
    cnt <- 1
    for (i in names(schedule_squo)) {
      sch_quo <- add_op_date(schedule_squo[[i]], start_year)
      sch_scn <- add_op_date(schedule_scen[[i]], start_year)

      if (!is.null(sch_scn)) {
        if (nrow(sch_scn) > 0) {
          date_i_upd <- update_dates(sch_quo, sch_scn, syn, 21, i)
          sch_scn$date <- date_i_upd
          schedule_scen[[i]] <- date_to_monday(sch_scn)
        }
      }
      display_progress_pct(cnt, length(schedule_squo), t0, 'Updating schedule dates:')
      cnt <- cnt + 1
    }
    finish_progress(length(schedule_squo), t0, 'Updated', 'schedule')
    scen_files[[scen_i]]$mgt_sch <- bind_rows(schedule_scen)

    cat('\n')
  }
  time_stamp <- format(Sys.time(), "%Y%m%d_%H%M")

  cat("Writing 'mgt_scenarios.rds' to", write_path)
  if(!dir.exists(write_path)) {
    dir.create(write_path, recursive = TRUE)
  }
  write_rds(scen_files, paste0(project_path, '/', time_stamp, '_mgt_scenarios.rds'))

  if (write_csv_mgts) {

    mgt_csv_path <- paste0(write_path, '/', time_stamp, '_management_scenarios')
    dir.create(mgt_csv_path, recursive = TRUE)

    cat("Writing management '.csv' tables to", mgt_csv_path)
    write_csv(mgt_sch_squo, paste0(mgt_csv_path, '/status_quo.csv'))

    for (scen_i in names(scen_files)) {
      write_csv(scen_files[[scen_i]]$mgt_sch,
                paste0(mgt_csv_path, '/', scen_i, '.csv'))
    }
  }
}

#' Update operation dates for scheduled scenario operations with the dates
#' of the respective operations in the status quo management schedule.
#'
#' @param sch_quo Operation schedule table for the status quo.
#' @param sch_scn Operation schedule table for the scenario case.
#' @param sch_name Name of the operation schedule.
#' @param synonyms Synonyms input table which defines synonymous operations.
#' @param max_day_diff Maximum difference in days between scheduled operations
#'   to consider them as being the same one.
#'
#' @returns A date vector with the corrected dates for the scenario schedule.
#'
#' @importFrom dplyr if_else mutate %>%
#' @importFrom tibble tibble
#'
#' @keywords internal
#'
update_dates <- function(sch_quo, sch_scn, sch_name,
                         synonyms = NULL, max_day_diff = 21) {

  n_quo <- nrow(sch_quo)
  n_scn <- nrow(sch_scn)

  n_op_equal <- n_quo == n_scn

  if (n_op_equal) {
    is_different <- any(sch_quo$date != sch_scn$date)
  } else {
    is_different <- TRUE
  }

  if(is_different) {
    n_break <- max(nrow(sch_quo), nrow(sch_scn))

    sch_quo$op_data1 <- if_else(is.na(sch_quo$op_data1), 'null', sch_quo$op_data1)
    sch_scn$op_data1 <- if_else(is.na(sch_scn$op_data1), 'null', sch_scn$op_data1)

    if(!is.null(synonyms)) {
      for (i in 1:nrow(synonyms)) {
        sch_quo$op_data1 <- if_else(sch_quo$op_data1 == synonyms[[1]][i],
                                    synonyms[[2]][i] , sch_quo$op_data1)
      }
    }

    date_diff <- outer(as.numeric(unlist(sch_quo$date)),
                       as.numeric(unlist(sch_scn$date)), '-')
    op_diff <- outer(unlist(sch_quo$op_typ),
                     unlist(sch_scn$op_typ), '==')
    op_diff[!op_diff] <- Inf
    op1_diff <- outer(unlist(sch_quo$op_data1),
                      unlist(sch_scn$op_data1), '==')
    op1_diff[!op1_diff] <- Inf

    same_op <- abs(date_diff) + op_diff + op1_diff

    in_rng  <- apply(same_op, 2, min) < max_day_diff
    date_id <- apply(same_op, 2, which.min)
    date_id[!in_rng] <- NA

    dates <- tibble(quo = sch_quo$date[date_id] ,
                    cc = sch_scn$date) %>%
      mutate(upd = if_else(is.na(quo), cc, quo),
             dif = c(as.numeric(diff(upd)), 0),
             has_quo = !is.na(quo),
             is_cc = is.na(quo))

    has_neg_dif <- any(dates$dif < 0)

    n <- 0

    while (has_neg_dif | n > n_break) {
      id_neg_dif_lead <- which(dates$dif < 0) + 1
      id_not_quo_lead <- which(!dates$has_quo) + 1
      id_is_cc_lead <- which(dates$is_cc) + 1

      id_chg_lead <- id_neg_dif_lead[id_neg_dif_lead %in% id_not_quo_lead]
      id_chg_lead <- unique(c(id_chg_lead,
                              id_neg_dif_lead[id_neg_dif_lead %in% id_is_cc_lead]))

      dates$upd[dates$dif < 0 & dates$has_quo & !dates$is_cc] <-
        dates$cc[dates$dif < 0 & dates$has_quo & !dates$is_cc]

      dates$upd[id_chg_lead] <- dates$cc[id_chg_lead]

      dates$is_cc <- dates$upd == dates$cc

      dates$dif <- c(as.numeric(diff(dates$upd)), 0)
      has_neg_dif <- any(dates$dif < 0)
      n <- n + 1
    }

    if(n == n_break) {
      stop('Date conflicts cannot be resolved for schedule ', sch_name)
    }

    dates_upd <- dates$upd
  } else {
    dates_upd <- sch_scn$date
  }
  return(dates_upd)
}

#' Title
#'
#' @param tbl Operation schedule table
#' @param start_year Start year for the operation schedule
#'
#' @returns The table with an added date column
#'
#' @importFrom dplyr lag mutate %>%
#' @importFrom lubridate ymd
#'
#' @keywords internal
#'
add_op_date <- function(tbl, start_year) {
  tbl %>%
    mutate(.,
           year = ifelse(op_typ == 'skip', 1, 0),
           year = lag(year, default = 0),
           year = cumsum(year),
           year = year + start_year,
           mon  = ifelse(op_typ == 'skip', 12, mon),
           day  = ifelse(op_typ == 'skip', 31, day),
           date = ymd(paste(year, mon, day, sep = '-'))) %>%
    select(-year)
}

#' Convert dates to numeric month and day values
#'
#' @param tbl Operation schedule table.
#'
#' @return The table without date column and updated month and day columns.
#'
#' @importFrom dplyr mutate select %>%
#' @importFrom lubridate month day
#'
#' @keywords internal
#'
date_to_monday <- function(tbl) {
  tbl %>%
    mutate(.,
           mon = month(date),
           day = day(date),
           mon = ifelse(op_typ == 'skip', 0, mon),
           day = ifelse(op_typ == 'skip', 0, day),
    ) %>%
    select(-date)
}

#' Load a farmR project
#'
#' @param file '.farm' file
#'
#' @return A loaded farmR environment
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbReadTable
#' @importFrom dplyr mutate %>%
#' @importFrom lubridate ymd
#' @importFrom tibble tibble
#' @importFrom RSQLite SQLite
#'
#' @keywords internal
#'
read_farmr <- function(file) {
  project_path <- dirname(file)
  project_name <- strsplit(basename(file), "\\.")[[1]][1]
  file_ext     <- strsplit(basename(file), "\\.")[[1]][2]

  farmr_obj <- readRDS(file)
  farmr_obj$.data$meta$project_path <- project_path
  farmr_obj$.data$meta$project_name <- project_name

  mgts_path <- paste0(project_path, '/', project_name, '.mgts')
  if(file.exists(mgts_path)) {
    mgt_db <- dbConnect(SQLite(), mgts_path)
    mgt_tbls <- dbListTables(mgt_db)
    farmr_obj$.data$scheduled_operations$scheduled_years <-
      dbReadTable(mgt_db, 'scheduled_years') %>%
      tibble(.)
    if('skipped_operations' %in% mgt_tbls) {
      farmr_obj$.data$scheduled_operations$skipped_operations <-
        dbReadTable(mgt_db, 'skipped_operations') %>%
        tibble(.) %>%
        mutate(date_prev_op = ymd(19700101) + date_prev_op)
    }
    dbDisconnect(mgt_db)
  }

  return(farmr_obj)
}

#' Write scheduled opeartions from farmR project into project folder
#'
#' @param farmr_project SWATfarmR project environment
#' @param start_year Start year
#' @param end_year End year
#'
#' @returns Writes the SWAT+ input files which are relevant for management
#'   scheduling into the project path
#'
#' @keywords internal
#'
write_farmr_ops <- function(farmr_project, start_year, end_year) {
  SWATfarmR:::write_operation(path = farmr_project$.data$meta$project_path,
                              proj_name = farmr_project$.data$meta$project_name,
                              mgt = farmr_project$.data$management$schedule,
                              mgt_raw = farmr_project$.data$meta$mgt_raw,
                              assigned_hrus = farmr_project$.data$scheduled_operations$assigned_hrus,
                              start_year = start_year,
                              end_year = end_year,
                              year_range = farmr_project$.data$scheduled_operations$scheduled_years,
                              version = farmr_project$.data$meta$swat_version)
}
