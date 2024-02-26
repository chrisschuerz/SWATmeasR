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
#' @importFrom dplyr bind_rows distinct left_join select %>%
#' @importFrom purrr map map_chr map_lgl map2
#' @importFrom readr read_csv write_csv
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

  # Load farmR projects ----------------------------------------------------
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

  # Load the scenario farmR projects into a list
  farm_scen <- map(farmr_names, ~ read_farmr(paste(project_path,
                                                   paste0(.x, '.farm'),
                                                   sep = '/'))) %>%
    set_names(farmr_names)

  # Load the farmR project for the status quo
  farm_squo <- read_farmr(paste(project_path,
                                paste0(status_quo, '.farm'),
                                sep = '/'))

  # Compare properties such as the start/end years or number of HRUs
  # to have minor checks if the projects are for the same SWAT+ project.
  compare_scen_quo_properties(farm_scen, farm_squo)

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
  scen_files <- list()

  for (scen_i in farmr_names) {
    cat('Preparing and loading input files for scenario', scen_i, ':\n')

    if(xor(is.null(start_year), is.null(end_year))) {
      stop("Either both 'start_year' and 'end_year' are defined or both are NULL.")
    }

    if(is.null(start_year)) {
      start_year <- farm_scen[[scen_i]]$.data$scheduled_operations$scheduled_years$start_year
      end_year <- farm_scen[[scen_i]]$.data$scheduled_operations$scheduled_years$end_year
    }

    write_farmr_ops(farm_scen[[scen_i]],
    start_year = start_year,
    end_year = end_year)

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

    scen_files[[scen_i]] <- list(hru_data.hru    = hru_data_scen,
                                 landuse.lum     = luse_lum_scen,
                                 management.sch  = mgt_sch_scen,
                                 plant.ini       = plt_ini_scen)
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

  # -------------------------------------------------------------------------
  # Excluded this check again as the land use labels can be different and
  # all matching labels must not necessarily be a requirement.
  # Check if mgt definitions are missing in scenarios -----------------------
  #
  # All scenario are checked if they miss any lu_mgt labels in hru-data
  # which are provided in the status quo.
  # If all farmR projects were set up correctly the landuse labels in all
  # projects should be identical.
  # mgt_not_in_scen <- map(scen_files,
  #                        ~ !.x$hru_data$lu_mgt %in% hru_data_squo$lu_mgt)
  # mgt_scen_miss <- map_lgl(mgt_not_in_scen, ~ any(.x))
  #
  # if (any(mgt_scen_miss)) {
  #   mgts_miss <- map2(scen_files, mgt_not_in_scen,
  #                     ~ .x$hru_data$lu_mgt[.y]) %>%
  #     map_chr(., ~ paste(.x, collapse = ', '))
  #   mgts_miss <- mgts_miss[mgt_scen_miss]
  #   msg <- map2_chr(names(mgts_miss), mgts_miss, ~ paste0(.x, ': ', .y, '\n' ))
  #   stop('The following lu_mgt entries are missing in the scenarios:\n', msg)
  # }
  # -------------------------------------------------------------------------

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
  hru_lum_squo  <- select(hru_data_squo, id, lu_mgt)

  for (scen_i in names(scen_files)) {
    cat('Updating schedule dates for scenario', scen_i, ':\n' )
    names_scen <- factor(scen_files[[scen_i]]$management.sch$name)
    schedule_scen <- split(scen_files[[scen_i]]$management.sch, names_scen)

    hru_lum <- scen_files[[scen_i]]$hru_data.hru %>%
      select(., id, lu_mgt) %>%
      left_join(., hru_lum_squo, by = 'id', suffix = c("_scn", "_quo")) %>%
      distinct(., lu_mgt_scn, .keep_all = T)

    t0 <- now()
    cnt <- 1
    for (i in 1:nrow(hru_lum)) {
      lum_squo_i <- hru_lum$lu_mgt_quo[i]
      lum_scen_i <- hru_lum$lu_mgt_scn[i]
      mgt_squo_i <- luse_lum_squo$mgt[luse_lum_scen$name == lum_squo_i]
      mgt_scen_i <- scen_files[[scen_i]]$landuse.lum$mgt[
        scen_files[[scen_i]]$landuse.lum$name == lum_squo_i]

      if(all(mgt_squo_i != 'null') & all(mgt_scen_i != 'null')) {
        sch_squo <- add_op_date(schedule_squo[[mgt_squo_i]], start_year)
        sch_scen <- add_op_date(schedule_scen[[mgt_scen_i]], start_year)

        if (!is.null(sch_scen)) {
          if (nrow(sch_scen) > 0) {
            date_i_upd <- update_dates(sch_squo, sch_scen, mgt_scen_i, synonyms, 21)
            sch_scen$date <- date_i_upd
            schedule_scen[[mgt_scen_i]] <- date_to_monday(sch_scen)
          }
        }
      }
      display_progress_pct(cnt, nrow(hru_lum), t0, 'Updating schedule dates:')
      cnt <- cnt + 1
    }
    finish_progress(length(schedule_squo), t0, 'Updated', 'schedule')
    scen_files[[scen_i]]$management.sch <- bind_rows(schedule_scen)

    cat('\n')
  }

  scen_files$status_quo <- list(hru_data.hru    = hru_data_squo,
                                landuse.lum     = luse_lum_squo,
                                management.sch  = mgt_sch_squo,
                                plant.ini       = plt_ini_squo)


  time_stamp <- format(Sys.time(), "%Y%m%d_%H%M")
  write_name <- paste0(time_stamp, '_mgt_scenarios')

  cat("Writing", paste0("'", write_name, '.rds', "'"), " to", write_path)

  if(!dir.exists(write_path)) {
    dir.create(write_path, recursive = TRUE)
  }
  saveRDS(scen_files, paste0(write_path, '/', write_name, '.rds'))

  if (write_csv_mgts) {

    mgt_csv_path <- paste0(write_path, '/', write_name)
    dir.create(mgt_csv_path, recursive = TRUE)

    cat("Writing management '.csv' tables to", mgt_csv_path)

    for (scen_i in names(scen_files)) {
      write_csv(scen_files[[scen_i]]$management.sch,
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

#' Add date column to operations table based on start_year and day and month
#' columns.
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

  project_type <- farmr_obj$.data$meta$project_type
  project_type <- ifelse(is.null(project_type), 'database', 'environment')

  if (project_type == 'database') {
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
  if (is.null(farmr_project$.data$meta$project_type)) {
    SWATfarmR:::write_operation(path = farmr_project$.data$meta$project_path,
                                proj_name = farmr_project$.data$meta$project_name,
                                mgt = farmr_project$.data$management$schedule,
                                mgt_raw = farmr_project$.data$meta$mgt_raw,
                                assigned_hrus = farmr_project$.data$scheduled_operations$assigned_hrus,
                                start_year = start_year,
                                end_year = end_year,
                                year_range = farmr_project$.data$scheduled_operations$scheduled_years,
                                version = farmr_project$.data$meta$swat_version)
  } else {
    SWATfarmR:::write_operation(data = farmr_project$.data,
                                start_year = start_year,
                                end_year = end_year)
  }
}


#' Compare start/end years and numbers of HRUs for all scenarios with the status
#' quo.
#'
#' @param farmr_scen List of scenario farmR projects.
#' @param farmr_quo farmR project for the status quo case.
#'
#' @returns Triggeres errors if differences are identified.
#'
#' @importFrom dplyr bind_rows mutate %>%
#' @importFrom purrr map map2
#'
#' @keywords internal
#'
compare_scen_quo_properties <- function(farm_scen, farm_squo) {
  scen_years <- map(farm_scen,
                    ~ .x$.data$scheduled_operations$scheduled_years) %>%
    map2(., names(.), ~ mutate(.x, farmr = .y, .before = 1))

  n_hru_squo <- nrow(farm_squo$.data$scheduled_operations$assigned_hrus)
  n_hru_scen <- map_int(farm_scen,
                        ~ nrow(.x$.data$scheduled_operations$assigned_hrus))

  if(any(n_hru_scen != n_hru_squo)) {
    hru_diff <- names(n_hru_scen)[n_hru_scen != n_hru_squo]
    stop('The following scenarios have a different number of HRUs compared to ',
         'the status quo:\n', paste(hru_diff, collapse = ', '))
  }

  squo_years <- farm_squo$.data$scheduled_operations$scheduled_years
  years_diff <- bind_rows(scen_years)$start_year != squo_years$start_year |
                bind_rows(scen_years)$end_year   != squo_years$end_year

  if(any(years_diff)) {
    scen_years <- scen_years[years_diff]
    fmt_name <- paste0('%-', max(10, nchar(scen_years$farmr)) + 2, 's')
    fmt_vals <- '%12s'
    head_msg <- paste0("The following differences in 'start_year' and ",
                       "'end_year \nbetween the satus quo and the scenarios ",
                       "were identified:\n\n",
                       sprintf(fmt_name, ''), sprintf(fmt_vals, 'start_year'),
                       sprintf(fmt_vals, 'end_year'), '\n')
    quo_msg <- paste0(sprintf(fmt_name,'status_quo'),
                      sprintf(fmt_vals, squo_years$start_year),
                      sprintf(fmt_vals, squo_years$end_year), '\n\n')
    scn_msg <- map_chr(scen_years, ~ paste0(sprintf(fmt_name, .x$farmr),
                                            sprintf(fmt_vals, .x$start_year),
                                            sprintf(fmt_vals, .x$end_year)))
    bot_msg <- '\n\nAll farmR projects must cover the same time interval.'
    stop(head_msg, quo_msg, scn_msg, bot_msg)
  }
}
