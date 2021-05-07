# Splitr hysplit_trajectory_mod() function to solve sys.time/sys.locale errors when calling gdas1 data on certain PCs

# alt+O to collapse sections

# MH 02/09/20

# This script contains:
# - utility functions from Splitr that hysplit_trajectory() relies on
# - the original hysplit_trajectory function
# - a tweaked version of hysplit_trajectory with solved gdas1 sys.time/sys.locale errors

# to use the new function, run the utility functions then just use hysplit_trajectory_mod() as you would
# normally use hysplit_trajectory(). Same parameters, inputs, etc. 

##### Splitr Utility Functions [ALWAYS RUN] #####

# from various source pages, such as: https://rdrr.io/github/rich-iannone/SplitR/src/R/utils.R

#set_ascdata()
set_ascdata <- function(lat_lon_ll = c(-90.0, -180.0),
                        lat_lon_spacing = c(1.0, 1.0),
                        lat_lon_n = c(180, 360),
                        lu_category = 2,
                        roughness_l = 0.2,
                        data_dir = "'.'") {
  
  arg_names <- formals(set_ascdata) %>% names()
  arg_vals <- mget(arg_names)
  
  arg_vals$lat_lon_ll <- 
    paste0(arg_vals$lat_lon_ll[1], "  ", arg_vals$lat_lon_ll[2])
  
  arg_vals$lat_lon_spacing <- 
    paste0(arg_vals$lat_lon_spacing[1], "  ", arg_vals$lat_lon_spacing[2])
  
  arg_vals$lat_lon_n <- 
    paste0(arg_vals$lat_lon_n[1], "  ", arg_vals$lat_lon_n[2])
  
  arg_vals[!vapply(arg_vals, FUN = is.null, FUN.VALUE = logical(1))]
}

# write_config_list()
write_config_list <- function(config_list, dir) {
  
  paste0(
    "&SETUP\n",
    paste0(names(config_list), " = ", config_list, ",\n", collapse = ""),
    "/\n"
  ) %>%
    cat(file = file.path(dir, "SETUP.CFG"))
}

#write_ascdata_list()
write_ascdata_list <- function(ascdata_list, dir) {
  
  paste0(ascdata_list, "\n", collapse = "") %>%
    cat(file = file.path(dir, "ASCDATA.CFG"))
}

#download_met_files()

download_met_files <- function(met_type,
                               days,
                               duration,
                               direction,
                               met_dir) {
  
  if (met_type == "gdas1") {
    
    met_files <-
      get_met_gdas1(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gdas0.5") {
    
    met_files <-
      get_met_gdas0p5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gfs0.25") {
    
    met_files <-
      get_met_gfs0p25(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "reanalysis") {
    
    met_files <-
      get_met_reanalysis(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "nam12") {
    
    met_files <-
      get_met_nam12(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "narr") {
    
    met_files <-
      get_met_narr(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "era5") {
    
    met_files <-
      get_met_era5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  met_files
}

# get_receptor_values()
get_receptor_values <- function(receptors_tbl,
                                receptor_i) {
  
  receptors_tbl[receptor_i, ] %>% as.list()
}

#to_short_year()
to_short_year <- function(date) {
  
  date %>%
    lubridate::year() %>%
    as.character() %>%
    substr(3, 4)
}

#to_short_month()
to_short_month <- function(date) {
  
  formatC(
    date %>% lubridate::month(),
    width = 2, flag = "0"
  )
}

#to_short_day()
to_short_day <- function(date) {
  
  formatC(
    date %>% lubridate::day(),
    width = 2, flag = "0"
  )
}

#get_traj_output_filename()
get_traj_output_filename <- function(traj_name,
                                     site,
                                     direction,
                                     year,
                                     month,
                                     day,
                                     hour,
                                     lat,
                                     lon,
                                     height,
                                     duration) {
  
  paste0(
    "traj-",
    ifelse(is.null(traj_name), "", traj_name),
    "-",
    ifelse(direction == "backward", "bwd", "fwd"), "-",
    year, "-",
    month, "-",
    day, "-",
    hour, "-",
    site,
    "lat_", gsub("\\.", "p", as.character(lat)), "_",
    "lon_", gsub("\\.", "p", as.character(lon)), "-",
    "hgt_", height, "-",
    duration, "h"
  )
}

#write_traj_control_file()
write_traj_control_file <- function(start_year_GMT,
                                    start_month_GMT,
                                    start_day_GMT,
                                    start_hour_GMT,
                                    lat,
                                    lon,
                                    height,
                                    direction,
                                    duration,
                                    vert_motion,
                                    model_height,
                                    met_files,
                                    output_filename,
                                    system_type,
                                    met_dir,
                                    exec_dir) {
  
  paste0(
    start_year_GMT, " ", start_month_GMT, " ",
    start_day_GMT, " ", start_hour_GMT, "\n",
    "1\n",
    lat, " ", lon, " ", height, "\n",
    ifelse(direction == "backward", "-", ""), duration, "\n",
    vert_motion, "\n",
    model_height, "\n",
    length(met_files), "\n",
    paste0(met_dir, "/\n", met_files, collapse = "\n"), "\n",
    exec_dir, "/\n",
    output_filename, "\n"
  ) %>%
    cat(file = file.path(exec_dir, "CONTROL"), sep = "", append = FALSE)
}

#execute_on_system()
execute_on_system <- function(sys_cmd, system_type) {
  
  if (system_type %in% c("mac", "unix")) {
    system(sys_cmd)
  } else if (system_type == "win") {
    shell(sys_cmd)
  }
}

#to_null_dev()
to_null_dev <- function(system_type) {
  
  if (system_type %in% c("mac", "unix")) {
    null_dev <- ">> /dev/null 2>&1"
  } else if (system_type == "win") {
    null_dev <- "> NUL 2>&1"
  }
  
  null_dev
}

#tidy_grepl()
tidy_grepl <- function(x, pattern) {
  
  vapply(
    pattern,
    FUN = function(pattern) {
      grepl(pattern = pattern, x = x)
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}

#get_os()
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    return("win")
  } else if (Sys.info()["sysname"] == "Darwin") {
    return("mac")
  } else if (.Platform$OS.type == "unix") {
    return("unix")
  } else {
    stop("Unknown OS", call. = FALSE)
  }
}

download_met_files <- function(met_type,
                               days,
                               duration,
                               direction,
                               met_dir) {
  
  if (met_type == "gdas1") {
    
    met_files <-
      get_met_gdas1(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gdas0.5") {
    
    met_files <-
      get_met_gdas0p5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gfs0.25") {
    
    met_files <-
      get_met_gfs0p25(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "reanalysis") {
    
    met_files <-
      get_met_reanalysis(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "nam12") {
    
    met_files <-
      get_met_nam12(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "narr") {
    
    met_files <-
      get_met_narr(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "era5") {
    
    met_files <-
      get_met_era5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  met_files
}

get_met_gdas1 <- function(days,
                          duration,
                          direction,
                          path_met_files) {
  
  # Determine the minimum date (as a `Date`) for the model run
  if (direction == "backward") {
    min_date <- 
      (lubridate::as_date(days[1]) - (duration / 24)) %>%
      lubridate::floor_date(unit = "day")
  } else if (direction == "forward") {
    min_date <- 
      (lubridate::as_date(days[1])) %>%
      lubridate::floor_date(unit = "day")
  }
  
  # Determine the maximum date (as a `Date`) for the model run
  if (direction == "backward") {
    max_date <- 
      (lubridate::as_date(days[length(days)])) %>%
      lubridate::floor_date(unit = "day")
  } else if (direction == "forward") {
    max_date <- 
      (lubridate::as_date(days[length(days)]) + (duration / 24)) %>%
      lubridate::floor_date(unit = "day")
  }
  
  met_days <- 
    seq(min_date, max_date, by = "1 day") %>% 
    lubridate::day()
  
  month_names <- 
    seq(min_date, max_date, by = "1 day") %>%
    lubridate::month(label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))  %>%
    as.character() %>%
    tolower()
  
  #month_names <- 
  #  seq(min_date, max_date, by = "1 day") %>%
  #  lubridate::month(label = TRUE, abbr = TRUE, locale = "en_US.UTF-8")  %>%
  #  as.character() %>%
  #  tolower()
  
  
  met_years <- 
    seq(min_date, max_date, by = "1 day") %>%
    lubridate::year() %>% 
    substr(3, 4)
  
  # Only consider the weeks of the month we need:
  #.w1 - days 1-7
  #.w2 - days 8-14
  #.w3 - days 15-21
  #.w4 - days 22-28
  #.w5 - days 29 - rest of the month 
  
  met_week <- ceiling(met_days / 7)
  
  files <- paste0("gdas1.", month_names, met_years, ".w", met_week) %>% unique()
  
  get_met_files(
    files = files,
    path_met_files = path_met_files,
    ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/gdas1"
  )
  
}

get_met_files <- function(files, path_met_files, ftp_dir) {
  
  # Determine which met files are already locally available
  files_in_path <- list.files(path_met_files)
  
  # Download list of GFS0.25 met files by name
  if (!is.null(files)) {
    
    for (file in files) {
      
      if (!(file %in% files_in_path)) {
        
        downloader::download(
          url = file.path(ftp_dir, file),
          destfile = path.expand(file.path(path_met_files, file)),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE
        )
      }
    }
  }
  
  files
}

##### Original hysplit_trajectory() function #####

# result returned from getAnywhere(hysplit_trajectory)

hysplit_trajectory <- function (lat = 49.263, lon = -123.25, height = 50, duration = 24, 
                                days = NULL, daily_hours = 0, direction = "forward", 
                                met_type = "reanalysis", vert_motion = 0, model_height = 20000, 
                                extended_met = FALSE, config = NULL, ascdata = NULL, traj_name = NULL, 
                                binary_path = NULL, met_dir = NULL, exec_dir = NULL, clean_up = TRUE) 
{
  if (is.null(exec_dir)) 
    exec_dir <- getwd()
  if (is.null(met_dir)) 
    met_dir <- getwd()
  binary_path <- set_binary_path(binary_path = binary_path, 
                                 binary_name = "hyts_std")
  system_type <- get_os()
  if (is.null(traj_name)) {
    folder_name <- paste0("traj-", format(Sys.time(), 
                                          "%Y-%m-%d-%H-%M-%S"))
  }
  else if (!is.null(traj_name)) {
    folder_name <- traj_name
  }
  if (is.null(config)) {
    config_list <- set_config()
  }
  else {
    config_list <- config
  }
  if (is.null(ascdata)) {
    ascdata_list <- set_ascdata()
  }
  else {
    ascdata_list <- ascdata
  }
  if (isTRUE(extended_met)) {
    tm_names <- config_list %>% names() %>% vapply(FUN.VALUE = logical(1), 
                                                   USE.NAMES = FALSE, FUN = function(y) y %>% tidy_grepl("^tm_")) %>% 
      which()
    config_list[tm_names] <- 1
  }
  config_list %>% write_config_list(dir = exec_dir)
  ascdata_list %>% write_ascdata_list(dir = exec_dir)
  if (length(lat) != length(lon)) {
    stop("The coordinate vectors are not the same length.", 
         call. = FALSE)
  }
  met_files <- download_met_files(met_type = met_type, days = days, 
                                  duration = duration, direction = direction, met_dir = met_dir)
  receptors_tbl <- dplyr::tibble(lat = lat, lon = lon) %>% 
    dplyr::group_by(lat, lon) %>% tidyr::expand(height = height) %>% 
    dplyr::ungroup() %>% dplyr::mutate(receptor = dplyr::row_number()) %>% 
    dplyr::select(receptor, dplyr::everything())
  receptors <- seq(nrow(receptors_tbl))
  ensemble_tbl <- dplyr::tibble()
  recep_file_path_stack <- c()
  for (receptor in receptors) {
    receptor_vals <- get_receptor_values(receptors_tbl = receptors_tbl, 
                                         receptor_i = receptor)
    receptor_i <- receptor_vals$receptor
    lat_i <- receptor_vals$lat
    lon_i <- receptor_vals$lon
    height_i <- receptor_vals$height
    list_run_days <- days %>% as.character()
    trajectory_files <- c()
    for (i in seq(list_run_days)) {
      start_year_GMT <- to_short_year(list_run_days[i])
      start_month_GMT <- to_short_month(list_run_days[i])
      start_day_GMT <- to_short_day(list_run_days[i])
      if (inherits(daily_hours, "numeric")) {
        daily_hours <- formatC(sort(daily_hours), width = 2, 
                               flag = 0)
      }
      for (j in daily_hours) {
        start_hour_GMT <- j
        if (start_year_GMT > 40) {
          full_year_GMT <- paste0("19", start_year_GMT)
        }
        else {
          full_year_GMT <- paste0("20", start_year_GMT)
        }
        output_filename <- get_traj_output_filename(traj_name = traj_name, 
                                                    site = receptor_i, direction = direction, year = start_year_GMT, 
                                                    month = start_month_GMT, day = start_day_GMT, 
                                                    hour = start_hour_GMT, lat = lat_i, lon = lon_i, 
                                                    height = height_i, duration = duration)
        trajectory_files <- c(trajectory_files, output_filename)
        write_traj_control_file(start_year_GMT = start_year_GMT, 
                                start_month_GMT = start_month_GMT, start_day_GMT = start_day_GMT, 
                                start_hour_GMT = start_hour_GMT, lat = lat_i, 
                                lon = lon_i, height = height_i, direction = direction, 
                                duration = duration, vert_motion = vert_motion, 
                                model_height = model_height, met_files = met_files, 
                                output_filename = output_filename, system_type = system_type, 
                                met_dir = met_dir, exec_dir = exec_dir)
        sys_cmd <- paste0("(cd \"", exec_dir, "\" && \"", 
                          binary_path, "\" ", to_null_dev(system_type = system_type), 
                          ")")
        execute_on_system(sys_cmd, system_type = system_type)
      }
    }
    recep_file_path <- file.path(exec_dir, receptor_i, folder_name)
    recep_file_path_stack <- c(recep_file_path_stack, file.path(exec_dir, 
                                                                receptor_i))
    if (!dir.exists(recep_file_path)) {
      dir.create(path = recep_file_path, recursive = TRUE)
    }
    file.copy(from = file.path(exec_dir, trajectory_files), 
              to = recep_file_path, copy.mode = TRUE)
    unlink(file.path(exec_dir, trajectory_files), force = TRUE)
    traj_tbl <- trajectory_read(output_folder = recep_file_path) %>% 
      dplyr::as_tibble() %>% dplyr::mutate(receptor = receptor_i, 
                                           lat_i = lat_i, lon_i = lon_i, height_i = height_i)
    ensemble_tbl <- ensemble_tbl %>% dplyr::bind_rows(traj_tbl)
  }
  if (clean_up) {
    unlink(file.path(exec_dir, traj_output_files()), force = TRUE)
    unlink(recep_file_path_stack, recursive = TRUE, force = TRUE)
  }
  ensemble_tbl <- ensemble_tbl %>% dplyr::select(-c(year, month, 
                                                    day, hour)) %>% dplyr::select(receptor, hour_along, traj_dt, 
                                                                                  lat, lon, height, traj_dt_i, lat_i, lon_i, height_i, 
                                                                                  dplyr::everything()) %>% dplyr::group_by(receptor, hour_along, 
                                                                                                                           traj_dt, traj_dt_i, lat_i, lon_i, height_i) %>% dplyr::slice(1) %>% 
    dplyr::ungroup()
  if (direction == "forward") {
    ensemble_tbl <- ensemble_tbl %>% dplyr::arrange(receptor, 
                                                    traj_dt_i)
  }
  else {
    ensemble_tbl <- ensemble_tbl %>% dplyr::arrange(receptor, 
                                                    traj_dt_i, dplyr::desc(hour_along))
  }
  ensemble_tbl %>% dplyr::right_join(ensemble_tbl %>% dplyr::select(receptor, 
                                                                    traj_dt_i, lat_i, lon_i, height_i) %>% dplyr::distinct() %>% 
                                       dplyr::mutate(run = dplyr::row_number()), by = c("receptor", 
                                                                                        "traj_dt_i", "lat_i", "lon_i", "height_i")) %>% 
    dplyr::select(run, dplyr::everything())
}

##### New function, hysplit_trajectory_mod(), with solved GDAS1 errors #####

# Three changes have been made: 

# 1) binary path block changed due to set_binary_path() errors. This may or may not be necessary; not sure.
#binary_path <- set_binary_path(binary_path = binary_path, 
#                               binary_name = "hyts_std")
# is now
#binary_name <- "hyts_std"
#binary_path <-
#  system.file(
#    file.path("win", paste0(binary_name, ".exe")),
#    package = "splitr"
#  )

# 2) instead of using download_met_files() from splitr, the function now uses download_met_files_mod(), 
#    which is identical to the original save for a tweak to how it calls gdas1 data (see below).

# 3) within download_met_files_mod(), get_met_gdas1() has been changed to get_met_gdas1_mod(), which 
#    changes the following lines of code to solve the sys_time error:

#month_names <- 
#  seq(min_date, max_date, by = "1 day") %>%
#  lubridate::month(label = TRUE, abbr = TRUE, locale = "en_US.UTF-8")  %>%
#  as.character() %>%
#  tolower()
# is now:
#month_names <- 
#  seq(min_date, max_date, by = "1 day") %>%
#  lubridate::month(label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))  %>%
#  as.character() %>%
#  tolower()

# This error presumably extends across each met type, in which case a similar edit will need to be made for 
# e.g. get_met_reanalysis() if you're after reanalysis.

hysplit_trajectory_mod <- function (lat = 49.263, lon = -123.25, height = 50, duration = 24, 
                                    days = NULL, daily_hours = 0, direction = "forward", 
                                    met_type = "reanalysis", vert_motion = 0, model_height = 20000, 
                                    extended_met = FALSE, config = NULL, ascdata = NULL, traj_name = NULL, 
                                    binary_path = NULL, met_dir = NULL, exec_dir = NULL, clean_up = TRUE) 
{
  #Source the two new functions
  if (is.null(exec_dir)) 
    exec_dir <- getwd()
  if (is.null(met_dir)) 
    met_dir <- getwd()
  binary_name <- "hyts_std"
  binary_path <-
    system.file(
      file.path("win", paste0(binary_name, ".exe")),
      package = "splitr"
    )
  system_type <- get_os()
  if (is.null(traj_name)) {
    folder_name <- paste0("traj-", format(Sys.time(), 
                                          "%Y-%m-%d-%H-%M-%S"))
  }
  else if (!is.null(traj_name)) {
    folder_name <- traj_name
  }
  if (is.null(config)) {
    config_list <- set_config()
  }
  else {
    config_list <- config
  }
  if (is.null(ascdata)) {
    ascdata_list <- set_ascdata()
  }
  else {
    ascdata_list <- ascdata
  }
  if (isTRUE(extended_met)) {
    tm_names <- config_list %>% names() %>% vapply(FUN.VALUE = logical(1), 
                                                   USE.NAMES = FALSE, FUN = function(y) y %>% tidy_grepl("^tm_")) %>% 
      which()
    config_list[tm_names] <- 1
  }
  config_list %>% write_config_list(dir = exec_dir)
  ascdata_list %>% write_ascdata_list(dir = exec_dir)
  if (length(lat) != length(lon)) {
    stop("The coordinate vectors are not the same length.", 
         call. = FALSE)
  }
  download_met_files_mod <- function(met_type,
                                     days,
                                     duration,
                                     direction,
                                     met_dir) {
    if (met_type == "gdas1") {
      get_met_gdas1_mod <- function(days,
                                    duration,
                                    direction,
                                    path_met_files) {
        
        # Determine the minimum date (as a `Date`) for the model run
        if (direction == "backward") {
          min_date <- 
            (lubridate::as_date(days[1]) - (duration / 24)) %>%
            lubridate::floor_date(unit = "day")
        } else if (direction == "forward") {
          min_date <- 
            (lubridate::as_date(days[1])) %>%
            lubridate::floor_date(unit = "day")
        }
        
        # Determine the maximum date (as a `Date`) for the model run
        if (direction == "backward") {
          max_date <- 
            (lubridate::as_date(days[length(days)])) %>%
            lubridate::floor_date(unit = "day")
        } else if (direction == "forward") {
          max_date <- 
            (lubridate::as_date(days[length(days)]) + (duration / 24)) %>%
            lubridate::floor_date(unit = "day")
        }
        
        met_days <- 
          seq(min_date, max_date, by = "1 day") %>% 
          lubridate::day()
        
        month_names <- 
          seq(min_date, max_date, by = "1 day") %>%
          lubridate::month(label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))  %>%
          as.character() %>%
          tolower()
        
        # This was the previous version of the above block, and threw up an error. 
        #month_names <- 
        #  seq(min_date, max_date, by = "1 day") %>%
        #  lubridate::month(label = TRUE, abbr = TRUE, locale = "en_US.UTF-8")  %>%
        #  as.character() %>%
        #  tolower()
        
        met_years <- 
          seq(min_date, max_date, by = "1 day") %>%
          lubridate::year() %>% 
          substr(3, 4)
        
        # Only consider the weeks of the month we need:
        #.w1 - days 1-7
        #.w2 - days 8-14
        #.w3 - days 15-21
        #.w4 - days 22-28
        #.w5 - days 29 - rest of the month 
        
        met_week <- ceiling(met_days / 7)
        
        files <- paste0("gdas1.", month_names, met_years, ".w", met_week) %>% unique()
        
        get_met_files(
          files = files,
          path_met_files = path_met_files,
          ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/gdas1"
        )
        
      }
      met_files <-
        get_met_gdas1_mod(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "gdas0.5") {
      
      met_files <-
        get_met_gdas0p5(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "gfs0.25") {
      
      met_files <-
        get_met_gfs0p25(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "reanalysis") {
      
      met_files <-
        get_met_reanalysis(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "nam12") {
      
      met_files <-
        get_met_nam12(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "narr") {
      
      met_files <-
        get_met_narr(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "era5") {
      
      met_files <-
        get_met_era5(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    met_files
  }
  met_files <- download_met_files_mod(met_type = met_type, days = days, 
                                      duration = duration, direction = direction, met_dir = met_dir)
  receptors_tbl <- dplyr::tibble(lat = lat, lon = lon) %>% 
    dplyr::group_by(lat, lon) %>% tidyr::expand(height = height) %>% 
    dplyr::ungroup() %>% dplyr::mutate(receptor = dplyr::row_number()) %>% 
    dplyr::select(receptor, dplyr::everything())
  receptors <- seq(nrow(receptors_tbl))
  ensemble_tbl <- dplyr::tibble()
  recep_file_path_stack <- c()
  for (receptor in receptors) {
    receptor_vals <- get_receptor_values(receptors_tbl = receptors_tbl, 
                                         receptor_i = receptor)
    receptor_i <- receptor_vals$receptor
    lat_i <- receptor_vals$lat
    lon_i <- receptor_vals$lon
    height_i <- receptor_vals$height
    list_run_days <- days %>% as.character()
    trajectory_files <- c()
    for (i in seq(list_run_days)) {
      start_year_GMT <- to_short_year(list_run_days[i])
      start_month_GMT <- to_short_month(list_run_days[i])
      start_day_GMT <- to_short_day(list_run_days[i])
      if (inherits(daily_hours, "numeric")) {
        daily_hours <- formatC(sort(daily_hours), width = 2, 
                               flag = 0)
      }
      for (j in daily_hours) {
        start_hour_GMT <- j
        if (start_year_GMT > 40) {
          full_year_GMT <- paste0("19", start_year_GMT)
        }
        else {
          full_year_GMT <- paste0("20", start_year_GMT)
        }
        output_filename <- get_traj_output_filename(traj_name = traj_name, 
                                                    site = receptor_i, direction = direction, year = start_year_GMT, 
                                                    month = start_month_GMT, day = start_day_GMT, 
                                                    hour = start_hour_GMT, lat = lat_i, lon = lon_i, 
                                                    height = height_i, duration = duration)
        trajectory_files <- c(trajectory_files, output_filename)
        write_traj_control_file(start_year_GMT = start_year_GMT, 
                                start_month_GMT = start_month_GMT, start_day_GMT = start_day_GMT, 
                                start_hour_GMT = start_hour_GMT, lat = lat_i, 
                                lon = lon_i, height = height_i, direction = direction, 
                                duration = duration, vert_motion = vert_motion, 
                                model_height = model_height, met_files = met_files, 
                                output_filename = output_filename, system_type = system_type, 
                                met_dir = met_dir, exec_dir = exec_dir)
        sys_cmd <- paste0("(cd \"", exec_dir, "\" && \"", 
                          binary_path, "\" ", to_null_dev(system_type = system_type), 
                          ")")
        execute_on_system(sys_cmd, system_type = system_type)
      }
    }
    recep_file_path <- file.path(exec_dir, receptor_i, folder_name)
    recep_file_path_stack <- c(recep_file_path_stack, file.path(exec_dir, 
                                                                receptor_i))
    if (!dir.exists(recep_file_path)) {
      dir.create(path = recep_file_path, recursive = TRUE)
    }
    file.copy(from = file.path(exec_dir, trajectory_files), 
              to = recep_file_path, copy.mode = TRUE)
    unlink(file.path(exec_dir, trajectory_files), force = TRUE)
    traj_tbl <- trajectory_read(output_folder = recep_file_path) %>% 
      dplyr::as_tibble() %>% dplyr::mutate(receptor = receptor_i, 
                                           lat_i = lat_i, lon_i = lon_i, height_i = height_i)
    ensemble_tbl <- ensemble_tbl %>% dplyr::bind_rows(traj_tbl)
  }
  if (clean_up) {
    unlink(file.path(exec_dir, traj_output_files()), force = TRUE)
    unlink(recep_file_path_stack, recursive = TRUE, force = TRUE)
  }
  ensemble_tbl <- ensemble_tbl %>% dplyr::select(-c(year, month, 
                                                    day, hour)) %>% dplyr::select(receptor, hour_along, traj_dt, 
                                                                                  lat, lon, height, traj_dt_i, lat_i, lon_i, height_i, 
                                                                                  dplyr::everything()) %>% dplyr::group_by(receptor, hour_along, 
                                                                                                                           traj_dt, traj_dt_i, lat_i, lon_i, height_i) %>% dplyr::slice(1) %>% 
    dplyr::ungroup()
  if (direction == "forward") {
    ensemble_tbl <- ensemble_tbl %>% dplyr::arrange(receptor, 
                                                    traj_dt_i)
  }
  else {
    ensemble_tbl <- ensemble_tbl %>% dplyr::arrange(receptor, 
                                                    traj_dt_i, dplyr::desc(hour_along))
  }
  ensemble_tbl %>% dplyr::right_join(ensemble_tbl %>% dplyr::select(receptor, 
                                                                    traj_dt_i, lat_i, lon_i, height_i) %>% dplyr::distinct() %>% 
                                       dplyr::mutate(run = dplyr::row_number()), by = c("receptor", 
                                                                                        "traj_dt_i", "lat_i", "lon_i", "height_i")) %>% 
    dplyr::select(run, dplyr::everything())
}