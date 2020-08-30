#' Import temperature and humidity from external module
#'
#' @param csv_name : file to import
#' @param ... : optional arguments
#'
#' @import readr
#' @import lubridate
#'
#' @return : a R tibble containing external temperature and humidity
#'
#' @examples
#' csv_name <- "f:/r/netatmo/csv/Exterieur_04_2017.csv"
#' boo <- import_ext_temp_hum(csv_name = csv_name)
import_ext_temp_hum <- function(csv_name = NULL, ...) {
  # parameters control ------------------------------------------------------
  stopifnot(!is.null(csv_name))
  stopifnot(nchar(csv_name) > 0)
  stopifnot(file.exists(csv_name))
  stopifnot(grepl(".CSV", toupper(csv_name)))


  # separator ---------------------------------------------------------------
  L <- readLines(csv_name, n = 1)
  if (grepl(";", L))
    delim <- ";"
  else
    delim <- ","


  # function ----------------------------------------------------------------
  boo <- readr::read_delim(
    file = csv_name,
    delim = delim,
    escape_double = FALSE,
    col_types = readr::cols(
      `Timezone : Europe/Paris` = readr::col_datetime(format = "%Y/%m/%d %H:%M:%S")
    ),
    locale = readr::locale(date_names = "fr"),
    trim_ws = TRUE,
    skip = 2
  )
  colnames(boo) <- c("id", "datetime", "temperature", "humidity")
  boo$location <- "EXTERIEUR"
  boo <- netatmo_files_datetime(boo)
  return(boo)
}


#' Import rainfall from external module
#'
#' @param csv_name : file to import
#' @param ... : optional arguments
#'
#' @import readr
#' @import lubridate
#'
#' @return : a R tibble containing external rainfall
#'
#' @examples
#' csv_name <- "f:/r/netatmo/csv/Pluviometre_04_2017.csv"
#' boo <- import_ext_rain(csv_name = csv_name)
import_ext_rain <- function(csv_name = NULL, ...) {
  # parameters control ------------------------------------------------------
  stopifnot(!is.null(csv_name))
  stopifnot(nchar(csv_name) > 0)
  stopifnot(file.exists(csv_name))
  stopifnot(grepl(".CSV", toupper(csv_name)))


  # separator ---------------------------------------------------------------
  L <- readLines(csv_name, n = 1)
  if (grepl(";", L))
    delim <- ";"
  else
    delim <- ","


  # function ----------------------------------------------------------------
  boo <- readr::read_delim(
    file = csv_name,
    delim = delim,
    escape_double = FALSE,
    col_types = readr::cols(
      `Timezone : Europe/Paris` = readr::col_datetime(format = "%Y/%m/%d %H:%M:%S")
    ),
    locale = readr::locale(date_names = "fr"),
    trim_ws = TRUE,
    skip = 2
  )
  colnames(boo) <- c("id", "datetime", "rain")
  boo$location <- "EXTERIEUR"
  boo <- netatmo_files_datetime(boo)
  return(boo)
}


#' Import wind from external module
#'
#' @param csv_name : file to import
#' @param ... : optional arguments
#'
#' @import readr
#' @import lubridate
#'
#' @return : a R tibble containing external wind and gust
#'
#' @examples
#' csv_name <- "f:/r/netatmo/csv/Anemometre_04_2017.csv"
#' boo <- import_ext_wind(csv_name = csv_name)
import_ext_wind <- function(csv_name = NULL, ...) {
  # parameters control ------------------------------------------------------
  stopifnot(!is.null(csv_name))
  stopifnot(nchar(csv_name) > 0)
  stopifnot(file.exists(csv_name))
  stopifnot(grepl(".CSV", toupper(csv_name)))


  # separator ---------------------------------------------------------------
  L <- readLines(csv_name, n = 1)
  if (grepl(";", L))
    delim <- ";"
  else
    delim <- ","


  # function ----------------------------------------------------------------
  boo <- readr::read_delim(
    file = csv_name,
    delim = delim,
    escape_double = FALSE,
    col_types = readr::cols(
      `Timezone : Europe/Paris` = readr::col_datetime(format = "%Y/%m/%d %H:%M:%S")
    ),
    locale = readr::locale(date_names = "fr"),
    trim_ws = TRUE,
    skip = 2
  )
  colnames(boo) <-
    c("id",
      "datetime",
      "wind_angle",
      "wind_strength",
      "gust_angle",
      "gust_strength")
  boo$location <- "EXTERIEUR"
  boo <- netatmo_files_datetime(boo)
  return(boo)
}



#' Import temperature, humidity, c02, noise and pressure from internal module
#'
#' @param csv_name : file to import
#' @param ... : optional arguments
#'
#' @import readr
#' @import lubridate
#' @import dplyr
#'
#' @return : a R tibble containing internal temperature, humidity, c02, noise and pressure
#'
#' @examples
#' csv_name <- "f:/r/netatmo/csv/Indoor_04_2017.csv"
#' boo <- import_ext_wind(csv_name = csv_name)
import_int <- function(csv_name = NULL, ...) {
  # parameters control ------------------------------------------------------
  stopifnot(!is.null(csv_name))
  stopifnot(nchar(csv_name) > 0)
  stopifnot(file.exists(csv_name))
  stopifnot(grepl(".CSV", toupper(csv_name)))


  # separator ---------------------------------------------------------------
  L <- readLines(csv_name, n = 1)
  if (grepl(";", L))
    delim <- ";"
  else
    delim <- ","


  # function ----------------------------------------------------------------
  boo <- readr::read_delim(
    file = csv_name,
    delim = delim,
    escape_double = FALSE,
    col_types = readr::cols(
      `Timezone : Europe/Paris` = readr::col_datetime(format = "%Y/%m/%d %H:%M:%S")
    ),
    locale = readr::locale(date_names = "fr"),
    trim_ws = TRUE,
    skip = 2
  )
  colnames(boo) <-
    c("id",
      "datetime",
      "temperature",
      "humidity",
      "c02",
      "noise",
      "pressure")
  boo$location <- dplyr::case_when(
    grepl("SALON", csv_name) == TRUE ~ "SALON",
    grepl("BUREAU", csv_name) == TRUE ~ "BUREAU",
    grepl("CHAMBRE", csv_name) == TRUE ~ "CHAMBRE",
    grepl("GARAGE", csv_name) == TRUE ~ "GARAGE",
    TRUE ~ "UNKNOWN"
  )
  boo <- netatmo_files_datetime(boo)
  return(boo)
}



#' List files to convert
#'
#' @param directory : file path where csv files are stored
#' @param pattern : list of module patterns to load
#' @param ... : optional arguments
#'
#' @import stringr
#'
#' @return : one or two lists of csv files to load
#' @export
#'
#' @examples
#' directory <- "f:/r/netatmo/csv"
#' file_lst <- netatmo_file_to_load(directory, pattern = list("indoor", "outdoor"))
#' listviewer::jsonedit(file_lst)
#' file_lst <- netatmo_file_to_load(directory, pattern = list("indoor"))
#' listviewer::jsonedit(file_lst)
#' file_lst <- netatmo_file_to_load(directory, pattern = list("outdoor"))
#' listviewer::jsonedit(file_lst)
netatmo_file_to_load <-
  function(directory = NULL,
           pattern = NULL,
           ...) {
    # parameters control ------------------------------------------------------
    stopifnot(!is.null(directory))
    stopifnot(file.info(directory)$isdir)

    stopifnot(!is.null(pattern))
    stopifnot(is.list(pattern))


    # function ----------------------------------------------------------------
    pattern <- netatmo_files_pattern(pattern)

    file_lst <-
      toupper(list.files(
        path = directory,
        recursive = TRUE,
        full.names = TRUE
      ))
    file_lst <- file_lst[grep(pattern = "CSV$", file_lst)]
    boo <-
      grepl("INTERIEUR", file_lst) + grepl("EXTERIEUR", file_lst) + grepl("PLUVIOMETRE", file_lst) + grepl("ANEMOMETRE", file_lst)
    if (sum(boo) == 0)
      warnings("No Netatmo files found in ", directory)

    file_lst_int <- NULL
    test <- sum(grepl("INTERIEUR", pattern))
    pattern_int <- NULL
    if (test > 0)
      pattern_int <- pattern[grepl("INTERIEUR", pattern)]
    if (!is.null(pattern_int)) {
      pattern_int <-
        lapply(pattern_int, function(x)
          stringr::str_to_upper(stringr::str_trim(x)))
      for (i in seq_along(pattern_int)) {
        file_lst_int <-
          rbind(file_lst_int, file_lst[grep(pattern = pattern_int[[i]], file_lst, fixed = TRUE)])
      }
    }
    message("files_to_load : ",
            length(file_lst_int),
            " internal climat files")

    file_lst_ext <- NULL
    test <- sum(grepl("EXTERIEUR", pattern))
    pattern_ext <- NULL
    if (test > 0)
      pattern_ext <- pattern[grepl("EXTERIEUR", pattern)]
    if (!is.null(pattern_ext)) {
      pattern_ext <-
        lapply(pattern_ext, function(x)
          stringr::str_to_upper(stringr::str_trim(x)))
      for (i in seq_along(pattern_ext)) {
        file_lst_ext <-
          rbind(file_lst_ext, file_lst[grep(pattern = pattern_ext[[i]], file_lst, fixed = TRUE)])
      }
    }
    message("files_to_load : ",
            length(file_lst_ext),
            " external climat files")

    file_lst_ext_rain <- NULL
    test <- sum(grepl("PLUVIOMETRE", pattern))
    pattern_ext_rain <- NULL
    if (test > 0)
      pattern_ext_rain <- pattern[grepl("PLUVIOMETRE", pattern)]
    if (!is.null(pattern_ext_rain)) {
      pattern_ext_rain <-
        lapply(pattern_ext_rain, function(x)
          stringr::str_to_upper(stringr::str_trim(x)))
      for (i in seq_along(pattern_ext_rain)) {
        file_lst_ext_rain <-
          rbind(file_lst_ext_rain, file_lst[grep(pattern = pattern_ext_rain[[i]], file_lst, fixed = TRUE)])
      }
    }
    message("files_to_load : ",
            length(file_lst_ext_rain),
            " external rain files")

    file_lst_ext_wind <- NULL
    test <- sum(grepl("ANEMOMETRE", pattern))
    pattern_ext_wind <- NULL
    if (test > 0)
      pattern_ext_wind <- pattern[grepl("ANEMOMETRE", pattern)]
    if (!is.null(pattern_ext_wind)) {
      pattern_ext_wind <-
        lapply(pattern_ext_wind, function(x)
          stringr::str_to_upper(stringr::str_trim(x)))
      for (i in seq_along(pattern_ext_wind)) {
        file_lst_ext_wind <-
          rbind(file_lst_ext_wind, file_lst[grep(pattern = pattern_ext_wind[[i]], file_lst, fixed = TRUE)])
      }
    }
    message("files_to_load : ",
            length(file_lst_ext_wind),
            " external wind files")

    return(
      list(
        file_lst_int = file_lst_int,
        file_lst_ext = file_lst_ext,
        file_lst_ext_rain = file_lst_ext_rain,
        file_lst_ext_wind = file_lst_ext_wind
      )
    )
  }



#' load CSV data
#'
#' @param file_lst : list of csv files to convert
#' @param ... : optional arguments
#'
#' @import readr
#' @import dplyr
#' @import purrr
#'
#' @export
#'
#' @examples
#' dir_csv <- "f:/r/netatmo/csv"
#' file_lst <- netatmo_file_to_load(dir_csv, pattern = list("indoor"))
#' db_lst <- load_csv(file_lst)
#' listviewer::jsonedit(db_lst)
#' db_lst$db_netatmo_indoor
#' file_lst <- netatmo_file_to_load(dir_csv, pattern = list("indoor", "outdoor"))
#' db_lst <- load_csv(file_lst)
#' listviewer::jsonedit(db_lst)
#' db_lst$db_netatmo_indoor
#' db_lst$db_netatmo_outdoor
load_csv <- function(file_lst = NULL, ...) {
  # parameters control ------------------------------------------------------
  stopifnot(!is.null(file_lst))
  stopifnot(is.list(file_lst))


  # function ----------------------------------------------------------------
  # creation structure data frame
  db_in <- data.frame(
    "location" = NA,
    "datetime" = NA,
    "date" = NA,
    "minute" = NA,
    "hour" = NA,
    "day" = NA,
    "week" = NA,
    "month" = NA,
    "month_abbr" = NA,
    "year" = NA,
    "temperature" = NA,
    "humidity" = NA,
    "c02" = NA,
    "noise" = NA,
    "pressure" = NA
  )

  db_out <- data.frame(
    "location" = NA,
    "datetime" = NA,
    "date" = NA,
    "minute" = NA,
    "hour" = NA,
    "day" = NA,
    "week" = NA,
    "month" = NA,
    "month_abbr" = NA,
    "year" = NA,
    "temperature" = NA,
    "humidity" = NA,
    "rain" = NA,
    "wind_angle" = NA,
    "wind_strength" = NA,
    "gust_angle" = NA,
    "gust_strength" = NA
  )


  # lecture des fichiers csv
  boo <- purrr::map(file_lst, length)

  if (boo[1] > 0) {
    # int
    boo1 <- to_add <- db_in1 <- datetime_new <- NULL
    boo1 <- purrr::map(file_lst$file_lst_int, import_int)
    to_add <- purrr::map(boo1, nrow) > 0
    db_in1 <-  purrr::map_df(boo1[to_add], dplyr::bind_rows) %>%
      dplyr::select(
        location,
        datetime,
        date,
        minute,
        hour,
        day,
        week,
        month,
        month_abbr,
        year,
        temperature,
        humidity,
        c02,
        noise,
        pressure
      )
    # ajout des identifiants nouveaux a db_in
    datetime_new <-
      db_in1[which(!db_in1$datetime %in% db_in$datetime), c("location", "datetime")]
    if (nrow(datetime_new) > 1) {
      db_in <- dplyr::bind_rows(db_in, datetime_new)
    }
  }

  if (boo[2] > 0) {
    # ext
    boo1 <- to_add <- db_out1 <- datetime_new <- NULL
    boo1 <- purrr::map(file_lst$file_lst_ext, import_ext_temp_hum)
    to_add <- purrr::map(boo1, nrow) > 0
    db_out1 <- purrr::map_df(boo1[to_add], dplyr::bind_rows) %>%
      dplyr::select(
        location,
        datetime,
        date,
        minute,
        hour,
        day,
        week,
        month,
        month_abbr,
        year,
        temperature,
        humidity
      )
    # ajout des identifiants nouveaux a db_out
    datetime_new <-
      db_out1[which(!db_out1$datetime %in% db_out$datetime), c("location", "datetime")]
    if (nrow(datetime_new) > 1) {
      db_out <- dplyr::bind_rows(db_out, datetime_new)
    }
  }

  if (boo[3] > 0) {
    # ext_rain
    boo1 <- to_add <- db_out2 <- datetime_new <- NULL
    boo1 <- purrr::map(file_lst$file_lst_ext_rain, import_ext_rain)
    to_add <- purrr::map(boo1, nrow) > 0
    db_out2 <- purrr::map_df(boo1[to_add], dplyr::bind_rows) %>%
      dplyr::select(
        location,
        datetime,
        date,
        minute,
        hour,
        day,
        week,
        month,
        month_abbr,
        year,
        rain
      )
    # ajout des identifiants nouveaux a db_out
    datetime_new <-
      db_out2[which(!db_out2$datetime %in% db_out$datetime), c("location", "datetime")]
    if (nrow(datetime_new) > 1) {
      db_out <- dplyr::bind_rows(db_out, datetime_new)
    }
  }

  if (boo[4] > 0) {
    # ext_wind
    boo1 <- to_add <- db_out3 <- datetime_new <- NULL
    boo1 <- purrr::map(file_lst$file_lst_ext_wind, import_ext_wind)
    to_add <- purrr::map(boo1, nrow) > 0
    db_out3 <- purrr::map_df(boo1[to_add], dplyr::bind_rows) %>%
      dplyr::select(
        location,
        datetime,
        date,
        minute,
        hour,
        day,
        week,
        month,
        month_abbr,
        year,
        wind_angle,
        wind_strength,
        gust_angle,
        gust_strength
      )
    # ajout des identifiants nouveaux a db_out
    datetime_new <-
      db_out3[which(!db_out3$datetime %in% db_out$datetime), c("location", "datetime")]
    if (nrow(datetime_new) > 1) {
      db_out <- dplyr::bind_rows(db_out, datetime_new)
    }
  }


  # creation tables finales
  db_in_raw_data <- coalesce_join(
    dataset_ori = db_in,
    dataset_add = db_in1,
    valid_cols = colnames(db_in),
    join_by = c("location", "datetime"),
    verbose = FALSE
  )
  db_out_raw_data <- coalesce_join(
    dataset_ori = db_out,
    dataset_add = db_out1,
    valid_cols = colnames(db_out),
    join_by = c("location", "datetime"),
    verbose = FALSE
  )
  db_out_raw_data <- coalesce_join(
    dataset_ori = db_out_raw_data,
    dataset_add = db_out2,
    valid_cols = colnames(db_out),
    join_by = c("location", "datetime"),
    verbose = FALSE
  )
  db_out_raw_data <- coalesce_join(
    dataset_ori = db_out_raw_data,
    dataset_add = db_out3,
    valid_cols = colnames(db_out),
    join_by = c("location", "datetime"),
    verbose = FALSE
  )

  return(list(
    db_netatmo_indoor = db_in_raw_data[!is.na(db_in_raw_data$datetime), ],
    db_netatmo_outdoor = db_out_raw_data[!is.na(db_out_raw_data$datetime), ]
  ))
}



#' CSV to RDS files transformation
#'
#' @param rds_directory : file path where rds files will be stored
#' @param file_lst : list of csv files to convert
#' @param ... : optional arguments
#'
#' @import dplyr
#' @import purrr
#' @import readr
#' @import lubridate
#'
#' @export
#'
#' @examples
#' dir_csv <- "f:/r/netatmo/csv"
#' dir_rds <- "e:/work/netatmo2"
#' file_lst <- netatmo_file_to_load(dir_csv, pattern = list("Indoor"))
#' csv_to_rds(dir_rds, file_lst)
#' file_lst <- netatmo_file_to_load(dir_csv, pattern = list("Indoor", "Outdoor"))
#' csv_to_rds(dir_rds, file_lst)
csv_to_rds <-
  function(rds_directory = getwd(),
           file_lst = NULL,
           ...) {
    # parameters control ------------------------------------------------------
    stopifnot(!is.null(rds_directory))
    stopifnot(file.info(rds_directory)$isdir)

    stopifnot(!is.null(file_lst))
    stopifnot(is.list(file_lst))


    # function ----------------------------------------------------------------
    db_lst <- load_csv(file_lst)

    dir.create(path = rds_directory,
               showWarnings = FALSE,
               recursive = TRUE)

    if (nrow(db_lst$db_netatmo_indoor) > 0) {
      readr::write_rds(db_lst$db_netatmo_indoor, file.path(
        rds_directory,
        paste0(lubridate::date(Sys.time()), "-Netatmo_indoor.rds")
      ))
    }

    if (nrow(db_lst$db_netatmo_outdoor) > 0) {
      readr::write_rds(db_lst$db_netatmo_outdoor, file.path(
        rds_directory,
        paste0(lubridate::date(Sys.time()), "-Netatmo_outdoor.rds")
      ))
    }


    if (file.exists(file.path(
      rds_directory,
      paste0(lubridate::date(Sys.time()), "-Netatmo_indoor.rds")
    ))) {
      message("Fichier ", file.path(
        rds_directory,
        paste0(lubridate::date(Sys.time()), "-Netatmo_indoor.rds")
      ), " cree.")
    } else {
      message("Erreur lors de la sauvegarde du fichier ", file.path(
        rds_directory,
        paste0(lubridate::date(Sys.time()), "-Netatmo_indoor.rds")
      ))
    }

    if (file.exists(file.path(
      rds_directory,
      paste0(lubridate::date(Sys.time()), "-Netatmo_outdoor.rds")
    ))) {
      message("Fichier ", file.path(
        rds_directory,
        paste0(lubridate::date(Sys.time()), "-Netatmo_outdoor.rds")
      ), " cree.")
    } else {
      message("Erreur lors de la sauvegarde du fichier ", file.path(
        rds_directory,
        paste0(lubridate::date(Sys.time()), "-Netatmo_outdoor.rds")
      ))
    }
}



#' load RDS data
#'
#' @param rds_directory : file path where rds files to load are be stored
#' @param ... : optional arguments
#'
#' @import readr
#' @import dplyr
#'
#' @export
#'
#' @examples
#' dir_csv <- "f:/r/netatmo/csv"
#' dir_rds <- "e:/work/netatmo2"
#' file_lst <- netatmo_file_to_load(dir_csv, pattern = list("indoor"))
#' csv_to_rds(dir_rds, file_lst)
#' db_lst <- load_rds(dir_rds)
#' listviewer::jsonedit(db_lst)
#' db_lst$db_netatmo_indoor
#' file_lst <- netatmo_file_to_load(dir_csv, pattern = list("indoor", "outdoor"))
#' csv_to_rds(dir_rds, file_lst)
#' db_lst <- load_rds(dir_rds)
#' listviewer::jsonedit(db_lst)
#' db_lst$db_netatmo_indoor
#' db_lst$db_netatmo_outdoor
load_rds <- function(rds_directory = getwd(), ...) {
  # parameters control ------------------------------------------------------
  stopifnot(!is.null(rds_directory))
  stopifnot(file.info(rds_directory)$isdir)


  # function ----------------------------------------------------------------
  db_in <- db_out <- NULL

  file_lst <-
    list.files(path = rds_directory,
               recursive = TRUE,
               full.names = TRUE)
  file_lst <- file_lst[grep(pattern = "Netatmo", file_lst)]

  if (length(file_lst) == 0)
    stop("No Netatmo files to load")

  if (sum(grepl("Netatmo_indoor.rds", file_lst)) > 0) {
    db_in <-
      readr::read_rds(file_lst[grepl("Netatmo_indoor.rds", file_lst)])
    message("file_loaded : netatmo indoor")
  }

  if (sum(grepl("Netatmo_outdoor.rds", file_lst)) > 0) {
    db_out <-
      readr::read_rds(file_lst[grepl("Netatmo_outdoor.rds", file_lst)])
    message("file_loaded : netatmo outdoor")
  }

  if (!is.null(db_in)) {
    db_in <- dplyr::as.tbl(db_in)
  }
  if (!is.null(db_out)) {
    db_out <- dplyr::as.tbl(db_out)
  }

  list(db_netatmo_indoor = db_in,
       db_netatmo_outdoor = db_out)
}
