
#' @export
netatmo_files_pattern <- function(pattern = NULL) {
  stopifnot(!is.null(pattern))
  stopifnot(is.list(pattern))

  pattern <- toupper(pattern)

  if (any(pattern == "OUTDOOR")) {
    pattern <- pattern[-which(pattern == "OUTDOOR")]
    pattern <- append(pattern, list("PLUVIOMETRE", "EXTERIEUR", "ANEMOMETRE"))
  }

  pattern
}


#' @import lubridate
#' @export
netatmo_files_datetime <- function(dataset = NULL) {
  stopifnot(!is.null(dataset))
  stopifnot(is.data.frame(dataset))

  dataset$date <- lubridate::date(dataset$datetime)
  dataset$minute <- as.integer(lubridate::minute(dataset$datetime))
  dataset$hour <- as.integer(lubridate::hour(dataset$datetime))
  dataset$day <- as.integer(lubridate::day(dataset$datetime))
  dataset$week <- as.integer(lubridate::isoweek(dataset$datetime))
  dataset$month <- as.integer(lubridate::month(dataset$datetime))
  dataset$month_abbr <- lubridate::month(dataset$datetime, label = TRUE, abbr = TRUE)
  dataset$month_abbr <- gsub("\\.", "", dataset$month_abbr, fixed = TRUE)
  dataset$year <- as.integer(lubridate::year(dataset$datetime))

  return(dataset)
}


#' @title is_valid_df
#' controle validite data frame
#'
#' @param dataset : dataset a tester
#' @param valid_cols : vecteur contenant les champs attendus dans dataset
#'
#' @export
is_valid_dataset <- function(dataset, valid_cols) {
  if (!is.data.frame(dataset)) {
    return(FALSE)
  }
  dataset_cols <- colnames(dataset)
  if (length(valid_cols) != length(dataset_cols)) {
    return(FALSE)
  }
  if (!all(valid_cols == dataset_cols)) {
    return(FALSE)
  }

  return(TRUE)
}


#' @title coalesce_join
#' ajout information a data frame existant
#'
#' @param dataset_ori ; dataset a complementer
#' @param dataset_add : dataset contenant les informations a ajouter
#' @param valid_cols : vecteur contenant les champs attendus dans dataset_ori
#' @param join_by : vecteur contenant les champs utilises pour joindre les datasets
#' @param verbose : booleen servant a masquer / afficher les messages
#'
#' @import dplyr
#'
#' @export
coalesce_join <- function(dataset_ori, dataset_add, valid_cols, join_by, verbose = FALSE) {

  # controle validite dataset
  if(!is_valid_dataset(dataset_ori, valid_cols)) {
    stop("Invalid dataset_ori")
  }

  # suffixes utilises dans les jointures
  suffix <- c(".x", ".y")

  # recherche des colonnes a mettre a jour
  # hors colonnes identifiant (celles dans by)
  dataset_ori_cols <- colnames(dataset_ori)
  dataset_add_cols <- colnames(dataset_add)
  cols_to_coalesce <- dataset_add_cols[dataset_add_cols %in% dataset_ori_cols & !(dataset_add_cols %in% join_by)]

  # message nb enregistrements communs
  if (verbose) {
    common_lines <- length(intersect(dataset_ori[, c(join_by)], dataset_add[, c(join_by)]))
    message("\n\n", paste0(rep("*", 99), collapse = ""))
    if (common_lines == 0) {
      message("Aucune ligne mise a jour car aucun element commun entre les tables dans les champs : ", paste0(join_by, collapse = ", "))
    } else {
      message("Nombre de lignes mises a jour : ", common_lines)
    }
    message(paste0(rep("*", 99), collapse = ""), "\n\n")
  }

  # jointure
  dataset_join <- dplyr::left_join(dataset_ori, dataset_add, by = join_by, suffix = suffix)

  # coalescence = place les valeurs non NA dans table origine
  for (col in cols_to_coalesce) {
    dataset_join[[col]] <- dplyr::coalesce(dataset_join[[paste0(col, suffix[1])]],
                                      dataset_join[[paste0(col, suffix[2])]])

  }

  # selection des colonnes
  dataset_res <- dplyr::select(dataset_join, dplyr::all_of(valid_cols))

  return(dataset_res)
}
