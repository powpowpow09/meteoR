#' @import R6
#' @import DT
#'
#' @export
default_data_frame <- R6::R6Class(
  "default_data_frame",
  # Public ----
  public = list(
    print = function() {
      print(private$.data)
    },


    head = function(n = 9) {
      head(private$.data, n = n)
    },


    tail = function(n = 9) {
      tail(private$.data, n = n)
    },


    view = function() {
      DT::datatable(private$.data,
                    extensions = 'Buttons',
                    options = list(dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All"))))
    },


    subset = function(subset, select, drop = FALSE) {
      r <- if (missing(subset))
        rep_len(TRUE, nrow(private$.data))
      else {
        r <- eval(substitute(subset), private$.data, parent.frame())
        if (!is.logical(r))
          stop("'subset' must be logical")
        r & !is.na(r)
      }
      vars <- if (missing(select))
        TRUE
      else {
        nl <- as.list(seq_along(private$.data))
        names(nl) <- names(private$.data)
        eval(substitute(select), nl, parent.frame())
      }
      return(private$.data[r, vars, drop = drop])
    },


    write_file = function(name, directory) {
      if (grepl(".csv$", name, fixed = FALSE)) {
        name <- paste0(name, ".csv")
      }
      if (!dir.exists(directory)) {
        dir.create(directory)
        message("Directory { ", directory, " } created")
      }
      write.table(private$.data, file.path(directory, paste(name, ".csv")), sep = ";", eol = "\r\n", na = "", quote = FALSE, row.names = FALSE)
    },

    initialize = function() {
    },

    finalize = function() {
    }
  ),
  # Private ----
  private = list(
    .data = NULL
  ),
  # Active ----
  active = list(
    data = function() {
      private$.data
    }
  )
)
