#use esquisse's function
get_data <- function(data = NULL, name = NULL) {

  if (!is.null(data)) {
    if (is.character(data)) {
      guiplot_data <- try({
        dat <- get(x = data, envir = globalenv())
        dat
        # if (inherits(dat, what = "sf")) {
        #   dat
        # } else {
        #   as.data.frame(dat)
        # }
      }, silent = TRUE)
      guiplot_data_name <- data
      if ("try-error" %in% class(guiplot_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        guiplot_data <- NULL
        guiplot_data_name <- ""
      }
    } else if (inherits(x = data, what = "data.frame")) {
      guiplot_data <- try({
        data
        # if (inherits(data, what = "sf")) {
        #   data
        # } else {
        #   as.data.frame(data)
        # }
      }, silent = TRUE)
      if ("try-error" %in% class(guiplot_data)) {
        warning(paste0("'", data, "' not found"), call. = FALSE)
        guiplot_data <- NULL
        guiplot_data_name <- ""
      } else {
        if (!is.null(name)) {
          guiplot_data_name <- as.character(name)
        } else {
          guiplot_data_name <- deparse(substitute(data))
        }
      }

    } else {
      guiplot_data <- NULL
      guiplot_data_name <- ""
    }
  }

  list(guiplot_data = guiplot_data, guiplot_data_name = guiplot_data_name)
}
