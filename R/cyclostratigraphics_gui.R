#' @title
#' cyclostratigraphics_gui
#' @description
#' GUI to help in the spectral and wavelet analysis of stratigraphic signals.
#' @import shiny
#' @author
#' Oscar Garcia-Cabrejo \email{khaors@gmail.com}
#' @export
cyclostratigraphics_gui <- function() {
  appDir <- system.file("Shiny", "cyclostratigraphics", package = "cyclostratigraphics")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `cyclostratigraphics`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
