#' Launch the PropCIs Shiny Application
#'
#' Opens a browser-based graphical interface for computing confidence intervals
#' for proportions using the methods provided by the \pkg{PropCIs} package.
#' Covers single proportions, two independent proportions (difference, relative
#' risk, odds ratio), matched pairs, and Bayesian credible intervals.
#'
#' Only one method is computed at a time: the user commits to a choice before
#' seeing results, which preserves the nominal Type I error rate and prevents
#' cherry-picking across methods.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'   (e.g. \code{port}, \code{launch.browser}).
#'
#' @return Called for its side effect; does not return a value.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   PropCIsGUI::run_app()
#' }
run_app <- function(...) {
  app_dir <- system.file("app", package = "PropCIsGUI")
  if (app_dir == "") {
    stop(
      "Could not find the app directory. Try reinstalling PropCIsGUI.",
      call. = FALSE
    )
  }
  shiny::runApp(app_dir, ...)
}
