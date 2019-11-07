# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Plot the mpg by wt of the mtcars data using the ggplot2 package.
#'
#' @examples plot_test_ggplot2_figure()
#' @export
plot_test_ggplot2_figure <- function() {
  ggplot2::qplot(datasets::mtcars$mpg, datasets::mtcars$wt)
}
