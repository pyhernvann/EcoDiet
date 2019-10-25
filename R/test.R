#' Plot the mpg by wt of the mtcars data using the ggplot2 package.
#'
#' @examples plot_test_ggplot2_figure()
#' @export
plot_test_ggplot2_figure <- function() {
  ggplot2::qplot(datasets::mtcars$mpg, datasets::mtcars$wt)
}
