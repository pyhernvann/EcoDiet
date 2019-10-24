# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Print "hello word"
#'
#' @examples hello()
hello <- function() {
  print("Hello, world!")
}

#' Plot the mpg by wt of the mtcars data using the base package
#'
#' @examples show_mtcars()
show_mtcars <- function() {
  data(mtcars)
  plot(mtcars$mpg, mtcars$wt)
}

#' Plot the mpg by wt of the mtcars data using the ggplot2 package
#'
#' @examples show_mtcars2()
show_mtcars2 <- function() {
  qplot(mpg, wt, data = mtcars)
}


#' Add together two numbers
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}
#' @examples
#' add(1, 1)
#' add(10, -2)
add <- function(x, y) {
  x + y
}

