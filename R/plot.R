#' Plot the isotopic data with a biplot
#' 
#' @param isotope_data the raw isotopic data
#' 
#' @import ggplot2
#' 
#' @keywords internal
#' @noRd

plot_isotope_data <- function(isotope_data){
  
  figure <- ggplot(isotope_data,
                   aes(x = isotope_data[, 2],
                       y = isotope_data[, 3],
                       colour = group)) +
    ggtitle("Isotopic data") + 
    xlab(names(isotope_data)[2]) + 
    ylab(names(isotope_data)[3]) +
    geom_point(size = 3) +
    scale_colour_brewer(palette = "Paired") +
    guides(colour = guide_legend(byrow = 1, ncol = 1)) +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "grey"),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(margin = margin(3, 0, 0, 0), size = 12),
          plot.title = element_text(hjust = 0.5))
  
  plot(figure)
}


#' Plot the raw data entered by the user.
#'
#' @param isotope_data the raw isotopic data
#' @param stomach_data the raw stomachal data if the user has it. 
#' By default this variable is set on NULL and is not ploted.
#' @param literature_diets the diet priors from the literature if the user has it.
#' By default this variable is set on NULL and is not ploted.
#'
#' @export

plot_data <- function(isotope_data, stomach_data = NULL, literature_diets = NULL){
  
  plot_isotope_data(isotope_data)
  
}