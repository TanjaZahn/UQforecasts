#' @title Plot horizontal confidence bands.
#'
#' @description  Generate a plot of the point estimates with horizontal confidence bands, e.g., over different forecast horizons, potentially for different variables.
#'
#' @param df a data frame containing the point estimate and the lower and upper bounds.
#' @param xvar Name of the variable on the x-axis.
#' @param y Name of the point estimate. Default is `SS`.
#' @param y_LB Name of the lower bound. Default is `LB_bs` as produced by the `conf_bands`function of our package.
#' @param y_UB Name of the upper bound. Default is `UB_bs` as produced by the `conf_bands`function of our package.
#' @param ylim Optional vector of length 2, containing the limits for the y-axis of the plot.
#' @param xlim Optional vector of length 2, containing the limits for the x-axis of the plot.
#' @param color_by Optional character string. Use different colors depending on the variable name supplied to this parameter. For example, choose different colors depending on the method.
#' @param facet_by Optional character string. Use different facets depending on the variable name supplied to this parameter. For example, create facts for different variables of interest.
#' @param facet_cols Optional. The number of columns used for the facets.
#' @param markzero Optional. If `TRUE` (default), plot black horizontal line at `y = 0`. If `FALSE`, no line will be added.
#' @param ylab Optional character string containing the label of the y-axis.
#' @param xlab Optional character string containing the label of the x-axis.
#' @param xbreaks Optional. Number of breaks on the x-axis.
#' @param plottitle Optional character string containing the plot title.
#' @param theme Optional theme of the plot, e.g., theme_bw() or some custom theme.
#'
#' @return A plot containing the point estimates with horizontal confidence bands.
#' 
#' 
#' 
#' @export
#' 
#' 
#' 
  


plot_horizontal <- function(df, xvar, y = "SS", y_LB = "LB_bs", y_UB = "UB_bs", ylim = NULL, xlim = NULL, color_by = NULL,
                            facet_by = NULL, facet_cols = NULL, markzero = TRUE, ylab = NULL, xlab = NULL, xbreaks = NULL, plottitle = NULL, theme = NULL){

  # Convert to symbols
  xvar <- ensym(xvar)
  y <- ensym(y)
  y_LB <- ensym(y_LB)
  y_UB <- ensym(y_UB)
  if(!is.null(facet_by)) facet_by <- ensym(facet_by)
  if(!is.null(color_by))color_by <- ensym(color_by)

  # Copy data frame
  df_new <- df

  # Keep the order of the variables
  if(!is.null(facet_by)){
    df_new <- df %>% mutate(facet_by = ordered(!!facet_by, levels = unique(!!facet_by)))
  }


  # Don't use color_by, i.e. there is only one method
  if(is.null(color_by))
    p <- ggplot(df_new) +
      geom_line(mapping = aes(x = !!xvar, y = !!y), color = "blue4") +
      geom_line(mapping = aes(x = !!xvar, y = !!y_LB), color = "blue4", linetype ="dashed") +
      geom_line(mapping = aes(x = !!xvar, y = !!y_UB), color = "blue4", linetype ="dashed")
  


  # Use color_by, e.g. if there are several methods
  if(!is.null(color_by))
    p <- ggplot(df_new, aes(x = !!xvar, y = !!y, color = !!color_by)) +
      geom_line() +
      geom_line(mapping = aes(y = !!y_LB), linetype ="dashed") +
      geom_line(mapping = aes(y = !!y_UB), linetype ="dashed")

  # Use facets, e.g. for different variables
  if(!is.null(facet_by))
    p <- (p + facet_wrap(vars(facet_by), ncol = facet_cols))

  # Add horizontal line at zero
  if(markzero == TRUE)
    p <- p + geom_hline(aes(yintercept = 0))

  # Scales and labs
  p <- p + scale_x_continuous(limits = xlim, n.breaks = xbreaks) +
    scale_y_continuous(limits = ylim) +
    labs(x = xlab, y = ylab, title = plottitle)

  # Output
  p <- p + theme
  p


}


