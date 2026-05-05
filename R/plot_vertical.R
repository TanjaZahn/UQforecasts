#' @title Plot vertical confidence bands.
#'
#' @description  Generate a plot of the point estimates with vertical confidence bands, e.g. for different variables.
#'
#' @param df a data frame containing the point estimate and the lower and upper bounds.
#' @param xvar Name of the variable on the x-axis (do not use quotation marks).
#' @param y Name of the point estimate (do not use quotation marks). Default is `SS`.
#' @param y_LB Name of the lower bound (do not use quotation marks). Default is `LB_bs` as produced by the `conf_bands`function of our package.
#' @param y_UB Name of the upper bound (do not use quotation marks). Default is `UB_bs` as produced by the `conf_bands`function of our package.
#' @param color_by Optional. Use different colors depending on the variable name (without quotation marks) supplied to this parameter, for example, choose different colors depending on the method.
#' @param markzero Optional. If `TRUE` (default), plot black horizontal line at `y = 0`. If `FALSE`, no line will be added.
#' @param ylim Optional vector of length 2, containing the limits for the y-axis of the plot.
#' @param xlab Optional character string containing the label of the x-axis.
#' @param ylab Optional character string containing the label of the y-axis.
#' @param plottitle Optional character string containing the plot title.
#' @param theme Optional theme of the plot, e.g., theme_bw() or some custom theme.
#'
#' @return A plot containing the point estimates with vertical confidence bands.
#' 
#' 
#' @export
#' 
#' 

plot_vertical <- function(df, xvar = NULL, y = "SS", y_LB = "LB_bs", y_UB = "UB_bs", color_by = NULL,
                          markzero = TRUE, ylim = NULL, xlab = NULL, ylab = NULL, plottitle = NULL, theme = NULL){
  
  # Convert to symbols
  y <- ensym(y)
  y_LB <- ensym(y_LB)
  y_UB <- ensym(y_UB)
  if(!is.null(xvar)) xvar <- ensym(xvar)
  if(!is.null(color_by))color_by <- ensym(color_by)
  
  # Helper indicator (used for axis labels)
  onevar <- FALSE
  
  # There is only one variable
  if(is.null(xvar)){
    xvar <- 1
    df_new <- df
    onevar <- TRUE
  } 
  
  # If several variables: Keep the order of the variables
  if(!missing(xvar)){
    df_new <- df %>% mutate(xvar = ordered({{xvar}}, levels = unique({{xvar}})))
  }
  
  # Don't use color_by, i.e. there is only one method
  if(is.null(color_by))
    p <- ggplot(df_new, aes(x = xvar, y = {{y}})) +
      geom_errorbar(aes(ymax = {{y_UB}}, ymin = {{y_LB}}), color = "blue4", width = 0.05) +
      geom_point(size = 2.5, color = "blue4")
  
  # Use color_by, e.g., if there are several methods
  if(!is.null(color_by))
    p <- ggplot(df_new, aes(x = xvar, y = {{y}}, color = {{color_by}})) +
      geom_errorbar(aes(ymax = {{y_UB}}, ymin = {{y_LB}}), width = 0.05) +
      geom_point(size = 2.5)
  
  # Add horizontal line at zero
  if(markzero == TRUE)
    p <- p + geom_hline(aes(yintercept = 0))
  
  # General scales and labs
  p <-  p +
    scale_y_continuous(limits = ylim) +
    labs(x = xlab, y = ylab, title = plottitle) +
    theme
  
  
  # Adjust xlab if there is only one variable
  if(onevar == TRUE)
    p <- p + 
    theme(axis.text.x = element_text(colour="white"), # get rid of labels/ticks for x-axis
          axis.ticks.x = element_blank(), 
          axis.title.x = element_text(colour="white"))
  
  # Output
  p
  
}











