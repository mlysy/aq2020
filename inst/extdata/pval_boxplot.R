#' Generate p-value boxplots in the AQ2020 paper.
#'
#' @param data Element `data` from list as output by [format_data()].
#' @param pval Element `pval` from list as output by [format_data()].
#' @param ylim Optional range of values at which to clip the plot.  Default is no clipping.
#' @param same_scale Whether to use the same scale for all plots.  This corresponds to the `fixed` vs `free_y` argument to [ggplot2::facet_wrap()].
#' @param nrow Number of rows when there are multiple stations to plot.  This corresponds to the `nrow` argument to [ggplot2::facet_wrap()].
#' @param pv_size Size of p-value labels.
#' @param pt_size Size of data points.
#' @param lab_size Size of axis labels.  Sets the arguments `axis.title` and `axis.text` in [ggplot2::theme()].
#' @param leg_size Size of legend text.  Sets the arguments `legend.title` and `legend.text` in [ggplot2::theme()].
#' @param main_size Size of station name in plot.
#' @param main_pos String for where to put the station name: "left" or "right".
#'
#' @return Boxplots with p-value labels of `Value` by `Period` within `Month`, tiled by `Station`.
#' @export
pval_boxplot <- function(data, pval, ylim, same_scale = TRUE, nrow = NULL,
                         pv_size, pt_size, lab_size, leg_size, main_size,
                         main_pos = c("right", "left")) {
  if(missing(ylim)) ylim <- c(-Inf, Inf)
  main_pos <- match.arg(main_pos)
  data %>%
    filter(Value <= ylim[2] & Value >= ylim[1]) %>%
    ggplot(aes(x = Month, y = Value)) +
    # Value points by Year and Period
    geom_jitter(mapping = aes(fill = year, group = Period, shape = year),
                position = position_jitterdodge(.5, seed = 1),
                size = pt_size, color = "transparent") +
    ## # Value text labels for clipped points
    ## geom_text(data = data %>% filter(Value >= ylim[2]),
    ##           mapping = aes(label = round(Value, 2), group = Period),
    ##           check_overlap = TRUE, size = pv_size, vjust = 1.5,
    ##           position = position_jitterdodge(.5, seed = 1)) +
    # Value boxplots by Period
    geom_boxplot(aes(color = Period),
                 outlier.shape = NA, alpha = 0) +
    # P-value labels by Period
    geom_text(data = pval,
              mapping = aes(x = Month, y = Yvalue, group = Period,
                            label = signif(round(Value, 2), 1)),
              position = position_dodge(.8),
              size = pv_size) +
    # Inner title by Station
    geom_text(data = pval,
              mapping = aes(label = Station, y = Inf,
                            x = ifelse(main_pos == "right", Inf, -Inf)),
              size = main_size,
              hjust = ifelse(main_pos == "right", 1.1, -0.1),
              vjust = 2) +
    facet_wrap(~ Station, scales = ifelse(same_scale, "fixed", "free_y"),
               nrow = nrow) +
    scale_fill_manual(values = c("red", "blue", "orange", "darkgreen")) +
    scale_shape_manual(values = 21:25) +
    guides(fill = guide_legend(override.aes = list(size=2.5))) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = c(0,.8),
      axis.title = element_text(size = lab_size),
      axis.text = element_text(size = lab_size),
      legend.text = element_text(size = leg_size),
      legend.title = element_text(size = leg_size),
      legend.spacing = unit(.2, "cm"),
      legend.margin = margin(2, 2, 2, 2),
      legend.key.size = unit(.5, "cm"),
      legend.background = element_rect(color = "black", size = .2),
      panel.border = element_rect(fill = NA, size = 1.5),
      axis.ticks = element_line(size = 1)
    )
}

#--- scratch -------------------------------------------------------------------

## #' P-value boxplots for AQ2020.
## #'
## #' @param data Tibble containing columns `Year`, `Month`, `Day`, and `Value`.
## #' @param pval Tibble containing columns `Period`, `Month`, and `Value`.
## #' @param main Title of plot.
## #' @param ylab Label for y-axis.
## #' @export
## pval_boxplot <- function(data, pval, main, ylab,
##                          pt_size = 2, pv_size = 4, legend = TRUE) {
##   # plot
##   data %>%
##     mutate(Year = factor(Year),
##            Period = ifelse(Year == 2020, "2020", "2017-2019")) %>%
##     ggplot(aes(x = Month, y = Value)) +
##     geom_jitter(aes(fill = Year, group = Period, shape = Year),
##                 position = position_jitterdodge(.5),
##                 size = pt_size, color = "transparent") +
##     geom_boxplot(aes(color = Period), outlier.shape = NA, alpha = 0) +
##     geom_text(data = pval,
##               mapping = aes(x = Month, y = 0, group = Period,
##                             label = paste0(round(Value, 2), "")),
##               position = position_dodge(.8),
##               size = pv_size) +
##     scale_fill_manual(values = c("red", "blue", "orange", "darkgreen")) +
##     scale_shape_manual(values = 21:25) +
##     ylab(ylab) +
##     ggtitle(main) +
##     multiplot::theme_min2(legend.position = if(legend) c(0, 1) else "none",
##                           legend.direction = "horizontal",
##                           legend.justification = c(-.02,1.02))
## }
