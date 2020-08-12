#' P-value boxplots for AQ2020.
#'
#' @param data Tibble containing columns `Year`, `Month`, `Day`, and `Value`.
#' @param pval Tibble containing columns `Period`, `Month`, and `Value`.
#' @param main Title of plot.
#' @param ylab Label for y-axis.
#' @export
pval_boxplot <- function(data, pval, main, ylab,
                         pt_size = 2, pv_size = 4, legend = TRUE) {
  # plot
  data %>%
    mutate(Year = factor(Year),
           Period = ifelse(Year == 2020, "2020", "2017-2019")) %>%
    ggplot(aes(x = Month, y = Value)) +
    geom_jitter(aes(fill = Year, group = Period, shape = Year),
                position = position_jitterdodge(.5),
                size = pt_size, color = "transparent") +
    geom_boxplot(aes(color = Period), outlier.shape = NA, alpha = 0) +
    geom_text(data = pval,
              mapping = aes(x = Month, y = 0, group = Period,
                            label = paste0(round(Value, 2), "")),
              position = position_dodge(.8),
              size = pv_size) +
    scale_fill_manual(values = c("red", "blue", "orange", "darkgreen")) +
    scale_shape_manual(values = 21:25) +
    ylab(ylab) +
    ggtitle(main) +
    multiplot::theme_min2(legend.position = if(legend) c(0, 1) else "none",
                          legend.direction = "horizontal",
                          legend.justification = c(-.02,1.02))
}
