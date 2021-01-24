#' Two-group absolute-difference statistics.
#'
#' @param value Value variable of length `nobs`.
#' @param group Grouping variable of length `nobs`.  Must contain exactly two distinct values.
#' @return A vector of length two, containing the absolute difference between group means and group medians, respectively.
#' @export
Tdiff <- function(value, group) {
  grnm <- sort(unique(group))
  ngr <- length(grnm)
  x <- matrix(NA, ngr, 2)
  for(ii in 1:ngr) {
    v <- value[group == grnm[ii]]
    x[ii,] <- c(mean(v, na.rm = TRUE), median(v, na.rm = TRUE))
  }
  abs(x[2,] - x[1,])
}