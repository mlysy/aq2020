#' Multi-group variance statistics.
#'
#' @param value Value variable of length `nobs`.
#' @param group Grouping variable of length `nobs`.  Must contain two or more distinct values.
#' @return A vector of length two, containing the variance of within-group means and within-group medians, respectively.
#' @export
Tvar <- function(value, group) {
  grnm <- sort(unique(group))
  ngr <- length(grnm)
  if(ngr < 2) stop("group must contain two or more distinct values.")
  x <- matrix(NA, ngr, 2)
  for(ii in 1:ngr) {
    v <- value[group == grnm[ii]]
    x[ii,] <- c(mean(v, na.rm = TRUE), median(v, na.rm = TRUE))
  }
  apply(x, 2, var)
}
