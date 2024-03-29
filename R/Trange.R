#' Multi-group range statistics.
#'
#' @param value Value variable of length `nobs`.
#' @param group Grouping variable of length `nobs`.  Must contain two or more distinct values.
#' @return A vector of length two, containing the range (max minus min) of within-group means and within-group medians, respectively.
#' @export
Trange <- function(value, group) {
  grnm <- sort(unique(group))
  ngr <- length(grnm)
  if(ngr < 2) stop("group must contain two or more distinct values.")
  x <- matrix(NA, ngr, 2)
  for(ii in 1:ngr) {
    v <- value[group == grnm[ii]]
    x[ii,] <- c(mean(v), median(v))
  }
  y <- apply(x, 2, range)
  y[2,] - y[1,]
}
