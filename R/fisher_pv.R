#' Fisher randomization test.
#'
#' @param value Value variable of length `nobs`.
#' @param group Grouping variable of length `nobs`.
#' @param Tfun Function taking vectorized arguments `value` and `group`, returning `ntest` test statistics.
#' @param nsim Number of randomizations to perform.
#' @return A two column matrix with columns:
#' \describe{
#'   \item{`pval`}{A vector of `ntest` p-values of the form `Pr(Tsim[i] > Tobs[i])`, where `Tobs[i] = Tfun(value, group)[i]` and where `Tsim` is computed by randomly reallocating `value` to the given vector `group`.}
#'   \item{`Tobs`}{A vector of length `ntest`, where `Tobs[i]` is the observed value of test statistic `i`.}
#' }
#' @export
fisher_pv <- function(value, group, Tfun, nsim = 1e3) {
  Tobs <- Tfun(value = value, group = group)
  ntest <- length(Tobs)
  Tsim <- replicate(nsim, Tfun(value = sample(value), group = group))
  if(ntest == 1) Tsim <- t(Tsim)
  cbind(Tobs = Tobs, pval = rowMeans(Tsim > Tobs))
  ## setNames(c(Tobs, mean(Tsim > Tobs)),
  ##          nm = c("Tobs", "pval"))
}


#' Two-group test statistics.
#'
#' @param value Value variable of length `nobs`.
#' @param group Grouping variable of length `nobs`.  Must contain exactly two distinct values.
#' @return A vector of length two, containing the absolute difference between group means and group medians, respectively.
#' @export
Tbin <- function(value, group) {
  grnm <- sort(unique(group))
  ngr <- length(grnm)
  x <- matrix(NA, ngr, 2)
  for(ii in 1:ngr) {
    v <- value[group == grnm[ii]]
    x[ii,] <- c(mean(v, na.rm = TRUE), median(v, na.rm = TRUE))
  }
  abs(x[2,] - x[1,])
}

#' Multi-group test statistics.
#'
#' @param value Value variable of length `nobs`.
#' @param group Grouping variable of length `nobs`.  Must contain exactly three or more distinct values.
#' @return A vector of length two, containing the variance of within-group means and within-group medians, respectively.
#' @export
Tmulti <- function(value, group) {
  grnm <- sort(unique(group))
  ngr <- length(grnm)
  x <- matrix(NA, ngr, 2)
  for(ii in 1:ngr) {
    v <- value[group == grnm[ii]]
    x[ii,] <- c(mean(v, na.rm = TRUE), median(v, na.rm = TRUE))
  }
  apply(x, 2, var)
}


