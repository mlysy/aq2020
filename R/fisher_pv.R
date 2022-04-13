#' Compute the p-value of a Fisher randomization test.
#'
#' @param value Vector of length `nobs` corresponding to the values from which the test statistics is computed.  That is, `value[i]` is the value corresponding to observation `i`.
#' @param group Vector of length `nobs` indicating to which group each observation belongs.  Group membership is determined from the elements of `unique(group)`.
#' @param Tfun Function taking vector arguments `value` and `group`, returning `ntest` test statistics.
#' @param nsim Number of randomizations to perform.
#' @param strict Whether the p-value is "strictly greater" or just "greater than or equal" (see 'Details').
#' @details Suppose there is only one test statistic `ntest = 1`.  Then the Fisher randomization test calculates
#' ```
#' Pr(Tsim > Tobs),
#' ```
#' where `Tobs = Tfun(value, group)` is the observed value of the test statistic, and `Tsim = Tfun(value[irand], group)`, where `irand` is a random permutation of the `nobs` observations.  When `ntest > 1`, [fisher_pv()] performs the calculation above for each test statistic.
#'
#' When `strict = FALSE` the p-value is `Pr(Tsim >= Tobs)`.
#' @return A two column matrix with `ntest` rows and columns:
#' \describe{
#'   \item{`Tobs`}{A vector of length `ntest`, where `Tobs[t]` is the observed value of test statistic `t`.}
#'   \item{`pval`}{A vector of `ntest` p-values of the form `Pr(Tsim[t] > Tobs[t])`, where `Tobs[t] = Tfun(value, group)[t]` and `Tsim[t]` is computed by randomly reallocating `value` to the given vector `group`.}
#' }
#' @export
fisher_pv <- function(value, group, Tfun, nsim = 1e3, strict = TRUE) {
  Tobs <- Tfun(value = value, group = group)
  ntest <- length(Tobs)
  Tsim <- replicate(nsim, Tfun(value = sample(value), group = group))
  if(ntest == 1) Tsim <- t(Tsim)
  if(strict) {
    out <- cbind(Tobs = Tobs, pval = rowMeans(Tsim > Tobs))
  } else {
    out <- cbind(Tobs = Tobs, pval = rowMeans(Tsim >= Tobs))
  }
  ## setNames(c(Tobs, mean(Tsim > Tobs)),
  ##          nm = c("Tobs", "pval"))
}

#--- scratch -------------------------------------------------------------------

## #' Two-group test statistics.
## #'
## #' @param value Value variable of length `nobs`.
## #' @param group Grouping variable of length `nobs`.  Must contain exactly two distinct values.
## #' @return A vector of length two, containing the absolute difference between group means and group medians, respectively.
## #' @export
## Tbin <- function(value, group) {
##   grnm <- sort(unique(group))
##   ngr <- length(grnm)
##   ## if(ngr != 2) stop("group must contain exactly two distinct values.")
##   x <- matrix(NA, ngr, 2)
##   for(ii in 1:ngr) {
##     v <- value[group == grnm[ii]]
##     x[ii,] <- c(mean(v, na.rm = TRUE), median(v, na.rm = TRUE))
##   }
##   abs(x[2,] - x[1,])
## }

## #' Multi-group test statistics.
## #'
## #' @param value Value variable of length `nobs`.
## #' @param group Grouping variable of length `nobs`.  Must contain two or more distinct values.
## #' @return A vector of length two, containing the variance of within-group means and within-group medians, respectively, when there are three or more groups.  When there are only two groups [Tbin()] is used.
## #' @export
## Tmulti <- function(value, group) {
##   grnm <- sort(unique(group))
##   ngr <- length(grnm)
##   ## if(ngr < 2) {
##   ##   stop("group must contain at least two distinct values.")
##   ## } else if(ngr == 2) {
##   if(ngr == 2) {
##     return(Tbin(value, group))
##   }
##   x <- matrix(NA, ngr, 2)
##   for(ii in 1:ngr) {
##     v <- value[group == grnm[ii]]
##     x[ii,] <- c(mean(v, na.rm = TRUE), median(v, na.rm = TRUE))
##   }
##   apply(x, 2, var)
## }


