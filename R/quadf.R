#' @title Quadratic Function
#'
#' @param x X variable
#' @param coef Array of 3 coefficients for a quadratic in the order [c, b, a]
#'
#' @return The function evaluated at X.
#' @export
#'
#' @examples
#' \dontrun{myf(15, coef = c(2, 5, 7))}
quadf = function(x, coef) {
  coef[1] + coef[2] * x + coef[3] * (x-18) * (x-18 > 0)
}
