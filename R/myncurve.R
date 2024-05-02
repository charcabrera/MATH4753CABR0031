#' Title
#'
#' @param mu the mean of the distribution
#' @param sigma the standard deviation
#' @param a maximum of interval (-Inf, a)
#'
#' @return a normal distribution with (-Inf, a) highlighted
#' @export
#'
#' @examples
#' \dontrun{myncurve(5, 10, 3)}
myncurve = function(mu, sigma, a){
  f = function(x) {
    dnorm(x,mean=mu,sd=sigma)
  }
  curve(f, xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(from=-mu*1000,to=a)
  ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(-Inf,xcurve,a),c(0,ycurve,0),col="Red")

  list(mu = mu, sigma = sigma, area = pnorm(a, mu, sigma))
}
