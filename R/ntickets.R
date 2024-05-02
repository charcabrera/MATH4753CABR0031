#' ntickets
#'
#' @param N number of seats on the plane
#' @param gamma probability of an overbook, i.e. too many people show up
#' @param p probability a passenger shows up
#'
#' @return a list containing the discrete and continuous answers, as well as the parameters passed in
#' @export
#'
#' @examples
#' \dontrun{ntickets(N=400,gamma = 0.02, p = 0.95)}
ntickets = function(N, gamma, p) {
  # discrete objective function
  fd = function(x) {
    N-qbinom(1-gamma, x, p)
  }

  # continuous objective function
  fc = function(x) {
    N+.5-qnorm(1-gamma,x*p,sqrt(x*p*(1-p)))
  }

  # find the root of the discrete distribution, round because of uniroot weirdness
  nd = round(uniroot(fd, c(0, N/p))$root)

  # find the root of the normal approximation
  nc = uniroot(fc, c(0, N/p))$root

  # build the discrete plot
  range=max(nd-5,N):(nd+10)
  plot(range, fd(range), type='b', xlab='n', ylab='objective',
       main=paste0("Objective vs. N to find optimal tickets sold\n(",
                   nd,
                   ") gamma=",
                   gamma,
                   " N=",
                   N,
                   " discrete"))
  abline(v = nd , h = 0)

  # build the continuous plot
  curve(fc, nc-5, nc+10, xlab='n', ylab='objective',
        main=paste0("Objective vs. N to find optimal tickets sold\n(",
                    nc,
                    ") gamma=",
                    gamma,
                    " N=",
                    N,
                    " continuous"))
  abline(v = nc , h = 0)

  # build the named list
  l = list(nd = nd,
           nc = nc,
           N = N,
           p = p,
           gamma = gamma)

  # print the named list
  print(l)

  # return the named list
  l
}
