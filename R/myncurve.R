#' @title Normal Curve Distribution
#'
#' @param mu mu as the mean of the distribution
#' @param sigma sigma as the standard deviation of the distribution
#' @param a a as the value that the random sample is less than
#'
#' @return Returns a normal distribution curve with the area shaded between the curve
#' and the x-axis from -10000 to x=a. The area of P(x<=a) is also displayed on the plot.
#'
#' @export
#'
#' @import stats
#' @import graphics
#'
#' @examples
#' myncurve(mu=10, sigma=2, a=8)
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma),
        xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(-10000,a,length=100000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  graphics::polygon(c(-10000,xcurve,a),c(0,ycurve,0),col="Red")
  prob=stats::pnorm(a,mean=mu,sd=sigma)
  prob=round(prob,4)
  text(mu+2*sigma, .4/sigma/2, paste("Area = ", prob), col="Red")
  list(Area = prob)
}
