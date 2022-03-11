#' @title Negative Binomial Probability
#'
#' @param y y variable as an integer for the number of failures
#' @param r r variable as an integer for the number of successes
#' @param p p as the probability of a success
#'
#' @return Returns the negative binomial probability that there are y number of failures before the rth success.
#' @export
#'
#' @examples
#' mynbin(10,3,0.4)
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}
