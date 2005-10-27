## RQuantLib function DiscountCurve
##
## Copyright (C) 2005  Dominick Samperi
##
## $Id: discount.R,v 1.1 2005/10/12 03:41:48 edd Exp $
##
## This program is part of the RQuantLib library for R (GNU S).
## It is made available under the terms of the GNU General Public
## License, version 2, or at your option, any later version.
##
## This program is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the implied
## warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
## PURPOSE.  See the GNU General Public License for more
## details.

DiscountCurve <- function(params, tsQuotes, times) {
  UseMethod("DiscountCurve")
}

DiscountCurve.default <- function(params, tsQuotes, times) {

  # Check that params is properly formatted.
  if(!is.list(params) || length(params) == 0) {
    stop("The params parameter must be a non-empty list");
  }

  # Check that the term structure quotes are properly formatted.
  if(!is.list(tsQuotes) || length(tsQuotes) == 0) {
    stop("Term structure quotes must be a non-empty list")
  }
  if(length(tsQuotes) != length(names(tsQuotes))) {
    stop("Term structure quotes must include labels")
  }
  if(!is.numeric(unlist(tsQuotes))) {
    stop("Term structure quotes must have numeric values")
  }
  
  # Check the times vector
  if(!is.numeric(times) || length(times) == 0)
    stop("The times parameter must be a non-emptry numeric vector")
    
  # Finally ready to make the call...
  val <- .Call("QL_DiscountCurve", params, tsQuotes, times,
               PACKAGE="RQuantLib")
  class(val) <- c("DiscountCurve")
  val
}

plot.DiscountCurve <- function(x,setpar=TRUE,...) {
  if(setpar) {
      savepar <- par(mfrow=c(3,1))
  }
  if(names(tsQuotes)[1] == "flat") {
    # Don't want to plot noise when we look at a flat yield curve
    plot(c(x$times[1],x$times[length(x$times)]), c(0,.5),type='n',
         main='forwards (flat)', xlab='time',ylab='forward rate')
    lines(x$times, x$forwards, type='l')
    plot(c(x$times[1],x$times[length(x$times)]), c(0,.5),type='n',
         main='zero rates (flat)', xlab='time',ylab='zero rate')
    lines(x$times, x$zerorates, type='l')
  }
  else {
    plot(x$times, x$forwards, type='l',
         main=paste('forwards (',x$params$interpHow,x$params$interpWhat,')'),
         xlab='time',ylab='fwd rate')
    plot(x$times, x$zerorates, type='l',
         main=paste('zero rate (',x$params$interpHow,x$params$interpWhat,')'),
         xlab='time',ylab='zero rate')
  }
  plot(x$times, x$discounts, type='l',
       main=paste('discounts (',x$params$interpHow,x$params$interpWhat,')'),
       xlab='time',ylab='discount')
  if(setpar) {
      par(savepar)
  }
}
