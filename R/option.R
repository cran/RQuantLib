## RQuantLib -- R interface to the QuantLib libraries
##
## Copyright 2002 Dirk Eddelbuettel <edd@debian.org>
##
## $Id: option.R,v 1.2 2002/02/26 03:40:05 edd Exp $
##
## This file is part of the RQuantLib library for GNU R.
## It is made available under the terms of the GNU General Public
## License, version 2, or at your option, any later version,
## incorporated herein by reference.
##
## This program is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the implied
## warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
## PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public
## License along with this program; if not, write to the Free
## Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
## MA 02111-1307, USA

EuropeanOption <- function(x, ...) {
  if (is.null(class(x)))
    class(x) <- data.class(x)
  UseMethod("EuropeanOption", x, ...)
}

EuropeanOption.default <- function(type, underlying, strike, dividendYield,
                                     riskFreeRate, maturity, volatility) {
  val <- .Call("QL_EuropeanOption",
               list(type=as.character(type),
                    underlying=as.double(underlying),
                    strike=as.double(strike),
                    dividendYield=as.double(dividendYield),
                    riskFreeRate=as.double(riskFreeRate),
                    maturity=as.double(maturity),
                    volatility=as.double(volatility)))
  class(val) <- c("EuropeanOption", "Option")
  val
}

AmericanOption <- function(x, ...) {
  if (is.null(class(x)))
    class(x) <- data.class(x)
  UseMethod("AmericanOption", x, ...)
}

AmericanOption.default <- function(type, underlying, strike, dividendYield,
                                     riskFreeRate, maturity, volatility,
                                     timeSteps=150, gridPoints=151) {
  val <- .Call("QL_AmericanOption",
               list(type=as.character(type),
                    underlying=as.double(underlying),
                    strike=as.double(strike),
                    dividendYield=as.double(dividendYield),
                    riskFreeRate=as.double(riskFreeRate),
                    maturity=as.double(maturity),
                    volatility=as.double(volatility),
                    timeSteps=as.integer(timeSteps),
                    gridPoints=as.integer(gridPoints)))
  class(val) <- c("AmericanOption","Option")
  val
}

BinaryOption <- function(x, ...) {
  if (is.null(class(x)))
    class(x) <- data.class(x)
  UseMethod("BinaryOption", x, ...)
}

BinaryOption.default <- function(type, underlying, strike, dividendYield,
                                   riskFreeRate, maturity, volatility,
                                   cashPayoff) {
  val <- .Call("QL_BinaryOption",
               list(type=as.character(type),
                    underlying=as.double(underlying),
                    strike=as.double(strike),
                    dividendYield=as.double(dividendYield),
                    riskFreeRate=as.double(riskFreeRate),
                    maturity=as.double(maturity),
                    volatility=as.double(volatility),
                    cashPayoff=as.double(cashPayoff)))
  class(val) <- c("BinaryOption", "Option")
  val
}


plot.Option <- function(x, ...) {
  warning(paste("No plotting available for class", class(x)[1],"\n"))
  invisible(x)
}

print.Option <- function(x, digits=options('digits')[[1]], ...) {
  cat(paste("Concise summary of valuation for", class(x)[1], "\n"))
  print(unlist(x[1:7]), digits=digits)
  invisible(x)
}

summary.Option <- function(object, digits=options('digits')[[1]], ...) {
  cat(paste("Detailed summary of valuation for", class(object)[1], "\n"))
  print(unlist(object[1:7]),digits=digits)
  print(unlist(object[["parameters"]]),digits=digits)
  invisible(object)
}




