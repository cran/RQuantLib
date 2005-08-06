## RQuantLib -- R interface to the QuantLib libraries
##
## Copyright 2002, 2005 Dirk Eddelbuettel <edd@debian.org>
##
## $Id: option.R,v 1.5 2005/08/07 02:05:27 edd Exp $
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

EuropeanOption <- function(type, underlying, strike, dividendYield,
                           riskFreeRate, maturity, volatility) {
  UseMethod("EuropeanOption")
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
                    volatility=as.double(volatility)),
               PACKAGE="RQuantLib")
  class(val) <- c("EuropeanOption", "Option")
  val
}

AmericanOption <- function(type, underlying, strike, dividendYield,
                           riskFreeRate, maturity, volatility,
                           timeSteps=150, gridPoints=151) {
  UseMethod("AmericanOption")
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
                    gridPoints=as.integer(gridPoints)),
               PACKAGE="RQuantLib")
  class(val) <- c("AmericanOption","Option")
  val
}

BinaryOption <- function(type, underlying, strike, dividendYield,
                         riskFreeRate, maturity, volatility,
                         cashPayoff) {
  UseMethod("BinaryOption")
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
                    cashPayoff=as.double(cashPayoff)),
               PACKAGE="RQuantLib")
  class(val) <- c("BinaryOption", "Option")
  val
}

BarrierOption <- function(barrType, type, underlying, strike,
                          dividendYield, riskFreeRate, maturity,
                          volatility, barrier, rebate=0.0) {
  UseMethod("BarrierOption")
}

BarrierOption.default <- function(barrType, type, underlying, strike,
                                  dividendYield, riskFreeRate, maturity,
                                  volatility, barrier, rebate=0.0) {
  val <- .Call("QL_BarrierOption",
               list(barrType=as.character(barrType),
                    type=as.character(type),
                    underlying=as.double(underlying),
                    strike=as.double(strike),
                    dividendYield=as.double(dividendYield),
                    riskFreeRate=as.double(riskFreeRate),
                    maturity=as.double(maturity),
                    volatility=as.double(volatility),
                    barrier=as.double(barrier),
                    rebate=as.double(rebate)),
               PACKAGE="RQuantLib")
  class(val) <- c("BarrierOption", "Option")
  val
}

plot.Option <- function(x, ...) {
  warning("No plotting available for class", class(x)[1],"\n")
  invisible(x)
}

print.Option <- function(x, digits=4, ...) {
  cat("Concise summary of valuation for", class(x)[1], "\n")
  print(round(unlist(x[1:7]), digits))
  invisible(x)
}

summary.Option <- function(object, digits=4, ...) {
  cat("Detailed summary of valuation for", class(object)[1], "\n")
  print(round(unlist(object[1:7]), digits))
  cat("with parameters\n")
  print(unlist(object[["parameters"]]))
  invisible(object)
}




