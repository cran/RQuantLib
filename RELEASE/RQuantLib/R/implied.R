## RQuantLib -- R interface to the QuantLib libraries
##
## Copyright 2002 Dirk Eddelbuettel <edd@debian.org>
##
## $Id: implied.R,v 1.3 2002/11/15 01:51:14 edd Exp $
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

EuropeanOptionImpliedVolatility <- function(x, ...) {
  if (is.null(class(x)))
    class(x) <- data.class(x)
  UseMethod("EuropeanOptionImpliedVolatility", x, ...)
}

EuropeanOptionImpliedVolatility.default <-
  function(type, value, underlying, strike, dividendYield,
            riskFreeRate, maturity, volatility) {
  val <- .Call("QL_EuropeanOptionImpliedVolatility",
               list(type=as.character(type),
		    value=as.double(value),
                    underlying=as.double(underlying),
                    strike=as.double(strike),
                    dividendYield=as.double(dividendYield),
                    riskFreeRate=as.double(riskFreeRate),
                    maturity=as.double(maturity),
                    volatility=as.double(volatility)),
               PACKAGE="RQuantLib")
  class(val) <- c("EuropeanOptionImpliedVolatility","ImpliedVolatility")
  val
}

AmericanOptionImpliedVolatility <- function(x, ...) {
  if (is.null(class(x)))
    class(x) <- data.class(x)
  UseMethod("AmericanOptionImpliedVolatility", x, ...)
}

AmericanOptionImpliedVolatility.default <-
  function(type, value, underlying, strike, dividendYield, riskFreeRate,
            maturity, volatility, timeSteps=150, gridPoints=151) {
  val <- .Call("QL_AmericanOptionImpliedVolatility",
               list(type=as.character(type),
		    value=as.double(value),
                    underlying=as.double(underlying),
                    strike=as.double(strike),
                    dividendYield=as.double(dividendYield),
                    riskFreeRate=as.double(riskFreeRate),
                    maturity=as.double(maturity),
                    volatility=as.double(volatility),
                    timeSteps=as.integer(timeSteps),
                    gridPoints=as.integer(gridPoints)),
               PACKAGE="RQuantLib")
  class(val) <- c("AmericanOptionImpliedVolatility","ImpliedVolatility")
  val
}

# dumps core :-/
#BinaryOptionImpliedVolatility <- function(x, ...) {
#  if (is.null(class(x)))
#    class(x) <- data.class(x)
#  UseMethod("BinaryOptionImpliedVolatility", x, ...)
#}

#BinaryOptionImpliedVolatility.default <-
#  function(type, value, underlying, strike, dividendYield, riskFreeRate,
#            maturity, volatility, cashPayoff=1) {
#  val <- .Call("QL_BinaryOptionImpliedVolatility",
#               list(type=as.character(type),
#		    value=as.double(value),
#                    underlying=as.double(underlying),
#                    strike=as.double(strike),
#                    dividendYield=as.double(dividendYield),
#                    riskFreeRate=as.double(riskFreeRate),
#                    maturity=as.double(maturity),
#                    volatility=as.double(volatility),
#                    cashPayoff=as.double(cashPayoff)),
#               PACKAGE="RQuantLib")
#  class(val) <- c("BinaryOptionImpliedVolatility","ImpliedVolatility")
#  val
#}

print.ImpliedVolatility <- function(x, ...) {
  cat(paste("Implied Volatility for", class(x)[1], "is", format(x[1]), "\n"))
  invisible(x)
}

summary.ImpliedVolatility <- function(object, ...) {
  cat(paste("Implied Volatility for", class(object)[1],
            "is", format(object[1]), "\n"))
  cat(paste("with parameters\n"))
  print(unlist(object[[2]]))
  invisible(object)
}
