% $Id: BarrierOption.Rd,v 1.1 2003/11/29 01:09:06 edd Exp $
\name{BarrierOption}
\alias{BarrierOption}
\alias{BarrierOption.default}
\title{Barrier Option evaluation using Closed-Form solution}
\description{
This function evaluations an Barrier option on a common stock
using a closed-form solution. The option value as well as the common first
derivatives (\"Greeks\") are returned.}
\usage{
BarrierOption.default(barrType, type, underlying, strike, dividendYield,
                      riskFreeRate, maturity, volatility,
		      barrier, rebate=0.0)

\method{print}{Option}
\method{summary}{Option}
}
\arguments{
  \item{barrType}{A string with one of the values \code{downin},
    \code{downout}, \code{upin} or \code{upout}}
  \item{type}{A string with one of the values \code{call}, \code{put} or
    \code{straddle}}
  \item{underlying}{Current price of the underlying stock}
  \item{strike}{Strike price of the option}
  \item{dividendYield}{Continuous dividend yield (as a fraction) of the stock}
  \item{riskFreeRate}{Risk-free rate}
  \item{maturity}{Time to maturity (in fractional years)}
  \item{volatility}{Volatility of the underlying stock}
  \item{barrier}{Option barrier value}
  \item{rebate}{Optional option rebate, defaults to 0.0}
}
\value{
  An object of class \code{BarrierOption} (which inherits from class
  \code{\link{Option}}) is returned. It contains a list with the
  following components: 
  \item{value}{Value of option}
  \item{delta}{Change in value for a change in the underlying}
  \item{gamma}{Change in value for a change in delta}
  \item{vega}{Change in value for a change in the underlying's volatility}
  \item{theta}{Change in value for a change in delta}
  \item{rho}{Change in value for a change in time to maturity}
  \item{dividendRho}{Change in value for a change in delta}
  \item{parameters}{List with parameters with which object was created}
}
\details{
  A closed-form solution is used to value the Barrier Option. In the
  case of Barrier options, the calculations are from Haug's "Option
  pricing formulas" book (McGraw-Hill).
  
  Please see any decent Finance textbook for background reading, and
  the \code{QuantLib} documentation for details on the \code{QuantLib}
  implementation.
}
\references{\url{http://quantlib.org} for details on \code{QuantLib}.}
\author{Dirk Eddelbuettel \email{edd@debian.org} for the \R interface;
  the QuantLib Group for \code{QuantLib}}
\note{The interface might change in future release as \code{QuantLib}
  stabilises its own API.}
\seealso{\code{\link{AmericanOption}},\code{\link{EuropeanOption}}}

\examples{
BarrierOption("downin", "call", 100, 100, 0.02, 0.03, 0.5, 0.4, 90)
}
\keyword{misc}

