% $Id: EuropeanOption.Rd,v 1.1 2002/02/26 03:48:30 edd Exp $
\name{EuropeanOption}
\alias{EuropeanOption}
\alias{EuropeanOption.default}
\title{European Option evaluation using Closed-Form solution}
\description{
  The \code{EuropeanOption} function evaluations an European-style
  option on a common stock using the Black-Scholes-Merton solution. The
  option value, the common first derivatives (\"Greeks\") as well as the
  calling parameters are returned.
}
\usage{
EuropeanOption.default(type, underlying, strike, dividendYield, riskFreeRate, maturity, volatility)

\method{plot}{Option}
\method{print}{Option}
\method{summary}{Option}
}
\arguments{
  \item{type}{A string with one of the values \code{call}, \code{put} or
    \code{straddle}}
  \item{value}{Value of the option (used only for ImpliedVolatility calculation)}
  \item{underlying}{Current price of the underlying stock}
  \item{strike}{Strike price of the option}
  \item{dividendYield}{Continuous dividend yield (as a fraction) of the stock}
  \item{riskFreeRate}{Risk-free rate}
  \item{maturity}{Time to maturity (in fractional years)}
  \item{volatility}{Volatility of the underlying stock}
}
\value{
  The \code{EuropeanOption} function returns an object of class
  \code{EuropeanOption} (which inherits from class 
  \code{\link{Option}}). It contains a list with the following
  components can be returned: 
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
  The well-known closed-form solution derived by Black, Scholes and
  Merton is used for valuation. Implied volatilities are calculated
  numerically.

  Please see any decent Finance textbook for background reading, and the
  \code{QuantLib} documentation for details on the \code{QuantLib}
  implementation.  
}
\references{\url{http://quantlib.org} for details on \code{QuantLib}.}
\author{Dirk Eddelbuettel \email{edd@debian.org} for the \R interface;
  the QuantLib Group for \code{QuantLib}}
\note{The interface might change in future release as \code{QuantLib}
  stabilises its own API.}
\seealso{\code{\link{EuropeanOptionImpliedVolatility}},
  \code{\link{EuropeanOptionArrays}},
  \code{\link{AmericanOption}},\code{\link{BinaryOption}}}

\examples{
# simple call with unnamed parameters
EuropeanOption("call", 100, 100, 0.01, 0.03, 0.5, 0.4)
# simple call with some explicit parameters
EuropeanOption("call",100, 100, 0.01, 0.03, 0.5, 0.4)
}
\keyword{misc}

