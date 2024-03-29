\name{EuropeanOption}
\alias{EuropeanOption}
\alias{EuropeanOption.default}
\title{European Option evaluation using Closed-Form solution}
\description{
  The \code{EuropeanOption} function evaluations an European-style
  option on a common stock using the Black-Scholes-Merton solution. The
  option value, the common first derivatives ("Greeks") as well as the
  calling parameters are returned.
}
\usage{
\method{EuropeanOption}{default}(type, underlying, strike,
	dividendYield, riskFreeRate, maturity, volatility, 
	discreteDividends, discreteDividendsTimeUntil)
}
\arguments{
  \item{type}{A string with one of the values \code{call} or \code{put}}
  \item{underlying}{Current price of the underlying stock}
  \item{strike}{Strike price of the option}
  \item{dividendYield}{Continuous dividend yield (as a fraction) of the stock}
  \item{riskFreeRate}{Risk-free rate}
  \item{maturity}{Time to maturity (in fractional years)}
  \item{volatility}{Volatility of the underlying stock}
  \item{discreteDividends}{Vector of discrete dividends (optional)}
  \item{discreteDividendsTimeUntil}{Vector of times to discrete dividends 
    (in fractional years, optional)}
}
\value{
  The \code{EuropeanOption} function returns an object of class
  \code{EuropeanOption} (which inherits from class 
  \code{\link{Option}}). It contains a list with the following
  components:
  \item{value}{Value of option}
  \item{delta}{Sensitivity of the option value for a change in the underlying}
  \item{gamma}{Sensitivity of the option delta for a change in the underlying}
  \item{vega}{Sensitivity of the option value for a change in the
    underlying's volatility} 
  \item{theta}{Sensitivity of the option value for a change in t, the
    remaining time to maturity}
  \item{rho}{Sensitivity of the option value for a change in the
    risk-free interest rate}
  \item{dividendRho}{Sensitivity of the option value for a change in the
    dividend yield}

}
\details{
  The well-known closed-form solution derived by Black, Scholes and
  Merton is used for valuation. Implied volatilities are calculated
  numerically.

  Please see any decent Finance textbook for background reading, and the
  \code{QuantLib} documentation for details on the \code{QuantLib}
  implementation.  
}
\references{\url{https://www.quantlib.org/} for details on \code{QuantLib}.}
\author{Dirk Eddelbuettel \email{edd@debian.org} for the \R interface;
  the QuantLib Group for \code{QuantLib}}
\note{The interface might change in future release as \code{QuantLib}
  stabilises its own API.}
\seealso{\code{\link{EuropeanOptionImpliedVolatility}},
  \code{\link{EuropeanOptionArrays}},
  \code{\link{AmericanOption}},\code{\link{BinaryOption}}}

\examples{
## simple call with unnamed parameters
EuropeanOption("call", 100, 100, 0.01, 0.03, 0.5, 0.4)
## simple call with some explicit parameters, and slightly increased vol:
EuropeanOption(type="call", underlying=100, strike=100, dividendYield=0.01, 
riskFreeRate=0.03, maturity=0.5, volatility=0.5)
## simple call with slightly shorter maturity: QuantLib 1.7 compiled with 
## intra-day time calculation support with create slightly changed values
EuropeanOption(type="call", underlying=100, strike=100, dividendYield=0.01, 
riskFreeRate=0.03, maturity=0.499, volatility=0.5)
}
\keyword{misc}

