% $Id: BinaryOption.Rd,v 1.3 2003/12/05 01:46:59 edd Exp $
\name{BinaryOption}
\alias{BinaryOption}
\alias{BinaryOption.default}
\title{Binary Option evaluation using Closed-Form solution}
\description{
This function evaluations an Binary option on a common stock
using a closed-form solution. The option value as well as the common first
derivatives (\"Greeks\") are returned.}
\usage{
BinaryOption.default(type, underlying, strike, dividendYield, riskFreeRate,
maturity, volatility, cashPayoff)

\method{print}{Option}
\method{summary}{Option}
}
\arguments{
  \item{type}{A string with one of the values \code{call}, \code{put} or
    \code{straddle}}
  \item{underlying}{Current price of the underlying stock}
  \item{strike}{Strike price of the option}
  \item{dividendYield}{Continuous dividend yield (as a fraction) of the stock}
  \item{riskFreeRate}{Risk-free rate}
  \item{maturity}{Time to maturity (in fractional years)}
  \item{volatility}{Volatility of the underlying stock}
  \item{cashPayoff}{Payout amount}
}
\value{
  An object of class \code{BinaryOption} (which inherits from class
  \code{\link{Option}}) is returned. It contains a list with the
  following components: 
  \item{value}{Value of option}
  \item{delta}{Change in value for a change in the underlying}
  \item{gamma}{Change in value for a change in delta}
  \item{vega}{Change in value for a change in the underlying's volatility}
  \item{theta}{Change in value for a change in delta}
  \item{rho}{Change in value for a change in time to maturity (\"time decay\")}  \item{dividendRho}{Change in value for a change in delta}
  \item{parameters}{List with parameters with which object was created}
}
\details{
  A closed-form solution is used to value the Binary Option.
  
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
BinaryOption("call", 100, 100, 0.02, 0.03, 0.5, 0.4, 10)
}
\keyword{misc}

