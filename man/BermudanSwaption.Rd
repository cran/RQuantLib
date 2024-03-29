\name{BermudanSwaption}
\alias{BermudanSwaption}
\alias{BermudanSwaption.default}
\alias{summary.G2Analytic}
\alias{summary.HWAnalytic}
\alias{summary.HWTree}
\alias{summary.BKTree}
\title{Bermudan swaption valuation using several short-rate models}
\description{
  \code{BermudanSwaption} prices a Bermudan swaption with specified
  strike and maturity (in years), after calibrating the selected
  short-rate model to an input swaption volatility matrix. Swaption
  maturities are in years down the rows, and swap tenors are in years
  along the columns, in the usual fashion. It is assumed that the
  Bermudan swaption is
  exercisable on each reset date of the underlying swaps.
}
\usage{
BermudanSwaption(params, ts, swaptionMaturities, swapTenors,
volMatrix)
}
\arguments{
  \item{params}{A list specifying the \code{tradeDate} (month/day/year),
    \code{settlementDate}, \code{startDate}, \code{maturity},  \code{payFixed}
    flag, \code{strike}, pricing \code{method}, and curve construction options
    (see \emph{Examples} section below). Curve construction options are
    \code{interpWhat} (possible values are \code{discount},
    \code{forward}, and \code{zero}) and
    \code{interpHow} (possible values are \code{linear},
    \code{loglinear}
    , and \code{spline}). Both \code{interpWhat} and \code{interpHow}
    are ignored when a flat yield curve is requested, but they must be
    present nevertheless.
    The pricing method can be one of the following (all short-rate models):
    \tabular{ll}{
      \code{G2Analytic} \tab G2 2-factor Gaussian model using analytic formulas.\cr
      \code{HWAnalytic} \tab Hull-White model using analytic formulas.\cr
      \code{HWTree} \tab Hull-White model using a tree.\cr
      \code{BKTree} \tab Black-Karasinski model using a tree.
      }
    }
  \item{ts}{A term structure built with DiscounCurve or market observables
     needed to construct the spot term
    structure of interest rates. A list of name/value pairs. See the
    help page for \code{\link{DiscountCurve}} for details.}
  \item{swaptionMaturities}{A vector containing the swaption maturities
    associated with the rows of the swaption volatility matrix.}
  \item{swapTenors}{A vector containing the underlying swap tenors
    associated with the columns of the swaption volatility matrix.}
  \item{volMatrix}{The swaption volatility matrix. Must be a 2D matrix
  stored by rows. See the example below.}
 }
\value{
  \code{BermudanSwaption} , if there are sufficient swaption vols to fit an affine model,
  returns a list containing calibrated model
  paramters (what parameters are returned depends on the model
  selected) along with:
  \item{price}{Price of swaption in basis points (actual price
  equals \code{price} times notional divided by 10,000)}
  \item{ATMStrike}{At-the-money strike}
  \item{params}{Input parameter list}
  If there are insufficient swaption vols to calibrate it throws a warning and returns \code{NULL}
}
\details{
  This function was update for \code{QuantLib} Version 1.7.1 or later. It
  introduces support for fixed-income instruments in \code{RQuantLib}. It implements the
  full function and should work in most cases as long as there are suuficient swaption vol
  data points to fit the affine model. At least 5 unique points are required. The data point
  search attempts to find 5 or more points with one being the closet match in terms in of
  expiration and maturity.

  See the \code{\link{SabrSwaption}} function for an alternative.
}
\references{
  Brigo, D. and Mercurio, F. (2001) \emph{Interest Rate Models: Theory and
    Practice}, Springer-Verlag, New York.

  For information about \code{QuantLib} see \url{https://www.quantlib.org/}.

  For information about \code{RQuantLib} see
\url{http://dirk.eddelbuettel.com/code/rquantlib.html}.

}
\author{Dominick Samperi}

\seealso{\code{\link{DiscountCurve}}, \code{\link{SabrSwaption}}}

\examples{
\dontrun{
# This data replicates sample code shipped with QuantLib 0.3.10 results
params <- list(tradeDate=as.Date('2002-2-15'),
               settleDate=as.Date('2002-2-19'),
               startDate=as.Date('2003-2-19'),
               maturity=as.Date('2008-2-19'),
               dt=.25,
               payFixed=TRUE,
               strike=.05,
               method="G2Analytic",
               interpWhat="discount",
               interpHow="loglinear")
setEvaluationDate(as.Date('2002-2-15'))
# Market data used to construct the term structure of interest rates
tsQuotes <- list(d1w  =0.05,
                 # d1m  =0.0372,
                 # fut1=96.2875,
                 # fut2=96.7875,
                 # fut3=96.9875,
                 # fut4=96.6875,
                 # fut5=96.4875,
                 # fut6=96.3875,
                 # fut7=96.2875,
                 # fut8=96.0875,
                 s3y  =0.05,
                 s5y  =0.05,
                 s10y =0.05,
                 s15y =0.05)

times=seq(0,14.75,.25)
swcurve=DiscountCurve(params,tsQuotes,times)
# Use this to compare with the Bermudan swaption example from QuantLib
#tsQuotes <- list(flat=0.04875825)

# Swaption volatility matrix with corresponding maturities and tenors
swaptionMaturities <- c(1,2,3,4,5)

swapTenors <- c(1,2,3,4,5)

volMatrix <- matrix(
    c(0.1490, 0.1340, 0.1228, 0.1189, 0.1148,
      0.1290, 0.1201, 0.1146, 0.1108, 0.1040,
      0.1149, 0.1112, 0.1070, 0.1010, 0.0957,
      0.1047, 0.1021, 0.0980, 0.0951, 0.1270,
      0.1000, 0.0950, 0.0900, 0.1230, 0.1160),
    ncol=5, byrow=TRUE)

volMatrix <- matrix(
    c(rep(.20,25)),
    ncol=5, byrow=TRUE)
# Price the Bermudan swaption
pricing <- BermudanSwaption(params, ts=.05,
                            swaptionMaturities, swapTenors, volMatrix)
summary(pricing)
}
}
\keyword{models}
