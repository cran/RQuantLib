\name{FittedBondCurve}
\alias{FittedBondCurve}
\alias{FittedBondCurve.default}
\alias{plot.FittedBondCurve}
\title{Returns the discount curve (with zero rates and forwards) given times}
\description{
  \code{FittedBondCurve} fits a term structure to a set of bonds
    using three different fitting methodologies. For more detail,
    see QuantLib/Example/FittedBondCurve.
}

\usage{
FittedBondCurve(curveparams, lengths, coupons, dateparams)
}

\arguments{
\item{curveparams}{curve parameters
	\tabular{ll}{
		\code{method} \tab a string, fitting methods: "ExponentialSplinesFitting", "SimplePolynomialFitting", "NelsonSiegelFitting"\cr
		\code{origDate} \tab a Date, starting date of the curve \cr
	}
}
\item{lengths}{an integer vector, length of the bonds in year}
\item{coupons}{a double vector, coupon rate of the bonds}

\item{dateparams}{QuantLib's date parameters. 
	\tabular{ll}{
	   \code{settlementDays} \tab a double, settlement days. \cr
	   \code{dayCounter} \tab a number or string, day counter convention. See \link{Enum} \cr
	   \code{period}  \tab a number or string, interest compounding interval. See \link{Enum} \cr
	   \code{businessDayConvention} \tab a number or string, business day convention. See \link{Enum} \cr
	}
	See example below.
}

}
\value{
 \code{table}, a three columns "date - zeroRate - discount" data frame 
}

\details{
 Please see any decent Finance textbook for background reading, and the
  \code{QuantLib} documentation for details on the \code{QuantLib}
  implementation.
}
\references{ 
\url{http://quantlib.org/reference/class_quant_lib_1_1_fitted_bond_discount_curve.html}
}
\author{Khanh Nguyen \email{knguyen@cs.umb.edu} for the inplementation; Dirk Eddelbuettel \email{edd@debian.org} for the \R interface;
  the QuantLib Group for \code{QuantLib}
}


\examples{

lengths <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)
coupons <- c( 0.0200, 0.0225, 0.0250, 0.0275, 0.0300,
              0.0325, 0.0350, 0.0375, 0.0400, 0.0425,
              0.0450, 0.0475, 0.0500, 0.0525, 0.0550 )
dateparams <- list(settlementDays=0, period="Annual", 
                   dayCounter="SimpleDayCounter", 
                  businessDayConvention ="Unadjusted")
curveparams <- list(method="ExponentialSplinesFitting", 
                    origDate = Sys.Date())
curve <- FittedBondCurve(curveparams, lengths, coupons, dateparams)
library(zoo)
z <- zoo(curve$table$zeroRates, order.by=curve$table$date)
plot(z)
}
