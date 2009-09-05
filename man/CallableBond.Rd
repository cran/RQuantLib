% $Id: CallableBond.Rd 87 2009-06-09 16:48:13Z knguyen $
\name{CallableBond}
\alias{CallableBond}
\alias{CallableBond.default}
\title{CallableBond evaluation}
\description{
  The \code{CallableBond} function sets up and evaluates a callable fixed rate bond using Hull-White model and a TreeCallableFixedBondEngine pricing engine.  
  For more detail, see the source codes in quantlib's example folder, Examples/CallableBond/CallableBond.cpp

}
\usage{
\method{CallableBond}{default}(bondparams, hullWhite, coupon, dateparams)
\method{plot}{Bond}
\method{print}{Bond}
\method{summary}{Bond}
}
\arguments{
\item{bondparams}{bond parameters:
	\tabular{ll}{
           \code{faceAmount}    \tab a double, face amount of the bond\cr
           \code{issueDate}     \tab a Date, the bond's issue date\cr
           \code{maturityDate}  \tab a Date, the bond's maturity date\cr
           \code{redemption}    \tab a double, percentage of the initial face amount that will be returned at maturity date. Normally set at 100\cr
	}
}
\item{hullWhite}{arguments that are needed to set up a HullWhite pricing engine in QuantLib: 
	\tabular{ll}{
	   \code{term}  \tab a double, to set up a flat rate yield term structure \cr
	   \code{alpha} \tab a double, Hull-White model's alpha value \cr
	   \code{sigma} \tab a double, Hull-White model's sigma value \cr
	   \code{gridIntervals}. \tab a double, time intervals parameter to set up the TreeCallableFixedBondEngine \cr
	}
	Currently, the codes only support a flat rate yield term structure. For more detail, see QuantLib's doc on HullWhite and TreeCallableFixedBondEngine.
}
\item{coupon}{ a double vector of coupon rates}
\item{dateparams}{QuantLib's date parameters of the bond. 
	\tabular{ll}{
	   \code{settlementDays} \tab a double, settlement days. \cr
	   \code{calendar} \tab a string, either 'us' or 'uk' corresponding to US Goverment Bond calendar and UK Exchange calendar. \cr
	   \code{dayCounter} \tab a number or string, day counter convention. See \link{Enum} \cr
	   \code{period}  \tab a number or string, interest compounding interval. See \link{Enum} \cr
	   \code{businessDayConvention} \tab a number or string, business day convention. See \link{Enum} \cr
	   \code{terminationDateConvention} \tab a number or string, termination day convention. See \link{Enum} \cr
	}
	See example below.}
}
\value{
  The \code{CallableBond} function returns an object of class
  \code{CallableBond} (which inherits from class 
  \code{Bond}). It contains a list with the following
  components:
  \item{NPV}{net present value of the bond}
  \item{cleanPrice}{price price of the bond}
  \item{dirtyPrice}{dirty price of the bond}
  \item{accruedAmount}{accrued amount of the bond}
  \item{yield}{yield of the bond}
  \item{cashFlows}{cash flows of the bond}
}
\details{
  Please see any decent Finance textbook for background reading, and the
  \code{QuantLib} documentation for details on the \code{QuantLib}
  implementation.  
}
\references{\url{http://quantlib.org} for details on \code{QuantLib}.}

\author{Khanh Nguyen \email{knguyen@cs.umb.edu} for the inplementation; Dirk Eddelbuettel \email{edd@debian.org} for the \R interface;
  the QuantLib Group for \code{QuantLib}}
\note{The interface might change in future release as \code{QuantLib}
  stabilises its own API.}
\examples{
#set-up a HullWhite according to example from QuantLib
HullWhite <- list(term = 0.055, alpha = 0.03, sigma = 0.01,
                  gridIntervals = 40)

#callability schedule dataframe
Price <- rep(as.double(100),24)
Type <- rep(as.character("C"), 24)
Date <- seq(as.Date("2006-09-15"), by = '3 months', length = 24)
callSch <- data.frame(Price, Type, Date)
callSch$Type <- as.character(callSch$Type)

bondparams <- list(faceAmount=100, issueDate = as.Date("2004-09-16"),
                   maturityDate=as.Date("2012-09-16"), redemption=100,
                   callabilitySchedule = callSch)
dateparams <- list(settlementDays=3, calendar="us", 
                   dayCounter = "ActualActual", 
                   period="Quarterly", 
                   businessDayConvention = "Unadjusted", 
                   terminationDateConvention= "Unadjusted")
coupon <- c(0.0465)

CallableBond(bondparams, HullWhite, coupon, dateparams)

}
\keyword{misc}

