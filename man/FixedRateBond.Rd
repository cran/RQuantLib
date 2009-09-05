% $Id: FixedRateBond.Rd 106 2009-08-19 02:21:27Z knguyen $
\name{FixedRateBond}
\alias{FixedRateBond}
\alias{FixedRateBond.default}
\title{Fixed rate bond evaluation using discount curve solution}
\description{
  The \code{FixedRateBond} function evaluates a fixed rate bond using discount curve. 
  More specificly, the calculation is done by DiscountingBondEngine from QuantLib.
  The NPV, clean price, dirty price, accrued interest, yield and cash flows of the bond is returned. 
  For more detail, see the source codes in quantlib's test-suite. test-suite/bond.cpp

}
\usage{
\method{FixedRateBond}{default}(bond, rates, discountCurve, dateparams )
\method{plot}{Bond}
\method{print}{Bond}
\method{summary}{Bond}
}
\arguments{

\item{bond}{bond parameters:
	\tabular{ll}{
           \code{faceAmount}    \tab a double, face amount of the bond \cr
           \code{issueDate}     \tab a Date, the bond's issue date \cr
           \code{maturityDate}  \tab a Date, the bond's maturity date \cr
           \code{redemption}    \tab a double, percentage of the initial face amount that will be returned at maturity date. Normally set at 100 \cr
	   \code{effectiveDate} \tab a Date, the bond's effective date \cr
	}
}


\item{rates}{a double vector of rates}

\item{discountCurve}{ Can be on of the following:
\tabular{l}{
	\code{a DiscountCurve object}   \cr
	\code{A list that specifies a flat curve in two values "todayDate" and "rate"}   \cr
	\code{A list that specified 3 values to construct a DiscountCurve object, "params" , "tsQuotes", "times". For more detail, see example or the discountCurve function} \cr
}
}


\item{dateparams}{QuantLib's date parameters of the bond. 
	\tabular{ll}{
	   \code{settlementDays} \tab a double, settlement days. \cr
	   \code{calendar} \tab a string, either 'us' or 'uk' corresponding to US Goverment Bond calendar and UK Exchange calendar. \cr
	   \code{dayCounter} \tab a number or string, day counter convention. See \link{Enum} \cr
	   \code{period}  \tab a number or string, interest compounding interval. See \link{Enum} \cr
	   \code{businessDayConvention} \tab a number or string, business day convention. See \link{Enum} \cr
	   \code{terminationDateConvention} \tab a number or string, business day convention. See \link{Enum} \cr
	   \code{endOfMonth} \tab an integer with value 1 or 0. \cr
	   \code{dateGeneration} \tab an integer, date generation method. See \link{Enum} \cr
	}
	See example below.
}




}
\value{
  The \code{FixedRateBond} function returns an object of class
  \code{FixedRateBond} (which inherits from class 
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
  A discount curve is built to calculate the bond value. 

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

bond <- list(faceAmount=100, issueDate=as.Date("2004-11-30"),
             maturityDate=as.Date("2008-11-30"), redemption=100, 
             effectiveDate=as.Date("2004-11-30"))
dateparams <- list(settlementDays=1, calendar="us", dayCounter = 1, period=3, 
                   businessDayConvention = 4, terminationDateConvention=4,
                   dateGeneration=1, endOfMonth=1)
curve <- list(todayDate=as.Date("2004-11-04"), riskFreeRate=0.03)
rates <- c(0.02875)
                       
FixedRateBond(bond, rates, curve, dateparams)

params <- list(tradeDate=as.Date('2002-2-15'),
               settleDate=as.Date('2002-2-19'),
               dt=.25,
               interpWhat="discount",
               interpHow="loglinear")

tsQuotes <- list(d1w  =0.0382,
                 d1m  =0.0372,
                 fut1=96.2875,
                 fut2=96.7875,
                 fut3=96.9875,
                 fut4=96.6875,
                 fut5=96.4875,
                 fut6=96.3875,
                 fut7=96.2875,
                 fut8=96.0875,
                 s3y  =0.0398,
                 s5y  =0.0443,
                 s10y =0.05165,
                 s15y =0.055175)

times <- seq(0,10,.1)
curve <- list(params, tsQuotes, times)
FixedRateBond(bond, rates, curve, dateparams)

curve <- DiscountCurve(params, tsQuotes, times)
dateparams <- list(settlementDays=1, calendar="us", dayCounter = "Thirty360", 
                   period="Annual", businessDayConvention = "Preceding", 
                   terminationDateConvention="Preceding",
                   dateGeneration="Forward", endOfMonth=1)
FixedRateBond(bond, rates, curve, dateparams)

}
\keyword{misc}

