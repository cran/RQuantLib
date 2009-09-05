% $Id: ZeroCouponBond.Rd 106 2009-08-19 02:21:27Z knguyen $
\name{ZeroCouponBond}
\alias{ZeroCouponBond}
\alias{ZeroCouponBond.default}
\title{Zero-oupon bond evaluation using discount curve solution}
\description{
  The \code{ZeroCouponBond} function evaluates a zero-coupon plainly using discount curve. 
  More specificly, the calculation is done by DiscountingBondEngine from QuantLib.
  The NPV, clean price, dirty price, accrued interest, yield and cash flows of the bond is returned. 
  For more detail, see the source codes in quantlib's test-suite. test-suite/bond.cpp

}
\usage{
\method{ZeroCouponBond}{default}(bond, discountCurve, dateparams)

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
	}
}



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
	   \code{businessDayConvention} \tab a number or string, business day convention. See \link{Enum} \cr
	}
	See example below.
}

}
\value{
  The \code{ZeroCouponBond} function returns an object of class
  \code{ZeroCouponBond} (which inherits from class 
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
# simple call with unnamed parameters
bond <- list(faceAmount=100, issueDate=as.Date("2004-11-30"),
             maturityDate=as.Date("2008-11-30"), redemption=100 )

dateparams <-list(settlementDays=1, calendar="us", businessDayConvention=4)

discountCurve <- list(todayDate=as.Date("2004-11-04"), riskFreeRate=0.03)

ZeroCouponBond(bond, discountCurve, dateparams)

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
discountCurve <- list(params, tsQuotes, times)

# depreciated
ZeroCouponBond(bond, discountCurve, dateparams)

# construct a curve
curves <- DiscountCurve(params, tsQuotes, times)
ZeroCouponBond(bond, curves, dateparams)

#construct a flat curve
flatquote <- list(flat=0.04)
flatCurve <- DiscountCurve(params, flatquote, times)
dateparams <-list(settlementDays=1, calendar="us", 
                  businessDayConvention="Following")

ZeroCouponBond(bond, flatCurve, dateparams)

}
\keyword{misc}

