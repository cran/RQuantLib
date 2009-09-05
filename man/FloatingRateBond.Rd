% $Id: FloatingRateBond.Rd 87 2009-06-09 16:48:13Z knguyen $
\name{FloatingRateBond}
\alias{FloatingRateBond}
\alias{FloatingRateBond.default}
\title{Fixed rate bond evaluation using discount curve solution}
\description{
  The \code{FloatingRateBond} function evaluates a floating rate bond using discount curve. 
  More specificly, the calculation is done by DiscountingBondEngine from QuantLib.
  The NPV, clean price, dirty price, accrued interest, yield and cash flows of the bond is returned. 
  For more detail, see the source codes in quantlib's test-suite. test-suite/bond.cpp
}
\usage{
\method{FloatingRateBond}{default}(bond, gearings, spreads,
                                   caps, floors, index, 
                                   curve, dateparams )
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

\item{gearings}{a double vectors, gearings paramters of FloatingRateBond's constructor. See quantlib's doc on FloatingRateBond for more detail }
\item{spreads}{a double vectors, spreads paramters of FloatingRateBond's constructor. See quantlib's doc on FloatingRateBond for more detail }
\item{caps}{a double vectors, spreads paramters of FloatingRateBond's constructor. See quantlib's doc on FloatingRateBond for more detail }
\item{floors}{a double vectors, spreads paramters of FloatingRateBond's constructor. See quantlib's doc on FloatingRateBond for more detail }


\item{curve}{ a discount curve. Can be on of the following:
\tabular{l}{
	\code{a DiscountCurve object}   \cr
	\code{A list that specifies a flat curve in two values "todayDate" and "rate"}   \cr
	\code{A list that specified 3 values to construct a DiscountCurve object, "params" , "tsQuotes", "times". For more detail, see example or the discountCurve function} \cr
}
}

\item{index}{ IborIndex term structure. 
 \tabular{ll}{
   \code{type} \tab a string, currently support only "USDLibor" \cr
   \code{length} \tab an integer, length of the index \cr
   \code{inTermOf} \tab a string, period unit, currently support only 'Month' \cr
   \code{term} \tab a DiscountCurve object, the term structure of the index\cr
  }
}

\item{dateparams}{ A list specifying date paramters, \code{settlemenDays},
    \code{calendar} - a string 'us' or 'uk', \code{businessDayConvention}- an integer,
    \code{dayCounter}, \code{period}
    \code{terminationDateConvention} - an integer,
    \code{period} - an integer
    \code{dateGeneration} - an integer,
    \code{endOfMonth} - value 1 or 0.  For more detail, see \link{Enum} and 
    the quantlib docs for FloatingRateBond
    \code{fixingDays} - an integer

See \link{Enum} on how each values represents
}

}
\value{
  The \code{FloatingRateBond} function returns an object of class
  \code{FloatingRateBond} (which inherits from class 
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
                   businessDayConvention = 1, terminationDateConvention=1,
                   dateGeneration=0, endOfMonth=0, fixingDays = 1)

gearings <- c()

spreads <- c()

caps <- c()

floors <- c()


length2 <- list(todayDate=as.Date("2004-11-22"), riskFreeRate=0.025)

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

length3 <- list(params, tsQuotes, times)


# both curves are flat

curve <- length2
termstructure <- length2
iborindex <- list(type="USDLibor", length=6, 
                  inTermOf="Month", term=termstructure)                      
FloatingRateBond(bond, gearings, spreads, caps, floors, 
                 iborindex, curve, dateparams)


# one flat, another one is constructe

curve <- length2
termstructure <- length3
iborindex <- list(type="USDLibor", length=6, 
                  inTermOf="Month", term = termstructure)                      
FloatingRateBond(bond, gearings, spreads, caps, floors, 
                 iborindex, curve, dateparams)


curve <- length3
termstructure <- length2
iborindex <- list(type="USDLibor", length=6, 
                  inTermOf="Month", term = termstructure)                      
FloatingRateBond(bond, gearings, spreads, caps, floors, 
                 iborindex, curve, dateparams)


# both curves are constructed

curve <- length3
termstructure <- length3
iborindex <- list(type="USDLibor", length=6, 
                  inTermOf="Month", term = termstructure)                      
FloatingRateBond(bond, gearings, spreads, caps, floors, 
                 iborindex, curve, dateparams)


curve2 <- DiscountCurve(params, tsQuotes, times)
index2 <- DiscountCurve(params, tsQuotes, times)
ibor <- list(type="USDLibor", length=6, 
             inTermOf="Month", term = index2)
dateparams <- list(settlementDays=1, calendar="us", dayCounter = "Actual360", 
                   period="Semiannual", 
                   businessDayConvention = "Following", 
                   terminationDateConvention= "Following",
                   dateGeneration= "Forward", 
                   endOfMonth=0, fixingDays = 1)
FloatingRateBond(bond, gearings, spreads, caps, floors,
                  ibor, curve2, dateparams)

}
\keyword{misc}

