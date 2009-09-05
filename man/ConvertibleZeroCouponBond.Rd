% $Id: ConvertibleZeroCouponBond 84 2009-06-19 knguyen $
\name{ConvertibleZeroCouponBond}
\alias{ConvertibleZeroCouponBond}
\alias{ConvertibleZeroCouponBond.default}
\title{Convertible Zero Coupon Bond evaluation}
\description{
The \code{ConvertibleZeroCouponBond} function setups and evaluates a ConvertibleFixedCouponBond using QuantLib's BinomialConvertibleEngine \url{http://quantlib.org/reference/class_quant_lib_1_1_binomial_convertible_engine.html} and BlackScholesMertonProcess \url{http://quantlib.org/reference/class_quant_lib_1_1_black_scholes_merton_process.html}. The NPV, clean price, dirty price, accrued interest, yield and cash flows of the bond is returned. For detail, see test-suite/convertiblebond.cpp.

}
\usage{
\method{ConvertibleZeroCouponBond}{default}(bondparams, process, dateparams)
\method{plot}{Bond}
\method{print}{Bond}
\method{summary}{Bond}
}
\arguments{
\item{bondparams}{bond parameters:
	\tabular{ll}{
	   \code{exercuse} 	\tab a string, either "eu" for European option, or "am" for American option. \cr
           \code{faceAmount}    \tab a double, face amount of the bond\cr
           \code{issueDate}     \tab a Date, the bond's issue date\cr
           \code{maturityDate}  \tab a Date, the bond's maturity date\cr
           \code{redemption}    \tab a double, percentage of the initial face amount that will be returned at maturity date. Normally set at 100\cr
	   \code{dividendSchedule} \tab a data frame whose columns are "Type", "Amount", "Rate", and "Date" corresponding to QuantLib's DividendSchedule \cr
	   \code{callabilitySchedule} \tab a data frame whose columns are "Price", "Type" and "Date" corresponding to QuantLib's CallabilitySchedule \cr
	   \code{creditSpread}  \tab a double, credit spread parameter in the constructor of the bond. \cr
	   \code{conversitionRatio}  \tab a double, conversition ratio parameter in the constructor of the bond. \cr
	}
}

\item{process}{arguments to construct a BlackScholes process and set up the binomial pricing engine for this bond. 
	\tabular{ll}{
	   \code{underlying} \tab a double, flat underlying term structure \cr
	   \code{volatility} \tab a double, flat volatility term structure \cr
	   \code{dividendYield} \tab a DiscountCurve object \cr
	   \code{riskFreeRate} \tab a DiscountCurve object \cr
	}
}

\item{dateparams}{QuantLib's date parameters of the bond. 
	\tabular{ll}{
	   \code{settlementDays} \tab a double, settlement days. \cr
	   \code{calendar} \tab a string, either 'us' or 'uk' corresponding to US Goverment Bond calendar and UK Exchange calendar. \cr
	   \code{dayCounter} \tab a number or string, day counter convention. See \link{Enum} \cr
	   \code{period}  \tab a number or string, interest compounding interval. See \link{Enum} \cr
	   \code{businessDayConvention} \tab a number or string, business day convention. See \link{Enum} \cr
	   \code{todayDate} \tab a date, date of today. \cr
	}
	See example below.
}



}
\value{
  The \code{ConvertibleZeroCouponBond} function returns an object of class
  \code{ConvertibleZeroCouponBond} (which inherits from class 
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
\references{ 
\url{http://quantlib.org/reference/class_quant_lib_1_1_convertible_zero_coupon_bond.html}
}
\author{
Khanh Nguyen \email{knguyen@cs.umb.edu} for the inplementation; Dirk Eddelbuettel \email{edd@debian.org} for the \R interface;
  the QuantLib Group for \code{QuantLib}
}
\examples{
#this follow an example in test-suite/convertiblebond.cpp for ConvertibleZeroCouponBond
params <- list(tradeDate=Sys.Date()-2,
               settleDate=Sys.Date(),
               dt=.25,
               interpWhat="discount",
               interpHow="loglinear")
times <- seq(0,10,.1)


dividendYield <- DiscountCurve(params, list(flat=0.02), times)
riskFreeRate <- DiscountCurve(params, list(flat=0.05), times)

dividendSchedule <- data.frame(Type=character(0), Amount=numeric(0),
                            Rate = numeric(0), Date = as.Date(character(0)))
callabilitySchedule <- data.frame(Price = numeric(0), Type=character(0),
                          Date = as.Date(character(0)))

process <- list(underlying=50, divYield = dividendYield,
                rff = riskFreeRate, volatility=0.15)

today <- Sys.Date()
bondparams <- list(exercise="am", faceAmount=100, divSch = dividendSchedule, 
                   callSch = callabilitySchedule, redemption=100, 
                   creditSpread=0.005, conversionRatio = 0.0000000001, 
                   issueDate=as.Date(today+2), 
                   maturityDate=as.Date(today+3650))
dateparams <- list(settlementDays=3, 
                   dayCounter="Actual360", 
                   period = "Once", calendar = "us", 
                   businessDayConvention="Following", 
                   todayDate=as.Date(today))

ConvertibleZeroCouponBond(bondparams, process, dateparams)

}
