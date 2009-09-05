% $Id: Enum.Rd 78 2009-05-30 18:31:48Z edd $
\name{Enum}
\alias{Enum}
\title{Documentation for parameters}
\description{
Reference for parameters when constructing a bond
}
\usage{}
\arguments{
\item{DayCounter}{an int value
       \tabular{ll}{
         \code{0} \tab Actual360 \cr
         \code{1} \tab Actual360FixEd \cr
         \code{2} \tab ActualActual \cr
         \code{3} \tab ActualBusiness252 \cr
         \code{4} \tab OneDayCounter \cr
         \code{5} \tab SimpleDayCounter \cr
         \code{anything else} \tab Thirty360
       }
}

\item{businessDayConvention}{an int value
      \tabular{ll}{
        \code{0} \tab Following \cr
        \code{1} \tab ModifiedFollowing \cr
        \code{2} \tab Preceding \cr
        \code{3} \tab ModifiedPreceding \cr
        \code{anything else} \tab UNadjusted 
      }
}

\item{compounding}{an int value
     \tabular{ll}{
       \code{0} \tab Simple \cr
       \code{1} \tab Compounded \cr
       \code{2} \tab Continuous \cr
       \code{3} \tab SimpleThenCompounded 
     }
}

\item{period or frequency}{an int value
     \tabular{ll}{
       \code{0} \tab NoFrequency \cr
       \code{1} \tab Once \cr
       \code{2} \tab Annual \cr
       \code{3} \tab Semiannual \cr
       \code{4} \tab EveryFourthMonth \cr
       \code{5} \tab Quarterly \cr
       \code{6} \tab BiMonthtly \cr
       \code{7} \tab EveryFourthWeek \cr
       \code{8} \tab Biweekly \cr
       \code{9} \tab Weekly \cr
       \code{anything else} \tab Daily 
     }
}

\item{date generation}{an int value to specify date generation rule
     \tabular{ll}{
        \code{0} \tab Backward \cr
        \code{1} \tab Forward \cr
        \code{2} \tab Zero \cr
        \code{3} \tab ThirdWednesday \cr
        \code{4} \tab Twentieth \cr
        \code{anything else} \tab TwentiethIMM
     }
}

}
\value{
None
}
\details{
\url{http://quantlib.org/reference/class_quant_lib_1_1_day_counter.html}
\url{http://quantlib.org/reference/group__datetime.html}

}
\references{\url{http://quantlib.org} for details on \code{QuantLib}.}
\author{Khanh Nguyen \email{knguyen@cs.umb.edu}}

\keyword{misc}

