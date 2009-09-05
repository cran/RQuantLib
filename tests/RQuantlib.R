
stopifnot(require(RQuantLib))

## values from Quantlib's test-suite
## Reference: Haug, Option Pricing Formulas, McGraw-Hill, 1998
##
## and generally sourced from the code in the test-suite/
## directory of the QuantLib distribution

## europeanoption.cpp:  call value == 2.1334
print(EuropeanOption("call", underlying=60, strike=65, div=0, riskFree=0.08,
                     maturity=0.25, vol=0.3), digits=5)
## europeanoption.cpp:  put value == 2.4648
print(EuropeanOption("put", underlying=100, strike=95, div=0.05, riskFree=0.1,
                     maturity=0.5, vol=0.2), digits=5)

## europeanoption.cpp:  call delta == 0.5946
print(EuropeanOption("call", underlying=105, strike=100,div=0.1,riskFree=0.1,
                     maturity=0.5, vol=0.36), digits=4)
## europeanoption.cpp:  put delta == -0.3566
print(EuropeanOption("put", underlying=105, strike=100,div=0.1,riskFree=0.1,
                     maturity=0.5, vol=0.36), digits=4)

## europeanoption.cpp:  call gamma == 0.0278
print(EuropeanOption("call", underlying=55, strike=60,div=0.0,riskFree=0.1,
                     maturity=0.75, vol=0.30), digits=4)
## europeanoption.cpp:  put gamma == 0.0278
print(EuropeanOption("put", underlying=55, strike=60,div=0.0,riskFree=0.1,
                     maturity=0.75, vol=0.30), digits=4)

## europeanoption.cpp:  call vega == 18.9358
print(EuropeanOption("call", underlying=55, strike=60,div=0.0,riskFree=0.1,
                     maturity=0.75, vol=0.30), digits=4)
## europeanoption.cpp:  put vega == 18.9358
print(EuropeanOption("put", underlying=55, strike=60,div=0.0,riskFree=0.1,
                     maturity=0.75, vol=0.30), digits=4)


## americanoption.cpp:  call value == 10.0089 -- we show 10.00606
print(AmericanOption("call", underlying=110, strike=100, div=0.1, riskFree=0.1,
                     maturity=0.1, vol=0.15), digits=5)
## americanoption.cpp:  put value == 0.3159
print(AmericanOption("call", underlying=90, strike=100, div=0.1, riskFree=0.1,
                     maturity=0.1, vol=0.25), digits=5)


## barrier: down and out call == 9.0246
print(BarrierOption("downout", barrier=95, rebate=3, type="call",
                    strike=90, underlying=100, div=0.04, riskF=0.08,
                    mat=0.5, vol=0.25), digits=4)
## barrier: down and in call == 7.7627
print(BarrierOption("downin", barrier=95, rebate=3, type="call",
                    strike=90, underlying=100, div=0.04, riskF=0.08,
                    mat=0.5, vol=0.25), digits=4)


## binary aka digital: put == 2.6710
print(BinaryOption(binType="cash", type="put", excType="european",
                   strike=80, underl=100, div=0.06, r=0.06,
                   mat=0.75, vol=0.35, cash=10), digits=4)

## asianoption.cpp:  put == 4.6922 (from testAnalyticContinuousGeometricAveragePrice())
print( AsianOption("geometric", "put", underlying=80, strike=85, div=-0.03, riskFree=0.05, maturity=0.25, vol=0.2))

# simple call with unnamed parameters
bond <- list(faceAmount=100, issueDate=as.Date("2004-11-30"),
             maturityDate=as.Date("2008-11-30"), redemption=100 )
dateparams <-list(settlementDays=1, calendar="us", businessDayConvention=4)
discountCurve <- list(todayDate=as.Date("2004-11-04"), riskFreeRate=0.03)
print(ZeroCouponBond(bond, discountCurve, dateparams))

## bond.cpp: examples from Fixed Income page of Matlab
ZeroYield(95, 100, as.Date("1993-6-24"), as.Date("1993-11-1"))

## bond.cpp: test theoretical price of bond by its yield
ZeroPriceByYield(0.1478, 100, as.Date("1993-6-24"), as.Date("1993-11-1"))

## bond.cpp: test theoretical yield of a fixed rate bond, = 0.0307
FixedRateBondYield(,99.282, 100000, as.Date("2004-11-30"), as.Date("2008-11-30"), 3, , c(0.02875), , , , ,as.Date("2004-11-30"))

## bond.cpp: test theoretical price of a fixed rate bond  = 99.2708
FixedRateBondPriceByYield(,0.0307, 100000, as.Date("2004-11-30"), as.Date("2008-11-30"), 3, , c(0.02875), , , , ,as.Date("2004-11-30"))

## bond.cpp
bond <- list(faceAmount=100, issueDate=as.Date("2004-11-30"),
             maturityDate=as.Date("2008-11-30"), redemption=100, 
             effectiveDate=as.Date("2004-11-30"))
dateparams <- list(settlementDays=1, calendar="us", dayCounter = 1, period=3, 
                   businessDayConvention = 4, terminationDateConvention=4,
                   dateGeneration=1, endOfMonth=1)
curve <- list(todayDate=as.Date("2004-11-04"), riskFreeRate=0.03)
rates <- c(0.02875)
                       
print(FixedRateBond(bond, rates, curve, dateparams))

## bond.cpp FloatingRateBond, following test-suite/bonds.cpp
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
curve <- length2
termstructure <- length2
iborindex <- list(type="USDLibor", length=6, 
                  inTermOf="Month", term=termstructure)                      
print(FloatingRateBond(bond, gearings, spreads, caps, floors, 
                 iborindex, curve, dateparams))

## CallableBond, following Example/CallableBond
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

## ConvertibleFixedCouponBond, test-suite/convertiblebond.cpp for ConvertibleFixedCouponBond

#set up arguments to build a pricing engine.
today <- as.Date("2009-08-01")

params <- list(tradeDate=today-2,
               settleDate=today,
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
coupon <- c(0.05)
ConvertibleFixedCouponBond(bondparams, coupon, process, dateparams)

## ConvertibleFloatingCouponBond, test-suite/convertiblebond.cpp for ConvertibleZeroCouponBond

today <- as.Date("2009-08-01")

params <- list(tradeDate=today-2,
               settleDate=today,
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

lengths <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)
coupons <- c( 0.0200, 0.0225, 0.0250, 0.0275, 0.0300,
              0.0325, 0.0350, 0.0375, 0.0400, 0.0425,
              0.0450, 0.0475, 0.0500, 0.0525, 0.0550 )
curvedateparams <- list(settlementDays=0, period="Annual", 
                   dayCounter="SimpleDayCounter", 
                  businessDayConvention ="Unadjusted")
curveparams <- list(method="ExponentialSplinesFitting", 
                    origDate = today)
curve <- FittedBondCurve(curveparams, lengths, coupons, curvedateparams)
iborindex <- list(type="USDLibor", length=6, 
                  inTermOf="Month", term=curve)   
spreads <- c()
ConvertibleFloatingCouponBond(bondparams, iborindex,spreads, process, dateparams)

## ConvertibleZeroCouponBond, test-suite/convertiblebond.cpp for ConvertibleZeroCouponBond
today <- as.Date("2009-08-01")
params <- list(tradeDate=today-2,
               settleDate=today,
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


