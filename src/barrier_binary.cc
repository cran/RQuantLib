// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002, 2003, 2004 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: barrier_binary.cc,v 1.7 2004/12/28 03:28:02 edd Exp $
//
// This file is part of the RQuantLib library for GNU R.
// It is made available under the terms of the GNU General Public
// License, version 2, or at your option, any later version,
// incorporated herein by reference.
//
// This program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public
// License along with this program; if not, write to the Free
// Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
// MA 02111-1307, USA

// NB can be build standalone as   PKG_LIBS=-lQuantLib R CMD SHLIB RQuantLib.cc

#include <ql/quantlib.hpp>	// make QuantLib known

using namespace QuantLib;

extern "C" {

#include "rquantlib.h"

  SEXP QL_BinaryOption(SEXP optionParameters) {

    const int nret = 8;		// dimension of return list

    char *type = CHAR(STRING_ELT( getListElement(optionParameters, 
						 "type"), 0));
    double underlying = REAL(getListElement(optionParameters, 
					    "underlying"))[0];
    double strike = REAL(getListElement(optionParameters, "strike"))[0];
    Spread dividendYield = REAL(getListElement(optionParameters, 
					       "dividendYield"))[0];
    Rate riskFreeRate = REAL(getListElement(optionParameters, 
					    "riskFreeRate"))[0];
    Time maturity = REAL(getListElement(optionParameters, "maturity"))[0];
    int length = int(maturity * 360); // FIXME: this could be better

    double volatility = REAL(getListElement(optionParameters, 
					    "volatility"))[0];
    double cashPayoff = REAL(getListElement(optionParameters, 
					    "cashPayoff"))[0];

    Option::Type optionType;
    if (!strcmp(type, "call")) {
      optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
      optionType = Option::Put;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    // new QuantLib 0.3.5 framework: digitals, updated for 0.3.7
    Date today = Date::todaysDate();
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<TermStructure> qTS = makeFlatCurve(today, qRate, dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<TermStructure> rTS = makeFlatCurve(today, rRate, dc);
    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
      makeFlatVolatility(today, vol, dc);
    boost::shared_ptr<PricingEngine> engine(new AnalyticEuropeanEngine);

    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new CashOrNothingPayoff(optionType, strike, cashPayoff));

    Date exDate = today.plusDays(length);

    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));

    spot->setValue(underlying);
    qRate->setValue(dividendYield);
    rRate->setValue(riskFreeRate);
    vol  ->setValue(volatility);

    boost::shared_ptr<BlackScholesProcess> 
      stochProcess(new BlackScholesProcess(
                RelinkableHandle<Quote>(spot),
                RelinkableHandle<TermStructure>(qTS),
                RelinkableHandle<TermStructure>(rTS),
                RelinkableHandle<BlackVolTermStructure>(volTS)));

    VanillaOption opt(stochProcess, payoff, exercise, engine);

    // now prepare R structure for return of results
    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements

    insertListElement(rl, nm, 0, opt.NPV(), "value");
    insertListElement(rl, nm, 1, opt.delta(), "delta");
    insertListElement(rl, nm, 2, opt.gamma(), "gamma");
    insertListElement(rl, nm, 3, opt.vega(), "vega");
    insertListElement(rl, nm, 4, opt.theta(), "theta");
    insertListElement(rl, nm, 5, opt.rho(),   "rho");
    insertListElement(rl, nm, 6, opt.dividendRho(), "divRho");

    SET_VECTOR_ELT(rl, 7, optionParameters);
    SET_STRING_ELT(nm, 7, mkChar("parameters"));

    setAttrib(rl, R_NamesSymbol, nm);
    // setAttrib(rl, R_ClassSymbol, ScalarString(mkChar("EuropeanOption")));

    UNPROTECT(2);
    return(rl);
  }

  // dumped core when we tried last
#if 0
  SEXP QL_BinaryOptionImpliedVolatility(SEXP optionParameters) {
    const int nret = 2;		// dimension of return list
    char *type = CHAR(STRING_ELT(getListElement(optionParameters, "type"),0));
    double value = REAL(getListElement(optionParameters, "value"))[0];
    double underlying = REAL(getListElement(optionParameters,"underlying"))[0];
    double strike = REAL(getListElement(optionParameters,"strike"))[0];	
    Spread dividendYield = REAL(getListElement(optionParameters, 
					       "dividendYield"))[0];
    Rate riskFreeRate = REAL(getListElement(optionParameters, 
					    "riskFreeRate"))[0];
    Time maturity = REAL(getListElement(optionParameters, "maturity"))[0];
    int length = int(maturity * 360); // FIXME: this could be better
    double volatility = REAL(getListElement(optionParameters,"volatility"))[0];
    double cashPayoff = REAL(getListElement(optionParameters,"cashPayoff"))[0];

    Option::Type optionType;
    if (!strcmp(type, "call")) {
      optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
      optionType = Option::Put;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    // new QuantLib 0.3.5 framework: digitals, updated for 0.3.7
    Date today = Date::todaysDate();
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<TermStructure> qTS = makeFlatCurve(today, qRate, dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<TermStructure> rTS = makeFlatCurve(today, rRate, dc);
    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
      makeFlatVolatility(today, vol, dc);
    boost::shared_ptr<PricingEngine> engine(new AnalyticEuropeanEngine);

    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new CashOrNothingPayoff(optionType, strike, cashPayoff));
    Date exDate = today.plusDays(length);

    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));

    spot->setValue(underlying);
    qRate->setValue(dividendYield);
    rRate->setValue(riskFreeRate);
    vol  ->setValue(volatility);

    boost::shared_ptr<BlackScholesStochasticProcess> 
      stochProcess(new BlackScholesStochasticProcess(
                RelinkableHandle<Quote>(spot),
                RelinkableHandle<TermStructure>(qTS),
                RelinkableHandle<TermStructure>(rTS),
                RelinkableHandle<BlackVolTermStructure>(volTS)));

    VanillaOption opt(stochProcess, payoff, exercise, engine);

    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
//     insertListElement(rl, nm, 0, BO.impliedVolatility(value), "impliedVol");
    SET_VECTOR_ELT(rl, 1, optionParameters);
    SET_STRING_ELT(nm, 1, mkChar("parameters"));
    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, 
    //      ScalarString(mkChar("EuropeanOptionImpliedVolatility")));
    UNPROTECT(2);
    return(rl);
  }
#endif

  SEXP QL_BarrierOption(SEXP optionParameters) {

    const int nret = 8;		// dimension of return list

    char *barrType = CHAR(STRING_ELT( getListElement(optionParameters, 
						     "barrType"), 0));
    char *type = CHAR(STRING_ELT( getListElement(optionParameters, 
						 "type"), 0));
    double underlying = REAL(getListElement(optionParameters, 
					    "underlying"))[0];
    double strike = REAL(getListElement(optionParameters, "strike"))[0];
    Spread dividendYield = REAL(getListElement(optionParameters, 
					       "dividendYield"))[0];
    Rate riskFreeRate = REAL(getListElement(optionParameters, 
					    "riskFreeRate"))[0];
    Time maturity = REAL(getListElement(optionParameters, "maturity"))[0];
    int length = int(maturity * 360); // FIXME: this could be better
    double volatility = REAL(getListElement(optionParameters, 
					    "volatility"))[0];
    double barrier = REAL(getListElement(optionParameters, 
					 "barrier"))[0];
    double rebate = REAL(getListElement(optionParameters, 
					"rebate"))[0];
    Barrier::Type barrierType;
    if (!strcmp(barrType, "downin")) {
      barrierType = Barrier::DownIn;
    } else if (!strcmp(barrType, "upin")) {
      barrierType = Barrier::UpIn;
    } else if (!strcmp(barrType, "downout")) {
      barrierType = Barrier::DownOut;
    } else if (!strcmp(barrType, "upout")) {
      barrierType = Barrier::UpOut;
    } else {
      error("Unexpected barrier type %s, aborting\n", barrType);
    }
    Option::Type optionType;
    if (!strcmp(type, "call")) {
      optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
      optionType = Option::Put;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    // new QuantLib 0.3.5 framework, updated for 0.3.7
    Date today = Date::todaysDate();
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<TermStructure> qTS = makeFlatCurve(today, qRate, dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<TermStructure> rTS = makeFlatCurve(today, rRate, dc);

    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
      makeFlatVolatility(today, vol, dc);

    Date exDate = today.plusDays(length);
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));

    spot ->setValue(underlying);
    qRate->setValue(dividendYield);
    rRate->setValue(riskFreeRate);
    vol  ->setValue(volatility);

    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new PlainVanillaPayoff(optionType, strike));

    boost::shared_ptr<BlackScholesProcess> 
      stochProcess(new BlackScholesProcess(
                RelinkableHandle<Quote>(spot),
                RelinkableHandle<TermStructure>(qTS),
                RelinkableHandle<TermStructure>(rTS),
                RelinkableHandle<BlackVolTermStructure>(volTS)));

    Size timeSteps = 1;
    bool antitheticVariate = false;
    bool controlVariate = false;
    Size requiredSamples = 10000;
    double requiredTolerance = 0.02;
    Size maxSamples = 1000000;
    bool isBiased = false;

    boost::shared_ptr<PricingEngine> engine(new AnalyticBarrierEngine);

    BarrierOption barrierOption(barrierType,
				barrier,
				rebate,
				stochProcess,
				payoff,
				exercise,
				//mcEngine);
				engine);

    double calculated = barrierOption.NPV();
    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements

    insertListElement(rl, nm, 0, barrierOption.NPV(), "value");
    //insertListElement(rl, nm, 1, barrierOption.delta(), "delta");
    //insertListElement(rl, nm, 2, barrierOption.gamma(), "gamma");
    //insertListElement(rl, nm, 3, barrierOption.vega(), "vega");
    //insertListElement(rl, nm, 4, barrierOption.theta(), "theta");
    //insertListElement(rl, nm, 5, barrierOption.rho(), "rho");
    //insertListElement(rl, nm, 6, barrierOption.dividendRho(), "divRho");

    SET_VECTOR_ELT(rl, 7, optionParameters);
    SET_STRING_ELT(nm, 7, mkChar("parameters"));

    setAttrib(rl, R_NamesSymbol, nm);
    UNPROTECT(2);
    return(rl);
  }
}
