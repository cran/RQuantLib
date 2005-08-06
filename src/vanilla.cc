// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002, 2003, 2004, 2005 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: vanilla.cc,v 1.15 2005/08/07 02:03:57 edd Exp $
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

// #include <ql/Instruments/vanillaoption.hpp>
// #include <ql/TermStructures/flatforward.hpp>
// #include <ql/Volatilities/blackconstantvol.hpp>
// #include <ql/Calendars/target.hpp>

// #include <ql/PricingEngines/Vanilla/baroneadesiwhaleyengine.hpp>

using namespace QuantLib;

extern "C" {

#include "rquantlib.h"

  SEXP QL_EuropeanOption(SEXP optionParameters) {

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
    int length = int(maturity*360 + 0.5); // FIXME: this could be better
    double volatility = REAL(getListElement(optionParameters, 
					    "volatility"))[0];

    Option::Type optionType;
    if (!strcmp(type, "call")) {
      optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
      optionType = Option::Put;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    Date today = Date::todaysDate();

    // new framework as per QuantLib 0.3.5
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
      makeFlatVolatility(today, vol, dc);
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> qTS = makeFlatCurve(today,qRate,dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> rTS = makeFlatCurve(today,rRate,dc);

    Date exDate = today + length;
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));

    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new PlainVanillaPayoff(optionType, strike));
    boost::shared_ptr<VanillaOption>
      option = makeOption(payoff, exercise, spot, qTS, rTS, volTS);

    spot->setValue(underlying);
    qRate->setValue(dividendYield);
    rRate->setValue(riskFreeRate);
    vol->setValue(volatility);

    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements

    insertListElement(rl, nm, 0, option->NPV(), "value");
    insertListElement(rl, nm, 1, option->delta(), "delta");
    insertListElement(rl, nm, 2, option->gamma(), "gamma");
    insertListElement(rl, nm, 3, option->vega(), "vega");
    insertListElement(rl, nm, 4, option->theta(), "theta");
    insertListElement(rl, nm, 5, option->rho(),   "rho");
    insertListElement(rl, nm, 6, option->dividendRho(), "divRho");

    SET_VECTOR_ELT(rl, 7, optionParameters);
    SET_STRING_ELT(nm, 7, mkChar("parameters"));

    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, ScalarString(mkChar("EuropeanOption")));

    UNPROTECT(2);
    return(rl);
  }

  SEXP QL_AmericanOption(SEXP optionParameters) {

    const int nret = 8;		// dimension of return list

    char *type = CHAR(STRING_ELT(getListElement(optionParameters,"type"), 0));
    double underlying = REAL(getListElement(optionParameters,"underlying"))[0];
    double strike = REAL(getListElement(optionParameters,"strike"))[0];	
    Spread dividendYield = REAL(getListElement(optionParameters, 
					       "dividendYield"))[0];
    Rate riskFreeRate = REAL(getListElement(optionParameters, 
					    "riskFreeRate"))[0];
    Time maturity = REAL(getListElement(optionParameters,"maturity"))[0];
    int length = int(maturity*360 + 0.5); // FIXME: this could be better
    double volatility = REAL(getListElement(optionParameters,"volatility"))[0];
    int timeSteps = INTEGER(getListElement(optionParameters,"timeSteps"))[0];
    int gridPoints = INTEGER(getListElement(optionParameters,"gridPoints"))[0];
    
    Option::Type optionType;
    if (!strcmp(type, "call")) {
      optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
      optionType = Option::Put;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    Date today = Date::todaysDate();

    // new framework as per QuantLib 0.3.5, updated for 0.3.7
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
      makeFlatVolatility(today, vol, dc);
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> qTS = makeFlatCurve(today,qRate,dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> rTS = makeFlatCurve(today,rRate,dc);
    Date exDate = today + length;
    boost::shared_ptr<Exercise> exercise(new AmericanExercise(today, exDate));
    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new PlainVanillaPayoff(optionType, strike));
//  boost::shared_ptr<VanillaOption> option = makeOption(payoff, exercise, 
// 					      spot, qTS, rTS, volTS,
// 					      JR); // engine
// 					      //TGEO); // engine
    spot->setValue(underlying);
    qRate->setValue(dividendYield);
    rRate->setValue(riskFreeRate);
    vol->setValue(volatility);

    // new from 0.3.7 BaroneAdesiWhaley
    boost::shared_ptr<PricingEngine> engine(
				    new BaroneAdesiWhaleyApproximationEngine);
    boost::shared_ptr<BlackScholesProcess> stochProcess(new
	BlackScholesProcess(
               	Handle<Quote>(spot),
                Handle<YieldTermStructure>(qTS),
                Handle<YieldTermStructure>(rTS),
                Handle<BlackVolTermStructure>(volTS)));

    VanillaOption option(stochProcess, payoff, exercise,
			 engine);

    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements

    insertListElement(rl, nm, 0, option.NPV(), "value");
//     insertListElement(rl, nm, 1, option->delta(), "delta");
//     insertListElement(rl, nm, 2, option->gamma(), "gamma");
//     insertListElement(rl, nm, 3, option->vega(), "vega");
//     insertListElement(rl, nm, 4, option->theta(), "theta");
//     insertListElement(rl, nm, 5, option->rho(),   "rho");
//     insertListElement(rl, nm, 6, option->dividendRho(), "divRho");

    SET_VECTOR_ELT(rl, 7, optionParameters);
    SET_STRING_ELT(nm, 7, mkChar("parameters"));

    setAttrib(rl, R_NamesSymbol, nm);
    //setAttrib(rl, R_ClassSymbol, ScalarString(mkChar("AmericanOption")));

    UNPROTECT(2);
    return(rl);
  }
 
}
