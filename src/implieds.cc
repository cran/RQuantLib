// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002, 2003 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: implieds.cc,v 1.1 2003/11/29 01:09:51 edd Exp $
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

#include <ql/Instruments/vanillaoption.hpp>
#include <ql/PricingEngines/mceuropeanengine.hpp>
#include <ql/Pricers/fdamericanoption.hpp>
#include <ql/TermStructures/flatforward.hpp>
#include <ql/Volatilities/blackconstantvol.hpp>
#include <ql/Calendars/target.hpp>

using namespace QuantLib;
// using QuantLib::Pricers::EuropeanOption;
// using QuantLib::Pricers::FdAmericanOption;
// using QuantLib::Pricers::BinaryOption;
// using QuantLib::Pricers::BarrierOption;

using namespace QuantLib::Math;
using namespace QuantLib::MonteCarlo;
using namespace QuantLib::RandomNumbers;
using namespace QuantLib::Pricers;
using namespace QuantLib::PricingEngines;
using namespace QuantLib::Instruments;
using namespace QuantLib::TermStructures;
using namespace QuantLib::VolTermStructures;
using namespace QuantLib::DayCounters;
using namespace QuantLib::Calendars;

extern "C" {

#include "rquantlib.h"

  SEXP QL_EuropeanOptionImpliedVolatility(SEXP optionParameters) {
    const int nret = 2;		// dimension of return list
    char *type = CHAR(STRING_ELT(getListElement(optionParameters, "type"),0));
    Option::Type optionType;
    if (!strcmp(type, "call")) {
      optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
      optionType = Option::Put;
    } else if (!strcmp(type, "straddle")) {
      optionType = Option::Straddle;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    double strike = REAL(getListElement(optionParameters,"strike"))[0];	
    
    Time maturity = REAL(getListElement(optionParameters, "maturity"))[0];
    int length = int(maturity * 360); // FIXME: this could be better

    Date today = Date::todaysDate();
    Date reference = TARGET().advance(today,2,Days);

    // now create handles
    Handle<SimpleMarketElement> underlying(new SimpleMarketElement(0.0));
    Handle<SimpleMarketElement> volatility(new SimpleMarketElement(0.0));
    Handle<BlackVolTermStructure> 
      volCurve(new BlackConstantVol(reference,
				    RelinkableHandle<MarketElement>(volatility),
				    Actual365()));
    Handle<SimpleMarketElement> divYield(new SimpleMarketElement(0.0));
    Handle<TermStructure> 
      divCurve(new FlatForward(today, reference,
			       RelinkableHandle<MarketElement>(divYield),
			       Actual365()));
    Handle<SimpleMarketElement> rfRate(new SimpleMarketElement(0.0));
    Handle<TermStructure> 
      rfCurve(new FlatForward(today, reference,
			      RelinkableHandle<MarketElement>(rfRate),
			      Actual365()));
    Handle<PricingEngine> engine(new AnalyticEuropeanEngine);
    
    Date exDate = today.plusDays(length);
    Handle<VanillaOption> 
      option(new VanillaOption(optionType, 
   			       RelinkableHandle<MarketElement>(underlying), 
			       strike,
			       RelinkableHandle<TermStructure>(divCurve), 
			       RelinkableHandle<TermStructure>(rfCurve), 
			       EuropeanExercise(exDate), 
			       RelinkableHandle<BlackVolTermStructure>(volCurve),
			       engine));
    underlying->setValue( REAL(getListElement(optionParameters,
 					      "underlying"))[0] );
    divYield->setValue( REAL(getListElement(optionParameters, 
					       "dividendYield"))[0] );
    rfRate->setValue( REAL(getListElement(optionParameters, 
					 "riskFreeRate"))[0] );
    volatility->setValue( REAL(getListElement(optionParameters,
					      "volatility"))[0] );
    
    double implVol =		// compute implied vol. given parameters
      option->impliedVolatility( REAL(getListElement(optionParameters, 
						     "value"))[0] );
    
    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
    insertListElement(rl, nm, 0, implVol, "impliedVol");
    SET_VECTOR_ELT(rl, 1, optionParameters);
    SET_STRING_ELT(nm, 1, mkChar("parameters"));
    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, 
    //      ScalarString(mkChar("EuropeanOptionImpliedVolatility")));
    UNPROTECT(2);
    return(rl);
  }

  SEXP QL_AmericanOptionImpliedVolatility(SEXP optionParameters) {
    const int nret = 2;		// dimension of return list
    char *type = CHAR(STRING_ELT(getListElement(optionParameters, "type"),0));
    Option::Type optionType;
    if (!strcmp(type, "call")) {
      optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
      optionType = Option::Put;
    } else if (!strcmp(type, "straddle")) {
      optionType = Option::Straddle;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    double strike = REAL(getListElement(optionParameters,"strike"))[0];	
    
    Time maturity = REAL(getListElement(optionParameters, "maturity"))[0];
    int length = int(maturity * 360); // FIXME: this could be better

    Date today = Date::todaysDate();
    Date reference = TARGET().advance(today,2,Days);

    // now create handles
    Handle<SimpleMarketElement> underlying(new SimpleMarketElement(0.0));
    Handle<SimpleMarketElement> volatility(new SimpleMarketElement(0.0));
    Handle<BlackVolTermStructure> 
      volCurve(new BlackConstantVol(reference,
				    RelinkableHandle<MarketElement>(volatility),
				    Actual365()));
    Handle<SimpleMarketElement> divYield(new SimpleMarketElement(0.0));
    Handle<TermStructure> 
      divCurve(new FlatForward(today, reference,
			       RelinkableHandle<MarketElement>(divYield),
			       Actual365()));
    Handle<SimpleMarketElement> rfRate(new SimpleMarketElement(0.0));
    Handle<TermStructure> 
      rfCurve(new FlatForward(today, reference,
			      RelinkableHandle<MarketElement>(rfRate),
			      Actual365()));
    int timeSteps = 800;
    Handle<PricingEngine> 
      engine(new BinomialVanillaEngine(BinomialVanillaEngine::EQP, timeSteps));
    
    Date exDate = today.plusDays(length);
    Handle<VanillaOption> 
      option(new VanillaOption(optionType, 
			       RelinkableHandle<MarketElement>(underlying), 
			       strike,
			       RelinkableHandle<TermStructure>(divCurve), 
			       RelinkableHandle<TermStructure>(rfCurve), 
			       AmericanExercise(today, exDate),
			       RelinkableHandle<BlackVolTermStructure>(volCurve),
			       engine));
    underlying->setValue( REAL(getListElement(optionParameters,
					      "underlying"))[0] );
    divYield->setValue( REAL(getListElement(optionParameters, 
					       "dividendYield"))[0] );
    rfRate->setValue( REAL(getListElement(optionParameters, 
					 "riskFreeRate"))[0] );
    volatility->setValue( REAL(getListElement(optionParameters,
					      "volatility"))[0] );
    
    //    double computedValue = option->NPV(); // do we need this?
    double implVol = 
      option->impliedVolatility( REAL(getListElement(optionParameters, 
						     "value"))[0] );
    
    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
    insertListElement(rl, nm, 0, implVol, "impliedVol");
    SET_VECTOR_ELT(rl, 1, optionParameters);
    SET_STRING_ELT(nm, 1, mkChar("parameters"));
    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, 
    //      ScalarString(mkChar("EuropeanOptionImpliedVolatility")));
    UNPROTECT(2);
    return(rl);
  }
 
}
