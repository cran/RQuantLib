
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002-2005 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: implieds.cpp,v 1.9 2005/10/12 03:56:07 edd Exp $
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

// NB can be build standalone as   PKG_LIBS=-lQuantLib R CMD SHLIB implieds.cc

#include "rquantlib.hpp"

RcppExport  SEXP QL_EuropeanOptionImpliedVolatility(SEXP optionParameters) {
    const Size maxEvaluations = 100;
    const double tolerance = 1.0e-6;
    const int nret = 2;		// dimension of return list

    char *type = CHAR(STRING_ELT(getListElement(optionParameters, "type"),0));
    Option::Type optionType=Option::Call;
    if (!strcmp(type, "call")) {
      optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
      optionType = Option::Put;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    double underlying = REAL(getListElement(optionParameters, "underlying"))[0];
    double strike = REAL(getListElement(optionParameters,"strike"))[0];	
    double dividendYield = 
      REAL(getListElement(optionParameters, "dividendYield"))[0];
    double riskFreeRate = 
      REAL(getListElement(optionParameters, "riskFreeRate"))[0];
    double volatility = REAL(getListElement(optionParameters, "volatility"))[0];
    Time maturity = REAL(getListElement(optionParameters, "maturity"))[0];
    int length = int(maturity*360 + 0.5); // FIXME: this could be better

    Date today = Date::todaysDate();

    // new framework as per QuantLib 0.3.5
    // updated for 0.3.7
    DayCounter dc = Actual360();

    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    spot->setValue(underlying);
    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
      makeFlatVolatility(today, vol, dc);
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    qRate->setValue(dividendYield);
    boost::shared_ptr<YieldTermStructure> qTS = makeFlatCurve(today, qRate, dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    rRate->setValue(riskFreeRate);
    boost::shared_ptr<YieldTermStructure> rTS = makeFlatCurve(today, rRate, dc);
    Date exDate = today + length;
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));
    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new PlainVanillaPayoff(optionType, strike));
    double implVol = 0.0; // just to remove a warning...
    boost::shared_ptr<VanillaOption> option = 
      makeOption(payoff, exercise, spot, qTS, rTS, volTS, 
		 Analytic, Null<Size>(), Null<Size>());

    double volguess = volatility;
    vol->setValue(volguess);

    //double value = option->NPV();
    double value = REAL(getListElement(optionParameters,"value"))[0];
    //    if (value != 0.0) {
    //  vol->setValue(volguess*1.5);	// shift guess somehow
      implVol = option->impliedVolatility(value, tolerance, maxEvaluations);
    //}

    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
    insertListElement(rl, nm, 0, implVol, "impliedVol");
    SET_VECTOR_ELT(rl, 1, optionParameters);
    SET_STRING_ELT(nm, 1, mkChar("parameters"));
    setAttrib(rl, R_NamesSymbol, nm);
    UNPROTECT(2);
    return(rl);
}

RcppExport  SEXP QL_AmericanOptionImpliedVolatility(SEXP optionParameters) {
    const Size maxEvaluations = 100;
    const double tolerance = 1.0e-6;
    const int nret = 2;		// dimension of return list

    char *type = CHAR(STRING_ELT(getListElement(optionParameters, "type"),0));
    Option::Type optionType=Option::Call;
    if (!strcmp(type, "call")) {
	optionType = Option::Call;
    } else if (!strcmp(type, "put")) {
	optionType = Option::Put;
    } else {
	error("Unexpected option type %s, aborting\n", type);
    }

    double strike = REAL(getListElement(optionParameters,"strike"))[0];	
    
    Time maturity = REAL(getListElement(optionParameters, "maturity"))[0];
    int length = int(maturity*360 + 0.5); // FIXME: this could be better

    Date today = Date::todaysDate();

    // new framework as per QuantLib 0.3.5
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
	makeFlatVolatility(today, vol,dc);
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> qTS = makeFlatCurve(today,qRate,dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> rTS = makeFlatCurve(today,rRate,dc);

    Date exDate = today + length;
    //boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));
    boost::shared_ptr<Exercise> exercise(new AmericanExercise(today, exDate));
    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new PlainVanillaPayoff(optionType, strike));
    boost::shared_ptr<VanillaOption> option = makeOption(payoff, exercise, spot,
					      qTS, rTS, volTS,
					      JR);

    spot->setValue(REAL(getListElement(optionParameters, "underlying"))[0]);
    qRate->setValue(REAL(getListElement(optionParameters, 
					"dividendYield"))[0]);
    rRate->setValue(REAL(getListElement(optionParameters,
					"riskFreeRate"))[0]);
    double volguess = REAL(getListElement(optionParameters, "volatility"))[0];
    vol->setValue(volguess);

    //double value = option->NPV();
    double value = REAL(getListElement(optionParameters,"value"))[0];
    double implVol = 0.0; // just to remove a warning...
    //    if (value != 0.0) {
    //  vol->setValue(volguess*1.5);	// shift guess somehow
      implVol = option->impliedVolatility(value, tolerance, maxEvaluations);
      //}

    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
    insertListElement(rl, nm, 0, implVol, "impliedVol");
    SET_VECTOR_ELT(rl, 1, optionParameters);
    SET_STRING_ELT(nm, 1, mkChar("parameters"));
    setAttrib(rl, R_NamesSymbol, nm);
    UNPROTECT(2);
    return(rl);
}
 
