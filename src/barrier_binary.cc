// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: barrier_binary.cc,v 1.1 2003/11/29 01:10:15 edd Exp $
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
using QuantLib::Pricers::EuropeanOption;
using QuantLib::Pricers::FdAmericanOption;
using QuantLib::Pricers::BinaryOption;
using QuantLib::Pricers::BarrierOption;

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
    double volatility = REAL(getListElement(optionParameters, 
					    "volatility"))[0];
    double cashPayoff = REAL(getListElement(optionParameters, 
					    "cashPayoff"))[0];

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

    BinaryOption BO = BinaryOption(optionType, underlying, strike,
				   dividendYield, riskFreeRate, maturity, 
				   volatility, cashPayoff);

    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements

    insertListElement(rl, nm, 0, BO.value(), "value");
    insertListElement(rl, nm, 1, BO.delta(), "delta");
    insertListElement(rl, nm, 2, BO.gamma(), "gamma");
    insertListElement(rl, nm, 3, BO.vega(), "vega");
    insertListElement(rl, nm, 4, BO.theta(), "theta");
    insertListElement(rl, nm, 5, BO.rho(),   "rho");
    insertListElement(rl, nm, 6, BO.dividendRho(), "divRho");

    SET_VECTOR_ELT(rl, 7, optionParameters);
    SET_STRING_ELT(nm, 7, mkChar("parameters"));

    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, ScalarString(mkChar("EuropeanOption")));

    UNPROTECT(2);
    return(rl);
  }

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
    double volatility = REAL(getListElement(optionParameters,"volatility"))[0];
    double cashPayoff = REAL(getListElement(optionParameters,"cashPayoff"))[0];

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

    BinaryOption BO = BinaryOption(optionType, underlying, strike,
					   dividendYield, riskFreeRate, 
					   maturity, volatility,
				   	   cashPayoff);
    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
    insertListElement(rl, nm, 0, BO.impliedVolatility(value), "impliedVol");
    SET_VECTOR_ELT(rl, 1, optionParameters);
    SET_STRING_ELT(nm, 1, mkChar("parameters"));
    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, 
    //      ScalarString(mkChar("EuropeanOptionImpliedVolatility")));
    UNPROTECT(2);
    return(rl);
  }

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
    } else if (!strcmp(type, "straddle")) {
      optionType = Option::Straddle;
    } else {
      error("Unexpected option type %s, aborting\n", type);
    }

    BarrierOption BO = BarrierOption(barrierType, optionType,
				     underlying, strike,
				     dividendYield, riskFreeRate,
				     maturity, volatility, 
				     barrier, rebate);

    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements

    insertListElement(rl, nm, 0, BO.value(), "value");
    insertListElement(rl, nm, 1, BO.delta(), "delta");
    insertListElement(rl, nm, 2, BO.gamma(), "gamma");
    insertListElement(rl, nm, 3, BO.vega(), "vega");
    insertListElement(rl, nm, 4, BO.theta(), "theta");
    insertListElement(rl, nm, 5, BO.rho(),   "rho");
    insertListElement(rl, nm, 6, BO.dividendRho(), "divRho");

    SET_VECTOR_ELT(rl, 7, optionParameters);
    SET_STRING_ELT(nm, 7, mkChar("parameters"));

    setAttrib(rl, R_NamesSymbol, nm);
    UNPROTECT(2);
    return(rl);
  }

}
