// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: RQuantLib.cc,v 1.3 2002/02/26 03:53:06 edd Exp $
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

extern "C" {

#include <R.h>
#include <Rinternals.h>

  // simple helper function to insert "labelled" element into list
  static inline void insertListElement(SEXP &list, SEXP &names,
				       const int pos, const double value, 
				       const char *label) {
    SEXP vec = PROTECT(allocVector(REALSXP, 1));
    REAL(vec)[0] = value; 
    SET_VECTOR_ELT(list, pos, vec);
    SET_STRING_ELT(names, pos, mkChar(label));
    UNPROTECT(1);
  }

  // get the list element named str, or return NULL 
  // courtesy of the R Exts manual, and the nls package
  static inline SEXP getListElement(SEXP list, char *str) {
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < length(list); i++)
      if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
	elmt = VECTOR_ELT(list, i);
	break;
      }
    return elmt;
  }

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
    double volatility = REAL(getListElement(optionParameters, 
					    "volatility"))[0];


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

    EuropeanOption EO = EuropeanOption(optionType, underlying, strike,
				       dividendYield, riskFreeRate, maturity, 
				       volatility);


    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements

    insertListElement(rl, nm, 0, EO.value(), "value");
    insertListElement(rl, nm, 1, EO.delta(), "delta");
    insertListElement(rl, nm, 2, EO.gamma(), "gamma");
    insertListElement(rl, nm, 3, EO.vega(), "vega");
    insertListElement(rl, nm, 4, EO.theta(), "theta");
    insertListElement(rl, nm, 5, EO.rho(),   "rho");
    insertListElement(rl, nm, 6, EO.dividendRho(), "divRho");

    SET_VECTOR_ELT(rl, 7, optionParameters);
    SET_STRING_ELT(nm, 7, mkChar("parameters"));

    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, ScalarString(mkChar("EuropeanOption")));

    UNPROTECT(2);
    return(rl);
  }

  SEXP QL_EuropeanOptionImpliedVolatility(SEXP optionParameters) {
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

    EuropeanOption EO = EuropeanOption(optionType, underlying, strike,
				       dividendYield, riskFreeRate, maturity, 
				       volatility);
    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
    insertListElement(rl, nm, 0, EO.impliedVolatility(value), "impliedVol");
    SET_VECTOR_ELT(rl, 1, optionParameters);
    SET_STRING_ELT(nm, 1, mkChar("parameters"));
    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, 
    //      ScalarString(mkChar("EuropeanOptionImpliedVolatility")));
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
    double volatility = REAL(getListElement(optionParameters,"volatility"))[0];
    int timeSteps = INTEGER(getListElement(optionParameters,"timeSteps"))[0];
    int gridPoints = INTEGER(getListElement(optionParameters,"gridPoints"))[0];
    
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

    FdAmericanOption AO = FdAmericanOption(optionType, underlying, strike,
					   dividendYield, riskFreeRate, 
					   maturity, volatility, 
					   timeSteps, gridPoints);

    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements

    insertListElement(rl, nm, 0, AO.value(), "value");
    insertListElement(rl, nm, 1, AO.delta(), "delta");
    insertListElement(rl, nm, 2, AO.gamma(), "gamma");
    insertListElement(rl, nm, 3, AO.vega(), "vega");
    insertListElement(rl, nm, 4, AO.theta(), "theta");
    insertListElement(rl, nm, 5, AO.rho(),   "rho");
    insertListElement(rl, nm, 6, AO.dividendRho(), "divRho");

    SET_VECTOR_ELT(rl, 7, optionParameters);
    SET_STRING_ELT(nm, 7, mkChar("parameters"));

    setAttrib(rl, R_NamesSymbol, nm);
    //setAttrib(rl, R_ClassSymbol, ScalarString(mkChar("AmericanOption")));

    UNPROTECT(2);
    return(rl);
  }

  SEXP QL_AmericanOptionImpliedVolatility(SEXP optionParameters) {
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
    int timeSteps = INTEGER(getListElement(optionParameters,"timeSteps"))[0];
    int gridPoints = INTEGER(getListElement(optionParameters,"gridPoints"))[0];

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

    FdAmericanOption AO = FdAmericanOption(optionType, underlying, strike,
					   dividendYield, riskFreeRate, 
					   maturity, volatility,
					   timeSteps, gridPoints);
    SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
    SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
    insertListElement(rl, nm, 0, AO.impliedVolatility(value), "impliedVol");
    SET_VECTOR_ELT(rl, 1, optionParameters);
    SET_STRING_ELT(nm, 1, mkChar("parameters"));
    setAttrib(rl, R_NamesSymbol, nm);
    //    setAttrib(rl, R_ClassSymbol, 
    //      ScalarString(mkChar("EuropeanOptionImpliedVolatility")));
    UNPROTECT(2);
    return(rl);
  }


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

//   SEXP QL_BinaryOptionImpliedVolatility(SEXP optionParameters) {
//     const int nret = 2;		// dimension of return list
//     char *type = CHAR(STRING_ELT(getListElement(optionParameters, "type"),0));
//     double value = REAL(getListElement(optionParameters, "value"))[0];
//     double underlying = REAL(getListElement(optionParameters,"underlying"))[0];
//     double strike = REAL(getListElement(optionParameters,"strike"))[0];	
//     Spread dividendYield = REAL(getListElement(optionParameters, 
// 					       "dividendYield"))[0];
//     Rate riskFreeRate = REAL(getListElement(optionParameters, 
// 					    "riskFreeRate"))[0];
//     Time maturity = REAL(getListElement(optionParameters, "maturity"))[0];
//     double volatility = REAL(getListElement(optionParameters,"volatility"))[0];
//     double cashPayoff = REAL(getListElement(optionParameters,"cashPayoff"))[0];

//     Option::Type optionType;
//     if (!strcmp(type, "call")) {
//       optionType = Option::Call;
//     } else if (!strcmp(type, "put")) {
//       optionType = Option::Put;
//     } else if (!strcmp(type, "straddle")) {
//       optionType = Option::Straddle;
//     } else {
//       error("Unexpected option type %s, aborting\n", type);
//     }

//     BinaryOption BO = BinaryOption(optionType, underlying, strike,
// 					   dividendYield, riskFreeRate, 
// 					   maturity, volatility,
// 				   	   cashPayoff);
//     SEXP rl = PROTECT(allocVector(VECSXP, nret)); // returned list
//     SEXP nm = PROTECT(allocVector(STRSXP, nret)); // names of list elements
//     insertListElement(rl, nm, 0, BO.impliedVolatility(value), "impliedVol");
//     SET_VECTOR_ELT(rl, 1, optionParameters);
//     SET_STRING_ELT(nm, 1, mkChar("parameters"));
//     setAttrib(rl, R_NamesSymbol, nm);
//     //    setAttrib(rl, R_ClassSymbol, 
//     //      ScalarString(mkChar("EuropeanOptionImpliedVolatility")));
//     UNPROTECT(2);
//     return(rl);
//   }

}
