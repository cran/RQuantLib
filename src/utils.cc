
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002, 2003, 2004 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: utils.cc,v 1.3 2004/04/06 03:39:10 edd Exp $
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

#include <ql/quantlib.hpp>	// make QuantLib known

using namespace QuantLib;

extern "C" {

#include <R.h>
#include <Rinternals.h>
#include "rquantlib.h"

  // R interface utils, with thanks to Doug Bates

  // simple helper function to insert "labelled" element into list
  void insertListElement(SEXP &list, SEXP &names,
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
  SEXP getListElement(SEXP list, char *str) {
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < length(list); i++)
      if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
	elmt = VECTOR_ELT(list, i);
	break;
      }
    return elmt;
  }

  Handle<VanillaOption>
  makeOption(const Handle<StrikedTypePayoff>& payoff,
	     const Handle<Exercise>& exercise,
	     const Handle<Quote>& u,
	     const Handle<TermStructure>& q,
	     const Handle<TermStructure>& r,
	     const Handle<BlackVolTermStructure>& vol,
	     EngineType engineType) {

    Size binomialSteps = 251;
    Handle<PricingEngine> engine;
    switch (engineType) {
    case Analytic:
      engine = Handle<PricingEngine>(new AnalyticEuropeanEngine);
      break;
    case JR:
      engine = Handle<PricingEngine>(
	         new BinomialVanillaEngine<JarrowRudd>(binomialSteps));
      break;
    case CRR:
      engine = Handle<PricingEngine>(
                new BinomialVanillaEngine<CoxRossRubinstein>(binomialSteps));
    case EQP:
      engine = Handle<PricingEngine>(
                new BinomialVanillaEngine<AdditiveEQPBinomialTree>(
							   binomialSteps));
      break;
    case TGEO:
      engine = Handle<PricingEngine>(
                new BinomialVanillaEngine<Trigeorgis>(binomialSteps));
      break;
    case TIAN:
      engine = Handle<PricingEngine>(
                new BinomialVanillaEngine<Tian>(binomialSteps));
      break;
    case LR:
      engine = Handle<PricingEngine>(
                new BinomialVanillaEngine<LeisenReimer>(binomialSteps));
      break;
    case PseudoMonteCarlo:
      engine = MakeMCEuropeanEngine<PseudoRandom>().withStepsPerYear(1)
	.withTolerance(0.05)
	.withSeed(42);
      break;
    case QuasiMonteCarlo:
      engine = MakeMCEuropeanEngine<LowDiscrepancy>().withStepsPerYear(1)
	.withSamples(1023);
      break;
    default:
      QL_FAIL("Unknown engine type");
    }


    Handle<BlackScholesStochasticProcess> 
      stochProcess(new
         BlackScholesStochasticProcess(
	     RelinkableHandle<Quote>(u),
	     RelinkableHandle<TermStructure>(q),
	     RelinkableHandle<TermStructure>(r),
	     RelinkableHandle<BlackVolTermStructure>(vol)));

    return 
      Handle<VanillaOption>(new
	    VanillaOption(stochProcess, payoff, exercise, engine));
  }



  // QuantLib option setup utils, copied from the test-suite sources

  Handle<TermStructure> makeFlatCurve(const Handle<Quote>& forward,
				      DayCounter dc) {
    Date today = Date::todaysDate();
    return Handle<TermStructure>(
	 new FlatForward(today, today, 
			 RelinkableHandle<Quote>(forward), dc));
  }

  Handle<BlackVolTermStructure> makeFlatVolatility(const Handle<Quote>& vol,
                                                     DayCounter dc) {
    Date today = Date::todaysDate();
    return Handle<BlackVolTermStructure>(
         new BlackConstantVol(today, 
			      RelinkableHandle<Quote>(vol), dc));
  }


}
