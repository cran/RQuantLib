/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

// RQuantLib function prototypes and macros
//
// Copyright 2002, 2003, 2004, 2005  Dirk Eddelbuettel <edd@debian.org>
// Copyright 2005  Dominick Samperi
//
// $Id: rquantlib.hpp,v 1.7 2005/10/27 04:21:40 dsamperi Exp $
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#ifndef rquantlib_hpp
#define rquantlib_hpp

#include <ql/quantlib.hpp>
#include <R.h>
#include <Rinternals.h>

#include "Rcpp.hpp"

#define NULL_RateHelper (boost::shared_ptr<RateHelper>)Null<boost::shared_ptr<RateHelper> >()

// Prototypes for convenience functions (some macros)
void insertListElement(SEXP &list, SEXP &names,
                       const int pos, const double value, 
                       const char *label);
SEXP getListElement(SEXP list, char *str);

// Used to maintain context while in an R function.
class RQLContext : public Singleton<RQLContext> {
public:
    RQLContext() { 
        fixingDays = 2;
        calendar = TARGET();
        settleDate = Date::todaysDate()+2;
    }
    // The tradeDate (evaluation date) is maintained by Settings,
    // and used to translate between dates and real-valued times.
    Date settleDate;
    Calendar calendar;
    Integer fixingDays;
};

// Instrument types used to construct the yield curve.
enum RQLObservableType { RQLDeposit, RQLSwap, RQLFuture, RQLFRA };

// Used to identify the specifics of a particular contract.
class RQLObservable {
public:
    RQLObservable(RQLObservableType type,
		  int n1, int n2,
		  TimeUnit units)
	: type_(type), n1_(n1), n2_(n2), units_(units) {}
    RQLObservableType getType() { return type_; }
    int getN1() { return n1_; }
    int getN2() { return n2_; }
    TimeUnit getUnits() { return units_; }
private:
    RQLObservableType type_;
    int n1_, n2_;    // n2 used for FRA's
    TimeUnit units_; // not used for futures and FRA's
};

typedef map<string, RQLObservable*> RQLMap;
typedef map<string, RQLObservable*>::const_iterator RQLMapIterator;

// Database used to maintain curve construction instrument details.
class ObservableDB : public Singleton<ObservableDB> {
public:
    ObservableDB();
    boost::shared_ptr<RateHelper> getRateHelper(string& ticker, Rate r);
private:
    RQLMap db_;
};

boost::shared_ptr<YieldTermStructure> getTermStructure
(string& interpWhat, string& interpHow, const Date& settleDate,
const std::vector<boost::shared_ptr<RateHelper> >& curveInput,
 DayCounter& dayCounter, Real tolerance);

boost::shared_ptr<YieldTermStructure>
makeFlatCurve(const Date& today,
	      const boost::shared_ptr<Quote>& forward,
	      const DayCounter& dc);

boost::shared_ptr<BlackVolTermStructure> 
makeFlatVolatility(const Date& today,
		   const boost::shared_ptr<Quote>& vol,
		   DayCounter dc);

enum EngineType {Analytic,
		 JR, CRR, EQP, TGEO, TIAN, LR,
		 FiniteDifferences,
		 PseudoMonteCarlo, QuasiMonteCarlo };

boost::shared_ptr<VanillaOption>
makeOption(const boost::shared_ptr<StrikedTypePayoff>& payoff,
	   const boost::shared_ptr<Exercise>& exercise,
	   const boost::shared_ptr<Quote>& u,
	   const boost::shared_ptr<YieldTermStructure>& q,
	   const boost::shared_ptr<YieldTermStructure>& r,
	   const boost::shared_ptr<BlackVolTermStructure>& vol,
	   EngineType engineType = Analytic,
	   Size binomialSteps=128,
	   Size samples=100); 

#endif
