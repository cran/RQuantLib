// RQuantLib function DiscountCurve
//
// Copyright (C) 2005  Dominick Samperi
//
// $Id: discount.cpp,v 1.3 2006/01/10 23:46:52 dsamperi Exp $
//
// This program is part of the RQuantLib library for R (GNU S).
// It is made available under the terms of the GNU General Public
// License, version 2, or at your option, any later version.
//
// This program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more
// details.

#include "rquantlib.hpp"

RcppExport SEXP QL_DiscountCurve(SEXP params, SEXP tsQuotes,
				     SEXP times) {
    SEXP rl=0;
    char* exceptionMesg=NULL;

    try {

	// Parameter wrapper classes.
	RcppParams rparam(params);
	RcppNamedList tslist(tsQuotes);

	int i;

	Date todaysDate = rparam.getDateValue("tradeDate");
	Date settlementDate = rparam.getDateValue("settleDate");
	RQLContext::instance().settleDate = settlementDate;
	Settings::instance().evaluationDate() = todaysDate;

	string firstQuoteName = tslist.getName(0);

	double dt = rparam.getDoubleValue("dt");
	
	string interpWhat, interpHow;
	if(firstQuoteName.compare("flat") != 0) {

	    // Get interpolation method (not needed for "flat" case)
	    interpWhat = rparam.getStringValue("interpWhat");
	    interpHow  = rparam.getStringValue("interpHow");
	}

        Calendar calendar = TARGET();
	RQLContext::instance().calendar = calendar;
        Integer fixingDays = 2;
	RQLContext::instance().fixingDays = fixingDays;

	// Any DayCounter would be fine.
	// ActualActual::ISDA ensures that 30 years is 30.0
	DayCounter termStructureDayCounter =
	    ActualActual(ActualActual::ISDA);
	double tolerance = 1.0e-15;

	boost::shared_ptr<YieldTermStructure> curve;
	if(firstQuoteName.compare("flat") == 0) {
	    // Create a flat term structure.
	    double rateQuote = tslist.getValue(0);
	    boost::shared_ptr<Quote> flatRate(new SimpleQuote(rateQuote));
	    boost::shared_ptr<FlatForward> ts(new FlatForward(settlementDate,
					      Handle<Quote>(flatRate),
					      Actual365Fixed()));
	    curve = ts;
	}
	else {
	    // Build curve based on a set of observed rates and/or prices.
	    std::vector<boost::shared_ptr<RateHelper> > curveInput;
	    for(i = 0; i < tslist.getLength(); i++) {
		string name = tslist.getName(i);
		double val = tslist.getValue(i);
		boost::shared_ptr<RateHelper> rh = 
		    ObservableDB::instance().getRateHelper(name, val);
		if(rh == NULL_RateHelper)
		    throw std::range_error("Unknown rate in getRateHelper");
		curveInput.push_back(rh);
	    }
	    boost::shared_ptr<YieldTermStructure> ts =
		getTermStructure(interpWhat, interpHow, 
			      settlementDate, curveInput,
			      termStructureDayCounter, tolerance);
	    curve = ts;
	}

	// Return discount, forward rate, and zero coupon curves
	int ntimes = length(times);
	SEXP disc  = PROTECT(allocVector(REALSXP, ntimes));
	SEXP fwds  = PROTECT(allocVector(REALSXP, ntimes));
	SEXP zero  = PROTECT(allocVector(REALSXP, ntimes));
	double t;
	for(i = 0; i < ntimes; i++) {
	    t = REAL(times)[i];
	    REAL(disc)[i] = curve->discount(t);
	    REAL(fwds)[i] = curve->forwardRate(t, t+dt, Continuous);
	    REAL(zero)[i] = curve->zeroRate(t, Continuous);
	}

	RcppResultSet rs;
	rs.add("times", times, false);
	rs.add("discounts", disc, true);
	rs.add("forwards", fwds, true);
	rs.add("zerorates", zero, true);
	rs.add("params", params, false);
	rl = rs.getReturnList();

    } catch(std::exception& ex) {
	exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
	exceptionMesg = copyMessageToR("unknown reason");
    }

    if(exceptionMesg != NULL)
	error(exceptionMesg);
    
    return rl;
}

