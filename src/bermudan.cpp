// RQuantLib function BermudanSwaption
//
// Copyright 2005 Dominick Samperi
//
// $Id: bermudan.cpp,v 1.2 2006/01/10 22:18:24 dsamperi Exp $
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

// Calibrates underlying swaptions to the input volatility matrix.
void calibrateModel(const boost::shared_ptr<ShortRateModel>& model,
                    const std::vector<boost::shared_ptr<CalibrationHelper> >&
                                                                      helpers,
                    Real lambda,
		    int *swaptionMat, int *swapLengths, double **swaptionVols,
		    Size numRows, Size numCols) {

    Simplex om(lambda, 1e-9);
    om.setEndCriteria(EndCriteria(10000, 1e-7));
    model->calibrate(helpers, om);

    // Output the implied Black volatilities
    for (Size i=0; i<numRows; i++) {
	Real npv = helpers[i]->modelValue();
	Volatility implied = helpers[i]->impliedVolatility(npv, 1e-4,
                                                           1000, 0.05, 0.50);
	Volatility diff = implied - swaptionVols[i][numCols-i-1];

	Rprintf("%dx%d: model %lf, market %lf, diff %lf\n",
		swaptionMat[i], swapLengths[numCols-i-1], implied, 
		swaptionVols[i][numCols-i-1], diff);
    }	
}	

RcppExport SEXP QL_BermudanSwaption(SEXP params, SEXP tsQuotes, 
				       SEXP maturities, SEXP tenors, 
				       SEXP vols) {
    SEXP rl=0;
    char* exceptionMesg=NULL;

    try {

	// Parameter wrapper classes.
	RcppParams rparam(params);
	RcppNamedList tslist(tsQuotes);

	Size i;
	int *swaptionMat=0, *swapLengths=0;
	double **swaptionVols=0;

	double notional = 10000; // prices in basis points


	Date todaysDate = rparam.getDateValue("tradeDate");
	Date settlementDate = rparam.getDateValue("settleDate");
	RQLContext::instance().settleDate = settlementDate;
        Settings::instance().evaluationDate() = todaysDate;

	bool payFixedRate = rparam.getBoolValue("payFixed");

	double strike = rparam.getDoubleValue("strike");

	string method = rparam.getStringValue("method");

	string firstQuoteName = tslist.getName(0);

	string interpWhat, interpHow;
	if(firstQuoteName.compare("flat") != 0) {

	    // Get interpolation method (not needed for "flat" case)
	    interpWhat = rparam.getStringValue("interpWhat");
	    interpHow  = rparam.getStringValue("interpHow");
	}

        Calendar calendar = TARGET();
        Integer fixingDays = 2;
	RQLContext::instance().calendar = calendar;
	RQLContext::instance().fixingDays = fixingDays;

	// Any DayCounter would be fine.
	// ActualActual::ISDA ensures that 30 years is 30.0
	DayCounter termStructureDayCounter =
	    ActualActual(ActualActual::ISDA);
	double tolerance = 1.0e-15;

	boost::shared_ptr<YieldTermStructure> curve;
	if(firstQuoteName.compare("flat") == 0) {
	    // Get flat yield curve
	    double rateQuote = tslist.getValue(0);
	    boost::shared_ptr<Quote> flatRate(new SimpleQuote(rateQuote));
	    boost::shared_ptr<FlatForward> ts(new FlatForward(settlementDate,
					      Handle<Quote>(flatRate),
					      Actual365Fixed()));
	    curve = ts;
	}
	else {
	    // Get yield curve based on a set of market rates and/or prices.
	    std::vector<boost::shared_ptr<RateHelper> > curveInput;
	    for(i = 0; i < (Size)tslist.getLength(); i++) {
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
	Handle<YieldTermStructure> rhTermStructure;
	rhTermStructure.linkTo(curve);

	// Get swaption vol matrix.
	RcppMatrix<double> myVols(vols);
	int dim1 = myVols.getDim1();
	int dim2 = myVols.getDim2();
	swaptionVols = myVols.cMatrix();
	
	// Get swaption maturities
	RcppVector<int> myMats(maturities);
	int numRows = myMats.getLength();
	swaptionMat = myMats.cVector();

	// Get swap tenors
	RcppVector<int> myLengths(tenors);
	int numCols = myLengths.getLength();
	swapLengths = myLengths.cVector();

	if(numRows*numCols != dim1*dim2) {
	    ostringstream oss;
	    oss << "Swaption vol matrix size (" << dim1 << " x " << dim2 << ") "
		<< "incompatible\nwith size of swaption maturity vector ("
		<< numRows << ") and swap tenor vector ("
		<< numCols << ")";
	    throw std::range_error(oss.str());
	}

	// Create dummy swap to get schedules.
        Frequency fixedLegFrequency = Annual;
        BusinessDayConvention fixedLegConvention = Unadjusted;
        BusinessDayConvention floatingLegConvention = ModifiedFollowing;
        DayCounter fixedLegDayCounter = Thirty360(Thirty360::European);
        Frequency floatingLegFrequency = Semiannual;
        Rate dummyFixedRate = 0.03;
        boost::shared_ptr<Xibor> indexSixMonths(new
            Euribor(6, Months, rhTermStructure));

        Date startDate = calendar.advance(settlementDate,1,Years,
                                          floatingLegConvention);
        Date maturity = calendar.advance(startDate,5,Years,
                                         floatingLegConvention);
        Schedule fixedSchedule(calendar,startDate,maturity,
                               fixedLegFrequency,fixedLegConvention);
        Schedule floatSchedule(calendar,startDate,maturity,
                               floatingLegFrequency,floatingLegConvention);
        boost::shared_ptr<SimpleSwap> swap(new SimpleSwap(
            payFixedRate, notional,
            fixedSchedule, dummyFixedRate, fixedLegDayCounter,
            floatSchedule, indexSixMonths, fixingDays, 0.0,
            rhTermStructure));

	// Find the ATM or break-even rate
        Rate fixedATMRate = swap->fairRate();

	Rate fixedRate;
	if(strike < 0) // factor instead of real strike
	    fixedRate = fixedATMRate * (-strike);
	else
	    fixedRate = strike;

	// The swap underlying the Bermudan swaption.
        boost::shared_ptr<SimpleSwap> mySwap(new SimpleSwap(
            payFixedRate, notional,
            fixedSchedule, fixedRate, fixedLegDayCounter,
            floatSchedule, indexSixMonths, fixingDays, 0.0,
            rhTermStructure));

	// Build swaptions that will be used to calibrate model to
	// the volatility matrix.
        std::vector<Period> swaptionMaturities;
	for(i = 0; i < (Size)numRows; i++)
	    swaptionMaturities.push_back(Period(swaptionMat[i], Years));

	// Swaptions used for calibration
        std::vector<boost::shared_ptr<CalibrationHelper> > swaptions;

        // List of times that have to be included in the timegrid
        std::list<Time> times;
        for (i=0; i<(Size)numRows; i++) {
            boost::shared_ptr<Quote> vol(new SimpleQuote(swaptionVols[i][numCols-i-1]));
            swaptions.push_back(boost::shared_ptr<CalibrationHelper>(new
                SwaptionHelper(swaptionMaturities[i],
                               Period(swapLengths[numCols-i-1], Years),
                               Handle<Quote>(vol),
                               indexSixMonths,
                               indexSixMonths->frequency(),
                               indexSixMonths->dayCounter(),
                               rhTermStructure)));
            swaptions.back()->addTimesTo(times);
        }

        // Building time-grid
        TimeGrid grid(times.begin(), times.end(), 30);

	// Get Bermudan swaption exercise dates.
        std::vector<Date> bermudanDates;
        const std::vector<boost::shared_ptr<CashFlow> >& leg =
            swap->fixedLeg();
        for (i=0; i<leg.size(); i++) {
            boost::shared_ptr<Coupon> coupon =
                boost::dynamic_pointer_cast<Coupon>(leg[i]);
            bermudanDates.push_back(coupon->accrualStartDate());
        }
        boost::shared_ptr<Exercise> bermudaExercise(new
            BermudanExercise(bermudanDates));

	RcppResultSet rs;

	// Price based on method selected.
	if(method.compare("G2Analytic") == 0) {
	    boost::shared_ptr<G2> modelG2(new G2(rhTermStructure));
	    Rprintf("G2/Jamshidian (analytic) calibration\n");
	    for(i = 0; i < swaptions.size(); i++)
		swaptions[i]->setPricingEngine(boost::shared_ptr<PricingEngine>(
                new G2SwaptionEngine(modelG2, 6.0, 16)));
	    calibrateModel(modelG2, swaptions, 0.05, 
			   swaptionMat,swapLengths, swaptionVols, 
			   numRows, numCols);
	    Swaption bermudanSwaption(mySwap, bermudaExercise, 
				      rhTermStructure,
				      boost::shared_ptr<PricingEngine>());
	    bermudanSwaption.setPricingEngine
		(boost::shared_ptr<PricingEngine>(new TreeSwaptionEngine(modelG2, 50)));
	    rs.add("a", modelG2->params()[0]);
	    rs.add("sigma", modelG2->params()[1]);
	    rs.add("b", modelG2->params()[2]);
	    rs.add("eta", modelG2->params()[3]);
	    rs.add("rho", modelG2->params()[4]);
	    rs.add("price", bermudanSwaption.NPV());
	    rs.add("ATMStrike", fixedATMRate);
	    rs.add("params", params, false);
	    rl = rs.getReturnList();
	}
	else if(method.compare("HWAnalytic") == 0) {
	    boost::shared_ptr<HullWhite> modelHW(new HullWhite(rhTermStructure));
	    Rprintf("Hull-White (analytic) calibration\n");
	    for (i=0; i<swaptions.size(); i++)
		swaptions[i]->setPricingEngine(boost::shared_ptr<PricingEngine>(
                new JamshidianSwaptionEngine(modelHW)));
	    calibrateModel(modelHW, swaptions, 0.05, 
			   swaptionMat, swapLengths, swaptionVols, 
			   numRows, numCols);
	    Swaption bermudanSwaption(mySwap, bermudaExercise, 
				      rhTermStructure,
				      boost::shared_ptr<PricingEngine>());
	    bermudanSwaption.setPricingEngine
		(boost::shared_ptr<PricingEngine>(new TreeSwaptionEngine(modelHW, 50)));
	    rs.add("a", modelHW->params()[0]);
	    rs.add("sigma", modelHW->params()[1]);
	    rs.add("price", bermudanSwaption.NPV());
	    rs.add("ATMStrike", fixedATMRate);
	    rs.add("params", params, false);
            rl = rs.getReturnList();
	}
	else if(method.compare("HWTree") == 0) {
	    boost::shared_ptr<HullWhite> modelHW2(new HullWhite(rhTermStructure));
	    Rprintf("Hull-White (tree) calibration\n");
	    for (i=0; i<swaptions.size(); i++)
            swaptions[i]->setPricingEngine(boost::shared_ptr<PricingEngine>(
                new TreeSwaptionEngine(modelHW2,grid)));

	    calibrateModel(modelHW2, swaptions, 0.05, 
			   swaptionMat, swapLengths, swaptionVols, 
			   numRows, numCols);
	    Swaption bermudanSwaption(mySwap, bermudaExercise, 
				      rhTermStructure,
				      boost::shared_ptr<PricingEngine>());
	    bermudanSwaption.setPricingEngine
		(boost::shared_ptr<PricingEngine>(new TreeSwaptionEngine(modelHW2, 50)));
	    rs.add("a", modelHW2->params()[0]);
	    rs.add("sigma", modelHW2->params()[1]);
	    rs.add("price", bermudanSwaption.NPV());
	    rs.add("ATMStrike", fixedATMRate);
	    rs.add("params", params, false);
	    rl = rs.getReturnList();
	}
	else if(method.compare("BKTree") == 0) {
	    boost::shared_ptr<BlackKarasinski> modelBK(new
		    BlackKarasinski(rhTermStructure));
	    Rprintf("Black-Karasinski (tree) calibration\n");
	    for (i=0; i<swaptions.size(); i++)
            swaptions[i]->setPricingEngine(boost::shared_ptr<PricingEngine>(
                new TreeSwaptionEngine(modelBK,grid)));
	    calibrateModel(modelBK, swaptions, 0.05, 
			   swaptionMat, swapLengths, swaptionVols, 
			   numRows, numCols);
	    Swaption bermudanSwaption(mySwap, bermudaExercise, 
				      rhTermStructure,
				      boost::shared_ptr<PricingEngine>());
	    bermudanSwaption.setPricingEngine
		(boost::shared_ptr<PricingEngine>(new TreeSwaptionEngine(modelBK, 50)));
	    rs.add("a", modelBK->params()[0]);
	    rs.add("sigma", modelBK->params()[1]);
	    rs.add("price", bermudanSwaption.NPV());
	    rs.add("ATMStrike", fixedATMRate);
	    rs.add("params", params, false);
	    rl = rs.getReturnList();
	}
	else {
	    throw std::range_error("Unknown method in BermudanSwaption\n");
	}
    } catch(std::exception& ex) {
	exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
	exceptionMesg = copyMessageToR("unknown reason");
    }

    if(exceptionMesg != NULL)
	error(exceptionMesg);

    return rl;
}
