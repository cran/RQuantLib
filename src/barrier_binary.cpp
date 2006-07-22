// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002-2006 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: barrier_binary.cpp,v 1.13 2006/07/22 14:16:36 dsamperi Exp $
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

#include "rquantlib.hpp"

RcppExport  SEXP QL_BinaryOption(SEXP optionParameters) {

  SEXP rl=R_NilValue;
  char* exceptionMesg=NULL;
  
  try {

    RcppParams rparam(optionParameters);    	// Parameter wrapper class

    string type = rparam.getStringValue("type");
    double underlying = rparam.getDoubleValue("underlying");
    double strike = rparam.getDoubleValue("strike");
    Spread dividendYield = rparam.getDoubleValue("dividendYield");
    Rate riskFreeRate = rparam.getDoubleValue("riskFreeRate");
    Time maturity = rparam.getDoubleValue("maturity");
    int length = int(maturity*360 + 0.5); // FIXME: this could be better
    double volatility = rparam.getDoubleValue("volatility");
    double cashPayoff = rparam.getDoubleValue("cashPayoff");
    
    Option::Type optionType=Option::Call;
    if (type=="call") {
      optionType = Option::Call;
    } else if (type=="put") {
      optionType = Option::Put;
    } else {
      throw std::range_error("Unknown option " + type);
    }

    // new QuantLib 0.3.5 framework: digitals, updated for 0.3.7
    Date today = Date::todaysDate();
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> qTS = makeFlatCurve(today,qRate,dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> rTS = makeFlatCurve(today,rRate,dc);
    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
      makeFlatVolatility(today, vol, dc);
    boost::shared_ptr<PricingEngine> engine(new AnalyticEuropeanEngine);

    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new CashOrNothingPayoff(optionType, strike, cashPayoff));

    Date exDate = today + length;

    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));

    spot->setValue(underlying);
    qRate->setValue(dividendYield);
    rRate->setValue(riskFreeRate);
    vol  ->setValue(volatility);

    boost::shared_ptr<BlackScholesProcess> 
      stochProcess(new BlackScholesProcess(
       		        Handle<Quote>(spot),
                	Handle<YieldTermStructure>(qTS),
                	Handle<YieldTermStructure>(rTS),
                	Handle<BlackVolTermStructure>(volTS)));

    VanillaOption opt(stochProcess, payoff, exercise, engine);

    RcppResultSet rs;
    rs.add("value", opt.NPV());
    rs.add("delta", opt.delta());
    rs.add("gamma", opt.gamma());
    rs.add("vega", opt.vega());
    rs.add("theta", opt.theta());
    rs.add("rho", opt.rho());
    rs.add("divRho", opt.dividendRho());
    rs.add("parameters", optionParameters, false);
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

// dumped core when we tried last
// no longer under 0.3.10 and g++ 4.0.1 (Aug 2005)
RcppExport  SEXP QL_BinaryOptionImpliedVolatility(SEXP optionParameters) {

  SEXP rl=R_NilValue;
  char* exceptionMesg=NULL;
  
  try {

    RcppParams rparam(optionParameters);    	// Parameter wrapper class

    string type = rparam.getStringValue("type");
    double value = rparam.getDoubleValue("value");
    double underlying = rparam.getDoubleValue("underlying");
    double strike = rparam.getDoubleValue("strike");
    Spread dividendYield = rparam.getDoubleValue("dividendYield");
    Rate riskFreeRate = rparam.getDoubleValue("riskFreeRate");
    Time maturity = rparam.getDoubleValue("maturity");
    int length = int(maturity*360 + 0.5); // FIXME: this could be better
    double volatility = rparam.getDoubleValue("volatility");
    double cashPayoff = rparam.getDoubleValue("cashPayoff");

    Option::Type optionType=Option::Call;
    if (type=="call") {
      optionType = Option::Call;
    } else if (type=="put") {
      optionType = Option::Put;
    } else {
      throw std::range_error("Unknown option " + type);
    }

    // new QuantLib 0.3.5 framework: digitals, updated for 0.3.7
    Date today = Date::todaysDate();
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> qTS = makeFlatCurve(today,qRate,dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> rTS = makeFlatCurve(today,rRate,dc);
    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
	makeFlatVolatility(today, vol, dc);
    boost::shared_ptr<PricingEngine> engine(new AnalyticEuropeanEngine);

    boost::shared_ptr<StrikedTypePayoff> 
	payoff(new CashOrNothingPayoff(optionType, strike, cashPayoff));
    Date exDate = today + length;

    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));

    spot->setValue(underlying);
    qRate->setValue(dividendYield);
    rRate->setValue(riskFreeRate);
    vol  ->setValue(volatility);

    boost::shared_ptr<BlackScholesProcess> 
      stochProcess(new BlackScholesProcess(Handle<Quote>(spot),
				Handle<YieldTermStructure>(qTS),
				Handle<YieldTermStructure>(rTS),
				Handle<BlackVolTermStructure>(volTS)));

    VanillaOption opt(stochProcess, payoff, exercise, engine);

    RcppResultSet rs;
    rs.add("impliedVol", opt.impliedVolatility(value));
    rs.add("parameters", optionParameters, false);
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

RcppExport  SEXP QL_BarrierOption(SEXP optionParameters) {

  SEXP rl=R_NilValue;
  char* exceptionMesg=NULL;
  
  try {

    RcppParams rparam(optionParameters);    	// Parameter wrapper class

    string barrType = rparam.getStringValue("barrType");
    string type = rparam.getStringValue("type");
    double underlying = rparam.getDoubleValue("underlying");
    double strike = rparam.getDoubleValue("strike");
    Spread dividendYield = rparam.getDoubleValue("dividendYield");
    Rate riskFreeRate = rparam.getDoubleValue("riskFreeRate");
    Time maturity = rparam.getDoubleValue("maturity");
    int length = int(maturity*360 + 0.5); // FIXME: this could be better
    double volatility = rparam.getDoubleValue("volatility");
    double barrier = rparam.getDoubleValue("barrier");
    double rebate = rparam.getDoubleValue("rebate");

    Barrier::Type barrierType=Barrier::DownIn;
    if (barrType=="downin") {
      barrierType = Barrier::DownIn;
    } else if (barrType=="upin") {
      barrierType = Barrier::UpIn;
    } else if (barrType=="downout") {
      barrierType = Barrier::DownOut;
    } else if (barrType=="upout") {
      barrierType = Barrier::UpOut;
    } else {
      throw std::range_error("Unknown barrier type " + type);
    }

    Option::Type optionType=Option::Call;
    if (type=="call") {
      optionType = Option::Call;
    } else if (type=="put") {
      optionType = Option::Put;
    } else {
      throw std::range_error("Unknown option " + type);
    }

    // new QuantLib 0.3.5 framework, updated for 0.3.7
    Date today = Date::todaysDate();
    DayCounter dc = Actual360();
    boost::shared_ptr<SimpleQuote> spot(new SimpleQuote(0.0));
    boost::shared_ptr<SimpleQuote> qRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> qTS = makeFlatCurve(today,qRate,dc);
    boost::shared_ptr<SimpleQuote> rRate(new SimpleQuote(0.0));
    boost::shared_ptr<YieldTermStructure> rTS = makeFlatCurve(today,rRate,dc);

    boost::shared_ptr<SimpleQuote> vol(new SimpleQuote(0.0));
    boost::shared_ptr<BlackVolTermStructure> volTS = 
      makeFlatVolatility(today, vol, dc);

    Date exDate = today + length;
    boost::shared_ptr<Exercise> exercise(new EuropeanExercise(exDate));

    spot ->setValue(underlying);
    qRate->setValue(dividendYield);
    rRate->setValue(riskFreeRate);
    vol  ->setValue(volatility);

    boost::shared_ptr<StrikedTypePayoff> 
      payoff(new PlainVanillaPayoff(optionType, strike));

    boost::shared_ptr<BlackScholesProcess> 
      stochProcess(new BlackScholesProcess(
                	Handle<Quote>(spot),
                	Handle<YieldTermStructure>(qTS),
                	Handle<YieldTermStructure>(rTS),
                	Handle<BlackVolTermStructure>(volTS)));

    // Size timeSteps = 1;
    // bool antitheticVariate = false;
    // bool controlVariate = false;
    // Size requiredSamples = 10000;
    // double requiredTolerance = 0.02;
    // Size maxSamples = 1000000;
    // bool isBiased = false;

    boost::shared_ptr<PricingEngine> engine(new AnalyticBarrierEngine);

    BarrierOption barrierOption(barrierType,
				barrier,
				rebate,
				stochProcess,
				payoff,
				exercise,
				//mcEngine);
				engine);

    RcppResultSet rs;
    rs.add("value", barrierOption.NPV());
    rs.add("delta", "NA");
    rs.add("gamma", "NA");
    rs.add("vega", "NA");
    rs.add("theta", "NA");
    rs.add("rho", "NA");
    rs.add("divRho", "NA");
    rs.add("parameters", optionParameters, false);
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

