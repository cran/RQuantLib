
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002-2006 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: vanilla.cpp,v 1.20 2006/07/22 14:18:18 dsamperi Exp $
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

RcppExport  SEXP QL_EuropeanOption(SEXP optionParameters) {

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
    
    Option::Type optionType=Option::Call;
    if (type=="call") {
      optionType = Option::Call;
    } else if (type=="put") {
      optionType = Option::Put;
    } else {
      throw std::range_error("Unknown option " + type);
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

    RcppResultSet rs;
    rs.add("value", option->NPV());
    rs.add("delta", option->delta());
    rs.add("gamma", option->gamma());
    rs.add("vega", option->vega());
    rs.add("theta", option->theta());
    rs.add("rho", option->rho());
    rs.add("divRho", option->dividendRho());
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

RcppExport  SEXP QL_AmericanOption(SEXP optionParameters) {

  SEXP rl=R_NilValue;
  char* exceptionMesg=NULL;
  
  try {

    // Parameter wrapper classes.
    RcppParams rparam(optionParameters);

    string type = rparam.getStringValue("type");
    double underlying = rparam.getDoubleValue("underlying");
    double strike = rparam.getDoubleValue("strike");
    Spread dividendYield = rparam.getDoubleValue("dividendYield");
    Rate riskFreeRate = rparam.getDoubleValue("riskFreeRate");
    Time maturity = rparam.getDoubleValue("maturity");
    int length = int(maturity*360 + 0.5); // FIXME: this could be better
    double volatility = rparam.getDoubleValue("volatility");

    Option::Type optionType=Option::Call;
    if (type=="call") {
      optionType = Option::Call;
    } else if (type=="put") {
      optionType = Option::Put;
    } else {
      throw std::range_error("Unknown option " + type);
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

    RcppResultSet rs;
    rs.add("value", option.NPV());
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

