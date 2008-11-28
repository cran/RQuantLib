// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- 
//
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002-2006 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: utils.cpp,v 1.14 2007/12/31 02:00:08 edd Exp $
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

#include "rquantlib.hpp"

// cf QuantLib-0.9.0/test-suite/europeanoption.cpp
boost::shared_ptr<VanillaOption>
makeOption(const boost::shared_ptr<StrikedTypePayoff>& payoff,
           const boost::shared_ptr<Exercise>& exercise,
           const boost::shared_ptr<Quote>& u,
           const boost::shared_ptr<YieldTermStructure>& q,
           const boost::shared_ptr<YieldTermStructure>& r,
           const boost::shared_ptr<BlackVolTermStructure>& vol,
           EngineType engineType,
           Size binomialSteps,
           Size samples) {
  
    boost::shared_ptr<GeneralizedBlackScholesProcess> stochProcess = makeProcess(u,q,r,vol);
    boost::shared_ptr<PricingEngine> engine;

    switch (engineType) {
    case Analytic:
        engine = boost::shared_ptr<PricingEngine>(new AnalyticEuropeanEngine(stochProcess));
        break;
    case JR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<JarrowRudd>(stochProcess, binomialSteps));
        break;
    case CRR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<CoxRossRubinstein>(stochProcess, binomialSteps));
    case EQP:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<AdditiveEQPBinomialTree>(stochProcess, binomialSteps));
        break;
    case TGEO:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Trigeorgis>(stochProcess, binomialSteps));
        break;
    case TIAN:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Tian>(stochProcess, binomialSteps));
        break;
    case LR:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<LeisenReimer>(stochProcess, binomialSteps));
        break;
    case JOSHI:
        engine = boost::shared_ptr<PricingEngine>(new BinomialVanillaEngine<Joshi4>(stochProcess, binomialSteps));
        break;
    case FiniteDifferences:
        engine = boost::shared_ptr<PricingEngine>(new FDEuropeanEngine(stochProcess, binomialSteps, samples));
        break;
    case Integral:
        engine = boost::shared_ptr<PricingEngine>(new IntegralEngine(stochProcess));
        break;
    case PseudoMonteCarlo:
        engine = MakeMCEuropeanEngine<PseudoRandom>(stochProcess)
            .withStepsPerYear(1)
            .withSamples(samples)
            .withSeed(42);
        break;
    case QuasiMonteCarlo:
        engine = MakeMCEuropeanEngine<LowDiscrepancy>(stochProcess)
            .withStepsPerYear(1)
            .withSamples(samples);
        break;
    default:
        QL_FAIL("Unknown engine type");
    }
    boost::shared_ptr<VanillaOption> option(new EuropeanOption(payoff, exercise));
    option->setPricingEngine(engine);
    return option;
}

// QuantLib option setup utils, copied from the test-suite sources

boost::shared_ptr<YieldTermStructure>
makeFlatCurve(const Date& today,
	      const boost::shared_ptr<Quote>& forward,
	      const DayCounter& dc) {
    return boost::shared_ptr<YieldTermStructure>(
	   new FlatForward(today, Handle<Quote>(forward), dc));
}

boost::shared_ptr<YieldTermStructure>
flatRate(const Date& today,
	 const boost::shared_ptr<Quote>& forward,
	 const DayCounter& dc) {
  return boost::shared_ptr<YieldTermStructure>(
	       new FlatForward(today, Handle<Quote>(forward), dc));
}
  
boost::shared_ptr<BlackVolTermStructure> 
makeFlatVolatility(const Date& today,
                   const boost::shared_ptr<Quote>& vol,
                   const DayCounter dc) {
    return boost::shared_ptr<BlackVolTermStructure>(
           new BlackConstantVol(today, NullCalendar(), Handle<Quote>(vol), dc));
}

boost::shared_ptr<BlackVolTermStructure>
flatVol(const Date& today,
	const boost::shared_ptr<Quote>& vol,
	const DayCounter& dc) {
  return boost::shared_ptr<BlackVolTermStructure>(new
            BlackConstantVol(today, NullCalendar(), Handle<Quote>(vol), dc));
}

boost::shared_ptr<GeneralizedBlackScholesProcess>
makeProcess(const boost::shared_ptr<Quote>& u,
            const boost::shared_ptr<YieldTermStructure>& q,
            const boost::shared_ptr<YieldTermStructure>& r,
            const boost::shared_ptr<BlackVolTermStructure>& vol) {
    return boost::shared_ptr<BlackScholesMertonProcess>(
           new BlackScholesMertonProcess(Handle<Quote>(u),
                                         Handle<YieldTermStructure>(q),
                                         Handle<YieldTermStructure>(r),
                                         Handle<BlackVolTermStructure>(vol)));
}

// R uses dates indexed to Jan 1, 1970. Rcpp uses an internal Julian Date representation,
// but Quantlib uses the 'spreadsheet' format indexed to 1905 so we need to adjust
int dateFromR(const RcppDate &d) {
    return(d.getJDN() - RcppDate::Jan1970Offset + RcppDate::QLtoJan1970Offset);
}

