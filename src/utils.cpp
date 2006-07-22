
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002-2006 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: utils.cpp,v 1.12 2006/04/15 21:13:28 edd Exp $
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
  
    boost::shared_ptr<PricingEngine> engine;
    switch (engineType) {
    case Analytic:
	engine = boost::shared_ptr<PricingEngine>(new AnalyticEuropeanEngine);
	break;
    case JR:
      engine = boost::shared_ptr<PricingEngine>(
			new BinomialVanillaEngine<JarrowRudd>(binomialSteps));
      break;
    case CRR:
      engine = boost::shared_ptr<PricingEngine>(
		new BinomialVanillaEngine<CoxRossRubinstein>(binomialSteps));
    case EQP:
      engine = boost::shared_ptr<PricingEngine>(
		new BinomialVanillaEngine<AdditiveEQPBinomialTree>(
      						   binomialSteps));
      break;
    case TGEO:
      engine = boost::shared_ptr<PricingEngine>(
		new BinomialVanillaEngine<Trigeorgis>(binomialSteps));
      break;
    case TIAN:
      engine = boost::shared_ptr<PricingEngine>(
		new BinomialVanillaEngine<Tian>(binomialSteps));
      break;
    case LR:
      engine = boost::shared_ptr<PricingEngine>(
		new BinomialVanillaEngine<LeisenReimer>(binomialSteps));
      break;
    case FiniteDifferences:
      engine = boost::shared_ptr<PricingEngine>(
		new FDEuropeanEngine(binomialSteps,samples));
      break;
    case PseudoMonteCarlo:
      engine = MakeMCEuropeanEngine<PseudoRandom>().withStepsPerYear(1)
      .withSamples(samples)
      .withSeed(42);
      break;
    case QuasiMonteCarlo:
      engine = MakeMCEuropeanEngine<LowDiscrepancy>().withStepsPerYear(1)
	.withSamples(samples);
      break;
    default:
      QL_FAIL("Unknown engine type");
    }
    boost::shared_ptr<BlackScholesProcess> 
      stochProcess(new
		   BlackScholesProcess(
				       Handle<Quote>(u),
				       Handle<YieldTermStructure>(q),
				       Handle<YieldTermStructure>(r),
				       Handle<BlackVolTermStructure>(vol)));
    return boost::shared_ptr<VanillaOption>(new
	   EuropeanOption(stochProcess, payoff, exercise, engine));

}

// QuantLib option setup utils, copied from the test-suite sources

boost::shared_ptr<YieldTermStructure>
makeFlatCurve(const Date& today,
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
	new BlackConstantVol(today, Handle<Quote>(vol), dc));
}
