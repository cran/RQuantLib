// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002, 2003, 2004 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: rquantlib.h,v 1.4 2005/04/27 02:29:02 edd Exp $
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

  void insertListElement(SEXP &list, SEXP &names,
			 const int pos, const double value, 
			 const char *label);
  SEXP getListElement(SEXP list, char *str);

  boost::shared_ptr<YieldTermStructure> 
     makeFlatCurve(const Date& today,
		   const boost::shared_ptr<Quote>& forward,
		   DayCounter dc);
  boost::shared_ptr<BlackVolTermStructure> 
     makeFlatVolatility(const Date& today,
			const boost::shared_ptr<Quote>& vol,
			DayCounter dc);

  enum EngineType { Analytic,
		    JR, CRR, EQP, TGEO, TIAN, LR,
		    PseudoMonteCarlo, QuasiMonteCarlo };

  boost::shared_ptr<VanillaOption>
  makeOption(const boost::shared_ptr<StrikedTypePayoff>& payoff,
	     const boost::shared_ptr<Exercise>& exercise,
	     const boost::shared_ptr<Quote>& u,
	     const boost::shared_ptr<YieldTermStructure>& q,
	     const boost::shared_ptr<YieldTermStructure>& r,
	     const boost::shared_ptr<BlackVolTermStructure>& vol,
	     EngineType engineType = Analytic);
}
