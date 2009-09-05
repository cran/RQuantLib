// -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- 
//
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright (C) 2002 - 2009 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: calendars.cpp 63 2009-04-10 20:24:04Z edd $
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

RcppExport SEXP QL_isBusinessDay(SEXP calParameters, SEXP dateSexp){

    SEXP rl=R_NilValue;
    char* exceptionMesg=NULL;

    try {
        RcppParams rparam(calParameters);

        std::string    calstr = rparam.getStringValue("calendar");
        RcppDateVector dates  = RcppDateVector(dateSexp);

        Calendar *pcal = NULL;

        if (calstr == "TARGET") { 		// generic calendar 
            pcal = new TARGET();

        } else if (calstr == "Canada" || calstr == "Canada/Settlement") {
            pcal = new Canada(Canada::Settlement);
        } else if (calstr == "Canada/TSX") {
            pcal = new Canada(Canada::TSX);

        } else if (calstr == "Germany" || calstr == "Germany/FrankfurtStockExchange") {
            pcal = new Germany(Germany::FrankfurtStockExchange);
        } else if (calstr == "Germany/Settlement") {
            pcal = new Germany(Germany::Settlement);
        } else if (calstr == "Germany/Xetra") {
            pcal = new Germany(Germany::Xetra);
        } else if (calstr == "Germany/Eurex") {
            pcal = new Germany(Germany::Eurex);

        } else if (calstr == "Italy" || calstr == "Italy/Settlement") {
            pcal = new Italy(Italy::Settlement);
        } else if (calstr == "Italy/Exchange") {
            pcal = new Italy(Italy::Exchange);

        } else if (calstr == "Japan") {
            pcal = new Japan();

        } else if (calstr == "UnitedKingdom" || calstr == "UnitedKingdom/Settlement") {
            pcal = new UnitedKingdom(UnitedKingdom::Settlement);
        } else if (calstr == "UnitedKingdom/Exchange") {
            pcal = new UnitedKingdom(UnitedKingdom::Exchange);
        } else if (calstr == "UnitedKingdom/Metals") {
            pcal = new UnitedKingdom(UnitedKingdom::Metals);

        } else if (calstr == "UnitedStates" || calstr == "UnitedStates/Settlement") {
            pcal = new UnitedStates(UnitedStates::Settlement);
        } else if (calstr == "UnitedStates/NYSE") {
            pcal = new UnitedStates(UnitedStates::NYSE);
        } else if (calstr == "UnitedStates/GovernmentBond") {
            pcal = new UnitedStates(UnitedStates::GovernmentBond);
        } else if (calstr == "UnitedStates/NERC") {
            pcal = new UnitedStates(UnitedStates::NERC);

        } else {
            throw std::invalid_argument("Calendar " + calstr + " not recognised ");
        }

        int n = dates.size();
        std::vector<int> bizdays(n);

        for (int i=0; i<n; i++) {
            QuantLib::Date day( dateFromR(dates(i)) );
            bizdays[i] = pcal->isBusinessDay(day);
        }

        delete pcal;

        RcppResultSet rs;
        rs.add("bizdays", bizdays);
        rl = rs.getReturnList();

    } catch(std::exception& ex) {
        exceptionMesg = copyMessageToR(ex.what());
    } catch(...) {
        exceptionMesg = copyMessageToR("unknown reason");
    }
  
    if(exceptionMesg != NULL)
        Rf_error(exceptionMesg);
    
    return rl;
}
