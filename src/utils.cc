
// RQuantLib -- R interface to the QuantLib libraries
//
// Copyright 2002, 2003 Dirk Eddelbuettel <edd@debian.org>
//
// $Id: utils.cc,v 1.1 2003/11/29 01:09:47 edd Exp $
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

extern "C" {

#include <R.h>
#include <Rinternals.h>

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

}
