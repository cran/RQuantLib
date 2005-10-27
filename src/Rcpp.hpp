// Rcpp.hpp
//
// Copyright (C) 2005  Dominick Samperi
//
// $Id: Rcpp.hpp,v 1.3 2005/10/14 05:23:59 dsamperi Exp $ 
//
// This program is part of the Rcpp R/C++ interface library for R (GNU S).
// It is made available under the terms of the GNU General Public
// License, version 2, or at your option, any later version.
//
// This program is distributed in the hope that it will be
// useful, but WITHOUT ANY WARRANTY; without even the implied
// warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE.  See the GNU General Public License for more
// details.

#ifdef USING_QUANTLIB
#include <ql/quantlib.hpp>
using namespace QuantLib;
#else
#include <stdexcept>
#include <sstream>
#include <string>
#include <list>
#include <map>
#endif

using namespace std;

#include <R.h>
#include <Rinternals.h>

#ifdef BUILDING_DLL
#define RcppExport extern "C" __declspec(dllexport)
#else
#define RcppExport extern "C"
#endif

#ifndef USING_QUANTLIB

// When USING_QUANTLIB is not set we use this dummy date class.
// All it does is check that d/m/y is in range, and print the date.
// TODO: implement a real Date class.
class Date {
public:
    Date(int day, int month, int year) throw(range_error);
    int getDay() const { return _day; }
    int getYear() const { return _year; }
    char* getMonth() const;
private:
    int _day, _month, _year;
};
ostringstream& operator<<(ostringstream& os, const Date& d);

#endif

class RcppParams {
public:
    RcppParams(SEXP params);
    void   checkNames(char *inputNames[], int len);
    double getDoubleValue(string name);
    int    getIntValue(string name);
    string getStringValue(string name);
    bool   getBoolValue(string name);
    Date   getDateValue(string name);
private:
    map<string, int> pmap;
    SEXP _params;
};

class RcppNamedList {
public:
    RcppNamedList(SEXP theList) {
	if(!isNewList(theList))
	    error("RcppNamedList: non-list passed to constructor");
        len = length(theList);
        names = getAttrib(theList, R_NamesSymbol);
        namedList = theList;
    }
    string getName(int i) {
        if(i < 0 || i >= len)
            error("RcppNamedList::getName: index out of bounds: %d", i);
        return string(CHAR(STRING_ELT(names,i)));
    }
    double getValue(int i) {
        if(i < 0 || i >= len)
            error("RcppNamedList::getValue: index out of bounds: %d", i);
	SEXP elt = VECTOR_ELT(namedList, i);
	if(isReal(elt))
	    return REAL(elt)[0];
	else if(isInteger(elt))
	    return (double)INTEGER(elt)[0];
	else
	    error("RcppNamedList: contains non-numeric value");
	return 0; // never get here
    }
    int getLength() { return len; }
private:
    int len;
    SEXP namedList;
    SEXP names;
};

template <typename T>
class RcppVector {
public:
    RcppVector(SEXP vec);
    RcppVector(int len);
    int getLength() { return len; }
    T& operator()(int i);
    T *cVector();
private:
    int len;
    T *v;
};

template <typename T>
class RcppMatrix {
public:
    RcppMatrix(SEXP mat);
    RcppMatrix(int nx, int ny);
    int getDim1() { return dim1; }
    int getDim2() { return dim2; }
    T& operator()(int i, int j);
    T **cMatrix();
private:
    int dim1, dim2;
    T **a;
};

class RcppResultSet {
public:
    RcppResultSet() { numProtected = 0; }
    void add(string, double);
    void add(string, int);
    void add(string, string);
    void add(string, double *, int);
    void add(string, int *, int);
    void add(string, double **, int, int);
    void add(string, int **, int, int);
    void add(string, RcppVector<int>&);
    void add(string, RcppVector<double>&);
    void add(string, RcppMatrix<int>&);
    void add(string, RcppMatrix<double>&);
    void add(string, SEXP, bool isProtected);
    SEXP getReturnList();
private:
    int numProtected;
    list<pair<string,SEXP> > values;
};

