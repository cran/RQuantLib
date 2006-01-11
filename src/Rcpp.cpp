// Rcpp.cpp - Rcpp 1.1
//
// Copyright (C) 2005  Dominick Samperi
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

#include "Rcpp.hpp"

RcppParams::RcppParams(SEXP params) {
    if(!isNewList(params))
	throw std::range_error("RcppParams: non-list passed to constructor");
    int len = length(params);
    SEXP names = getAttrib(params, R_NamesSymbol);
    for(int i = 0; i < len; i++) {
	pmap[string(CHAR(STRING_ELT(names,i)))] = i;
    }
    _params = params;
}

void RcppParams::checkNames(char *inputNames[], int len) {
    for(int i = 0; i < len; i++) {
	map<string,int>::iterator iter = pmap.find(inputNames[i]);
	if(iter == pmap.end()) {
	    string mesg = "checkNames: missing required parameter ";
	    throw range_error(mesg+inputNames[i]);
	}
    }
}

double RcppParams::getDoubleValue(string name) {
    map<string,int>::iterator iter = pmap.find(name);
    if(iter == pmap.end()) {
	string mesg = "getDoubleValue: no such name: ";
	throw std::range_error(mesg+name);
    }
    int posn = iter->second;
    SEXP elt = VECTOR_ELT(_params,posn);
    if(!isNumeric(elt) || length(elt) != 1) {
	string mesg = "getDoubleValue: must be scalar ";
	throw std::range_error(mesg+name);
    }
    if(isInteger(elt))
	return (double)INTEGER(elt)[0];
    else if(isReal(elt))
	return REAL(elt)[0];
    else {
	string mesg = "getDoubleValue: invalid value for ";
	throw std::range_error(mesg+name);
    }
    return 0; // never get here
}

int RcppParams::getIntValue(string name) {
    map<string,int>::iterator iter = pmap.find(name);
    if(iter == pmap.end()) {
	string mesg = "getIntValue: no such name: ";
	throw std::range_error(mesg+name);
    }
    int posn = iter->second;
    SEXP elt = VECTOR_ELT(_params,posn);
    if(!isNumeric(elt) || length(elt) != 1) {
	string mesg = "getIntValue: must be scalar: ";
	throw std::range_error(mesg+name);
    }
    if(isInteger(elt))
	return INTEGER(elt)[0];
    else if(isReal(elt))
	return (int)REAL(elt)[0];
    else {
	string mesg = "getIntValue: invalid value for: ";
	throw std::range_error(mesg+name);
    }
    return 0; // never get here
}

bool RcppParams::getBoolValue(string name) {
    map<string,int>::iterator iter = pmap.find(name);
    if(iter == pmap.end()) {
	string mesg = "getBoolValue: no such name: ";
	throw std::range_error(mesg+name);
    }
    int posn = iter->second;
    SEXP elt = VECTOR_ELT(_params,posn);
    if(isLogical(elt))
	return INTEGER(elt)[0];
    else {
	string mesg = "getBoolValue: invalid value for: ";
	throw std::range_error(mesg+name);
    }
    return false; // never get here
}

string RcppParams::getStringValue(string name) {
    map<string,int>::iterator iter = pmap.find(name);
    if(iter == pmap.end()) {
	string mesg = "getStringValue: no such name: ";
	throw std::range_error(mesg+name);
    }
    int posn = iter->second;
    SEXP elt = VECTOR_ELT(_params,posn);
    if(isString(elt))
		return string(CHAR(STRING_ELT(elt,0)));
    else {
	string mesg = "getStringValue: invalid value for: ";
	throw std::range_error(mesg+name);
    }
    return ""; // never get here
}

#ifdef USING_QUANTLIB
Date RcppParams::getDateValue(string name) {
    map<string,int>::iterator iter = pmap.find(name);
    if(iter == pmap.end()) {
	string mesg = "getDateValue: no such name: ";
	throw std::range_error(mesg+name);
    }
    int posn = iter->second;
    int day=0, month=0, year=0;
    SEXP dateSEXP = VECTOR_ELT(_params, posn);
    if(!isNumeric(dateSEXP) || length(dateSEXP) != 3) {
	string mesg = "getDateValue: invalid date: ";
	throw std::range_error(mesg+name);
    }
    if(isInteger(dateSEXP)) {
	month   = INTEGER(dateSEXP)[0];
	day   = INTEGER(dateSEXP)[1];
	year  = INTEGER(dateSEXP)[2];
    }
    else if(isReal(dateSEXP)) {
	month   = (int)REAL(dateSEXP)[0];
	day   = (int)REAL(dateSEXP)[1];
	year  = (int)REAL(dateSEXP)[2];
    }
    else {
	string mesg = "getDateValue: invalid value for: ";
	throw std::range_error(mesg+name);
    }
    Date d(day, (Month)month, year);
    return d;
}
#else
Date RcppParams::getDateValue(string name) {
    map<string,int>::iterator iter = pmap.find(name);
    if(iter == pmap.end()) {
	string mesg = "getDateValue: no such name: ";
	throw std::range_error(mesg+name);
    }
    int posn = iter->second;
    int day=0, month=0, year=0;
    SEXP dateSEXP = VECTOR_ELT(_params, posn);
    if(!isNumeric(dateSEXP) || length(dateSEXP) != 3) {
	string mesg = "getDateValue: invalid date: ";
	throw std::range_error(mesg+name);
    }
    if(isInteger(dateSEXP)) {
	month   = INTEGER(dateSEXP)[0];
	day   = INTEGER(dateSEXP)[1];
	year  = INTEGER(dateSEXP)[2];
    }
    else if(isReal(dateSEXP)) {
	month   = (int)REAL(dateSEXP)[0];
	day   = (int)REAL(dateSEXP)[1];
	year  = (int)REAL(dateSEXP)[2];
    }
    else {
	string mesg = "getDateValue: invalid value for: ";
	throw std::range_error(mesg+name);
    }
    Date d(day, month, year);
    return d;
}
#endif


template <typename T>
RcppVector<T>::RcppVector(SEXP vec) {
    int i;

    // The function isVector returns TRUE for vectors AND
    // matrices, so it does not distinguish. We could
    // check the dim attribute here to be sure that it
    // is not present (i.e., dimAttr == R_NilValue, not 0!).
    // But it is easier to simply check if it is set via
    // isMatrix (in which case we don't have a vector).
    if(!isNumeric(vec) || isMatrix(vec) || isLogical(vec))
	throw std::range_error("RcppVector: invalid numeric vector in constructor");

    len = length(vec);
    if(len == 0)
	throw std::range_error("RcppVector: null vector in constructor");
    int isInt = isInteger(vec);
    v = (T *)R_alloc(len, sizeof(T));
    if(isInt) {
	for(i = 0; i < len; i++)
	    v[i] = (T)(INTEGER(vec)[i]);
    }	
    else {
	for(i = 0; i < len; i++)
	    v[i] = (T)(REAL(vec)[i]);
    }
}

template <typename T>
RcppVector<T>::RcppVector(int _len) {
    len = _len;
    v = (T *)R_alloc(len, sizeof(T));
    for(int i = 0; i < len; i++)
	v[i] = 0;
}

template <typename T>
T *RcppVector<T>::cVector() {
    T* tmp = (T *)R_alloc(len, sizeof(T));
    for(int i = 0; i < len; i++)
	tmp[i] = v[i];
    return tmp;
}

template <typename T>
RcppMatrix<T>::RcppMatrix(SEXP mat) {

    if(!isNumeric(mat) || !isMatrix(mat))
	throw std::range_error("RcppMatrix: invalid numeric matrix in constructor");

    // Get matrix dimensions
    SEXP dimAttr = getAttrib(mat, R_DimSymbol);
    dim1 = INTEGER(dimAttr)[0];
    dim2 = INTEGER(dimAttr)[1];

    // We guard against  the possibility that R might pass an integer matrix.
    // Can be prevented using R code: temp <- as.double(a), dim(temp) <- dim(a)
    int i,j;
    int isInt = isInteger(mat);
    T *m = (T *)R_alloc(dim1*dim2, sizeof(T));
    a = (T **)R_alloc(dim1, sizeof(T *));
    for(i = 0; i < dim1; i++)
	a[i] = m + i*dim2;
    if(isInt) {
	for(i=0; i < dim1; i++)
	    for(j=0; j < dim2; j++)
		a[i][j] = (T)(INTEGER(mat)[i+dim1*j]);
    }	
    else {
	for(i=0; i < dim1; i++)
	    for(j=0; j < dim2; j++)
		a[i][j] = (T)(REAL(mat)[i+dim1*j]);
    }	
}

template <typename T>
RcppMatrix<T>::RcppMatrix(int _dim1, int _dim2) {
    dim1 = _dim1;
    dim2 = _dim2;
    int i,j;
    T *m = (T *)R_alloc(dim1*dim2, sizeof(T));
    a = (T **)R_alloc(dim1, sizeof(T *));
    for(i = 0; i < dim1; i++)
	a[i] = m + i*dim2;
    for(i=0; i < dim1; i++)
	for(j=0; j < dim2; j++)
	    a[i][j] = 0;
}

template <typename T>
T **RcppMatrix<T>::cMatrix() {
    int i,j;
    T *m = (T *)R_alloc(dim1*dim2, sizeof(T));
    T **tmp = (T **)R_alloc(dim1, sizeof(T *));
    for(i = 0; i < dim1; i++)
	tmp[i] = m + i*dim2;
    for(i=0; i < dim1; i++)
	for(j=0; j < dim2; j++)
	    tmp[i][j] = a[i][j];
    return tmp;
}

// Explicit instantiation (required for external linkage)
template class RcppVector<int>;
template class RcppVector<double>;
template class RcppMatrix<int>;
template class RcppMatrix<double>;

void RcppResultSet::add(string name, double x) {
    SEXP value = PROTECT(allocVector(REALSXP, 1));
    numProtected++;
    REAL(value)[0] = x;
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, int i) {
    SEXP value = PROTECT(allocVector(INTSXP, 1));
    numProtected++;
    INTEGER(value)[0] = i;
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, string strvalue) {
    SEXP value = PROTECT(allocVector(STRSXP, 1));
    numProtected++;
    SET_STRING_ELT(value, 0, mkChar(strvalue.c_str()));
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, double *vec, int len) {
    SEXP value = PROTECT(allocVector(REALSXP, len));
    numProtected++;
    for(int i = 0; i < len; i++)
	REAL(value)[i] = vec[i];
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, int *vec, int len) {
    SEXP value = PROTECT(allocVector(INTSXP, len));
    numProtected++;
    for(int i = 0; i < len; i++)
	INTEGER(value)[i] = vec[i];
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, double **mat, int nx, int ny) {
    SEXP value = PROTECT(allocMatrix(REALSXP, nx, ny));
    numProtected++;
    for(int i = 0; i < nx; i++)
	for(int j = 0; j < ny; j++)
	    REAL(value)[i + nx*j] = mat[i][j];
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, int **mat, int nx, int ny) {
    SEXP value = PROTECT(allocMatrix(INTSXP, nx, ny));
    numProtected++;
    for(int i = 0; i < nx; i++)
	for(int j = 0; j < ny; j++)
	    INTEGER(value)[i + nx*j] = mat[i][j];
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, RcppVector<int>& vec) {
    int len = vec.getLength();
    int *a = vec.cVector();
    SEXP value = PROTECT(allocVector(INTSXP, len));
    numProtected++;
    for(int i = 0; i < len; i++)
	INTEGER(value)[i] = a[i];
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, RcppVector<double>& vec) {
    int len = vec.getLength();
    double *a = vec.cVector();
    SEXP value = PROTECT(allocVector(REALSXP, len));
    numProtected++;
    for(int i = 0; i < len; i++)
	REAL(value)[i] = a[i];
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, RcppMatrix<int>& mat) {
    int nx = mat.getDim1();
    int ny = mat.getDim2();
    int **a = mat.cMatrix();
    SEXP value = PROTECT(allocMatrix(INTSXP, nx, ny));
    numProtected++;
    for(int i = 0; i < nx; i++)
	for(int j = 0; j < ny; j++)
	    INTEGER(value)[i + nx*j] = a[i][j];
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, RcppMatrix<double>& mat) {
    int nx = mat.getDim1();
    int ny = mat.getDim2();
    double **a = mat.cMatrix();
    SEXP value = PROTECT(allocMatrix(REALSXP, nx, ny));
    numProtected++;
    for(int i = 0; i < nx; i++)
	for(int j = 0; j < ny; j++)
	    REAL(value)[i + nx*j] = a[i][j];
    values.push_back(make_pair(name, value));
}

void RcppResultSet::add(string name, SEXP sexp, bool isProtected) {
    values.push_back(make_pair(name, sexp));
    if(isProtected)
	numProtected++;
}

SEXP RcppResultSet::getReturnList() {
    int nret = values.size();
    SEXP rl = PROTECT(allocVector(VECSXP,nret));
    SEXP nm = PROTECT(allocVector(STRSXP,nret));
    list<pair<string,SEXP> >::iterator iter = values.begin();
    for(int i = 0; iter != values.end(); iter++, i++) {
	SET_VECTOR_ELT(rl, i, iter->second);
	SET_STRING_ELT(nm, i, mkChar(iter->first.c_str()));
    }
    setAttrib(rl, R_NamesSymbol, nm);
    UNPROTECT(numProtected+2);
    return rl;
}

#ifdef USING_QUANTLIB
ostringstream& operator<<(ostringstream& os, const Date& d) {
    os << d.month() << " " << d.weekday() << ", " << d.year();
    return os;
}
#else

// Dummy Date class implementation when USING_QUANTLIB is not set...

static char* months[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", 
			       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

Date::Date(int day, int month, int year) throw(range_error) {
    if(day < 0 || day > 31 || month < 0 || month >= 12 || year < 0)
	throw std::range_error("Date parameters out of range");
    _day = day, _month = month, _year = year;
}

char* Date::getMonth() const {
    return months[_month];
}

ostringstream& operator<<(ostringstream& os, const Date& d) {
    os << d.getMonth() << " " << d.getDay() << ", " << d.getYear();
    return os;
}
#endif

// This function copies the message string to R-managed memory so the
// original C++ message object can be destroyed (when it goes out of
// scope before returning to R).
//
// Thanks to Paul Roebuck for pointing out that the exception
// object's memory will not be reclaimed if error() is called inside of
// a catch block (due to a setjmp() call), and for suggesting the
// work-around.
char *copyMessageToR(const char* const mesg) {
    char* Rmesg;
    char* prefix = "Exception: ";
    void* Rheap = R_alloc(std::strlen(prefix)+std::strlen(mesg)+1,sizeof(char));
    Rmesg = static_cast<char*>(Rheap);
    std::strcpy(Rmesg, prefix);
    std::strcat(Rmesg, mesg);
    return Rmesg;
}

