#ifndef CCLABEL_H
#define CCLABEL_H

#include "Rcpp.h"

using namespace Rcpp;

// [[Rcpp::export]]
void rcpp_ccl(IntegerMatrix mat, int directions = 8);

#endif // CCLABEL_H
