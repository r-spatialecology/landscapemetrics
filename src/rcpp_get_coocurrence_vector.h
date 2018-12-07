#ifndef GET_COOCURRENCE_VECTOR_H
#define GET_COOCURRENCE_VECTOR_H
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

int triangular_index(int r, int c);

NumericVector rcpp_get_coocurrence_vector(IntegerMatrix x, arma::imat directions, bool ordered = true);

#endif // GET_COOCURRENCE_VECTOR_H
