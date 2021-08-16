#ifndef GET_COOCURRENCE_MATRIX_SINGLE_H
#define GET_COOCURRENCE_MATRIX_SINGLE_H
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

IntegerMatrix rcpp_get_coocurrence_matrix_single(const IntegerMatrix x,
                                                 const arma::imat directions,
                                                 const int single_class);
#endif // GET_COOCURRENCE_MATRIX_SINGLE_H
