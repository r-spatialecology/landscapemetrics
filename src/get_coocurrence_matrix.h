#ifndef GET_COOCURRENCE_MATRIX_H
#define GET_COOCURRENCE_MATRIX_H
#define DARMA_64BIT_WORD 1
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
IntegerMatrix rcpp_get_coocurrence_matrix(arma::imat x, arma::imat directions);

// [[Rcpp::export]]
IntegerMatrix rcpp_get_coocurrence_matrix2(const IntegerVector x,
                                           const arma::imat directions,
                                           unsigned ncols, unsigned nrows,
                                           IntegerVector classes);
#endif // GET_COOCURRENCE_MATRIX_H
