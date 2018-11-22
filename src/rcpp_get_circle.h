#ifndef RCPP_CREATE_ANEIGHBORHOOD_H
#define RCPP_CREATE_ANEIGHBORHOOD_H
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

NumericMatrix rcpp_get_circle(NumericMatrix points, double resolution);

#endif // RCPP_CREATE_ANEIGHBORHOOD_H
