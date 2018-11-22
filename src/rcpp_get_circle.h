#ifndef RCPP_CREATE_ANEIGHBORHOOD_H
#define RCPP_CREATE_ANEIGHBORHOOD_H
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

NumericMatrix rcpp_get_circle(NumericMatrix points, double resolution_x, double resolution_y);

#endif // RCPP_CREATE_ANEIGHBORHOOD_H
