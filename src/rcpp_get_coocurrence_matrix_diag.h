#ifndef RCPP_GET_CONTIG_MEAN_H
#define RCPP_GET_CONTIG_MEAN_H
#include <RcppArmadillo.h>
using namespace Rcpp;

IntegerVector rcpp_get_coocurrence_matrix_diag(const IntegerMatrix &x,
                                               const arma::imat directions);
#endif // RCPP_GET_CONTIG_MEAN_H
