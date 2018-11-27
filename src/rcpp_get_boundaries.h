#ifndef GET_BOUNDARIES_H
#define GET_BOUNDARIES_H
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

IntegerMatrix rcpp_get_boundaries(const IntegerMatrix x,
                                  const arma::imat directions);

std::map<int, unsigned> get_class_index_map(const std::vector<int> &classes);

#endif // GET_BOUNDARIES_H
