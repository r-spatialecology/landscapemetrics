#ifndef RCPP_GET_ADJACENCY_H
#define RCPP_GET_ADJACENCY_H

#include <RcppArmadillo.h>
IntegerMatrix rcpp_get_adjacency(arma::imat x, int directions);
IntegerMatrix rcpp_get_pairs(arma::imat x, int directions = 4);

#endif
