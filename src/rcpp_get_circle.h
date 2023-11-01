#ifndef RCPP_GET_CIRCLE_H
#define RCPP_GET_CIRCLE_H
#include "Rcpp.h"
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame rcpp_get_circle(const IntegerMatrix &mat,
                          const double resolution_xy = 1.0);

#endif // RCPP_GET_CIRCLE_H

/*** R
landscape_labeled <- get_patches(landscape,
                                 class = 1,
                                 directions = 8)[[1]]

rcpp_get_circle(landscape_labeled,
                resolution_xy = 1)

*/
