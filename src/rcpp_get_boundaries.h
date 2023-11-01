//#ifndef GET_BOUNDARIES_H
//#define GET_BOUNDARIES_H
#include "Rcpp.h"
using namespace Rcpp;

//// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
IntegerMatrix rcpp_get_boundaries(const IntegerMatrix &xx,
                                  int directions = 4);

IntegerMatrix add_padding(const IntegerMatrix &xx);
IntegerMatrix rm_padding(const IntegerMatrix &xx);
void get_boundaries_4(IntegerMatrix &x);
void get_boundaries_8(IntegerMatrix &x);

//#endif // GET_BOUNDARIES_H
