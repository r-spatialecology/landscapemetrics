#ifndef GET_NEIGHBORS_H
#define GET_NEIGHBORS_H

#include <RcppArmadillo.h>
arma::ivec get_neighbors(arma::imat x, int i, int j, int directions = 8);

#endif
