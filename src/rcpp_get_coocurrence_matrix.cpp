#include "get_coocurrence_matrix.h"
#include "get_adjacency.h"

IntegerMatrix rcpp_get_coocurrence_matrix(arma::imat x, arma::imat directions) {

    // get unique values
    arma::ivec u = arma::conv_to<arma::ivec>::from(arma::unique(x.elem(find(x != INT_MIN))));

    // create a matrix of zeros of unique values size
    arma::imat cooc_mat(u.n_elem, u.n_elem, arma::fill::zeros);

    // extract adjency pairs
    IntegerMatrix pairs = rcpp_get_pairs(x, directions);

    IntegerVector center_cells = pairs[0];
    IntegerVector neigh_cells = pairs[1];

    // number of pairs
    int num_pairs = center_cells.length();

    // for each row and col
    for (int i = 0; i < num_pairs; i++) {

        // extract value of central cell and its neighbot
        int center = center_cells(i);
        int neigh = neigh_cells(i);

        // find their location in the output matrix
        arma::uvec loc_c = find(u == center);
        arma::uvec loc_n = find(u == neigh);

        // add its count
        cooc_mat(loc_c, loc_n) += 1;

    }

    // return a coocurence matrix
    IntegerMatrix cooc_mat_result = as<IntegerMatrix>(wrap(cooc_mat));

    // add names
    List u_names = List::create(u, u);
    cooc_mat_result.attr("dimnames") = u_names;
    return cooc_mat_result;
}
