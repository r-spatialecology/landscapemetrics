#include "borders.hpp"

namespace {

void validate_input(const arma::mat& m) {
  if (m.n_cols < 2 || m.n_rows < 2) {
    Rcpp::stop("Input matrix have >=2 rows and columns!");
  }
}

} // anonymous namespace

namespace landscapemetrics {

border_mat borders(const arma::mat& m) {
  using namespace arma;

  validate_input(m);

  // Column major border connection point values
  const border_t N(1), NW(2), W(4), SW(8), S(16), SE(32), E(64), NE(128);

  // Subview matrix dims.
  const uword r = m.n_rows - 1;
  const uword c = m.n_cols - 1;

  // vertical, horizontal, diagonal and antidiagonal equality.
  typedef border_mat bm;
  const bm v = conv_to<bm>::from(m.head_rows(r) == m.tail_rows(r));
  const bm h = conv_to<bm>::from(m.head_cols(c) == m.tail_cols(c));
  const bm d = conv_to<bm>::from(m(0, 0, size(r, c)) == m(1, 1, size(r, c)));
  const bm a = conv_to<bm>::from(m(1, 0, size(r, c)) == m(0, 1, size(r, c)));

  bm b = bm(size(m));
  b.fill(255);
  b.tail_rows(r) -= N * v;
  b.head_rows(r) -= S * v;
  b.tail_cols(c) -= W * h;
  b.head_cols(c) -= E * h;
  b(1, 1, size(r, c)) -= NW * d;
  b(0, 0, size(r, c)) -= SE * d;
  b(1, 0, size(r, c)) -= NE * a;
  b(0, 1, size(r, c)) -= SW * a;

  return b;
}

} // namespace landscapemetrics

//' @title 8-way connectedness border states.
//'
//' @description 8-way neighborhood connected component border
//' (edge/vertice) states.
//'
//' @param m A two dimensional numeric matrix.
//'
//' @return Numeric matrix of the same dimensions as mat with
//' values indicating 8-way connected border states.
//'
//' @details A cell is considered to have 8 distinct borders indexed
//' in counter clockwise order with values of 2^c(0:7) beginning
//' with the North edge and ending with the Northeast vertex. A cell
//' fully enclosed by similar neighbors has no borders ( a 0 value )
//' whereas when fully enclosed by dissimilar neighbors is fully
//' bordered ( a 255 value ).
//'
//' @author Thell Fowler \email{thell@tbfowler.name}
//'
//' @keywords internal
//'
//' @name ccl_borders
//' @export
// [[Rcpp::export]]
arma::mat ccl_borders(const arma::mat& m) {
  return arma::conv_to<arma::mat>::from(landscapemetrics::borders(m));
}
