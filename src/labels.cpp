#include "labels.h"

#include <array>
#include <functional>

namespace landscapemetrics {

// Given a matrix of border states valid widthrepresenting 8-way connected closed
// borders as set bits, non-connected = 255 and fully connected = 0,
// check the first four bits for openness to connected component.
// Use column major offsets from current index for ccl[ N, NW, W, SW ].
Labels labels(const border_mat& border_states) {
  using namespace arma;

  auto dim(border_states.n_rows);
  std::array<size_t, 4> ccl = {1, dim + 1, dim, dim - 1};

  sz_vec parents;
  parents.reserve(border_states.n_elem);
  std::vector<std::reference_wrapper<size_t>> labels;
  labels.reserve(border_states.n_elem);

  size_t index(0), label(0);
  border_t state(0);
  auto ccl_ref = std::ref(label);

  for (const auto& borders : border_states) {
    state = (~borders) & 15;
    if (state > 0) {
      ccl_ref = labels.at(index - ccl.at(__builtin_ffsl(state) - 1));
    } else {
      parents.emplace_back(parents.size() + 1);
      ccl_ref = parents.back();
    }
    labels.emplace_back(ccl_ref);

    if (state > 8 && state < 12) {
      rem_merge(parents, labels[index], labels.at(index - ccl[3]));
    }
    ++index;
  }

  // Flatten parent labels to a sequence.
  index = 0;
  label = 0;
  for (auto& p : parents) {
    p = p < ++index ? parents[p - 1] : ++label;
  }

  // The label matrix
  index = 0;
  label_mat ccl_umat = label_mat(size(border_states));
  ccl_umat.imbue([&]() { return labels[index++]; });

  // Component membership counts (count the refs to each parent).
  sz_vec member_counts(label);
  sz_vec p_indexes(parents.begin(), parents.end());
  for (auto& p : parents)
    p = 0;
  for (auto& p : labels)
    p = ++p;
  for (index = 0; index < parents.size(); ++index)
    member_counts[p_indexes[index] - 1] += parents[index];

  return std::make_pair(ccl_umat, member_counts);
}

} // namespace landscapemetrics

//' @title 8-way connected component labelling.
//'
//' @description 8-way neighborhood connected component labelling of
//' all given values.
//'
//' @param m A two dimensional numeric matrix.
//'
//' @return Numeric matrix of the same dimensions as mat with
//'   values indicating component identification label where values
//'   increase in column major order.
//'
//' @author Thell Fowler \email{thell@tbfowler.name}
//'
//' @keywords internal
//'
//' @name ccl_labels
//' @export
// [[Rcpp::export]]
Rcpp::List ccl_labels(const arma::mat& m) {
  using namespace Rcpp;
  const auto b(landscapemetrics::borders(m));
  const auto l(landscapemetrics::labels(b));
  return List::create(wrap(arma::conv_to<arma::mat>::from(l.first)),
                      wrap(l.second));
}
