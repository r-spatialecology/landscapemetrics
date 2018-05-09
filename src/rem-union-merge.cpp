#include "rem-union-merge.hpp"

namespace landscapemetrics {

// Rem union-find merge.
// Algorithm 2, A New Parallel Algorithm for Two-Pass Connected
// Component Labeling, Gupta, et al.,
size_t rem_merge(sz_vec& parents, const size_t& x, const size_t& y) {
  // x and y are labels which are index 1 based; the vector is 0 based.
  auto root_x = x - 1;
  auto root_y = y - 1;

  while (parents.at(root_x) != parents.at(root_y)) {
    if (parents.at(root_x) > parents.at(root_y)) {
      if (root_x == parents.at(root_x)) {
        parents.at(root_x) = parents.at(root_y);
        return (parents.at(root_x));
      }
      auto z = parents.at(root_x) - 1;
      parents.at(root_x) = parents.at(root_y);
      root_x = z;
    } else {
      if (root_y == parents.at(root_y)) {
        parents.at(root_y) = parents.at(root_x);
        return (parents.at(root_x));
      }
      auto z = parents.at(root_y) - 1;
      parents.at(root_y) = parents.at(root_x);
      root_y = z;
    }
  }
  return (parents.at(root_x));
}

} // namespace landscapemetrics

