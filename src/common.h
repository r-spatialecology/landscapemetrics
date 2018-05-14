#pragma once

#include <RcppArmadillo.h>

namespace landscapemetrics {

  typedef unsigned char border_t;
  typedef arma::uchar_mat border_mat;

  typedef std::vector<size_t> sz_vec;
  typedef std::vector<sz_vec> nested_sz_vec;
  typedef std::vector<nested_sz_vec> cc_nested_sz_vec;

  typedef arma::umat label_mat;
  typedef std::pair<label_mat, sz_vec> Labels;

  typedef uint_fast8_t incident_t;
  typedef std::vector<incident_t> incident_vec;

  typedef size_t vertex_t;

  typedef std::tuple<incident_t, vertex_t, vertex_t> Incident;
  typedef std::vector<Incident> Edge_Table;

} // namespace ccloutline
