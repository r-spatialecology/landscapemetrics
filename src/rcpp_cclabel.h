#ifndef CCLABEL_H
#define CCLABEL_H

#include "Rcpp.h"
#define below data[(col + 1) * nrows + row]
#define above data[(col - 1) * nrows + row]
#define left data[col * nrows + row - 1]

using namespace Rcpp;

// [[Rcpp::export]]
void rcpp_ccl(IntegerMatrix data, int directions = 8);

struct Pixel_coords {
  Pixel_coords(unsigned row, unsigned col) : row(row), col(col){};
  bool operator==(const Pixel_coords &rhs) const {
    return row == rhs.row && col == rhs.col;
  }
  int row;
  int col;
};

class Ccl {
public:
  Ccl(IntegerMatrix mat, const int directions = 8);
  void ccl();

private:
  IntegerMatrix data;
  unsigned nrows;
  unsigned ncols;
  unsigned cur_label = 0;
  const unsigned directions;
  std::vector<Pixel_coords> neig;

  const int white_pixel_marked = -1;
  const int white_pixel_unmarked = NA_INTEGER;
  const int black_pixel = 1;
  unsigned internal_contour_tracing_start;
  unsigned external_contour_tracing_start;
  unsigned opposite_direction;
  unsigned init_search_pos_inc;
  unsigned labels = 1; // i.e. marked/labelled black pixels

  Pixel_coords tracer(const Pixel_coords start, unsigned &tracing_direction);
  void contour_tracing(const Pixel_coords start, unsigned tracing_direction);
};

#endif // CCLABEL_H
