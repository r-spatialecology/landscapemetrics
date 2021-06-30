#include "rcpp_cclabel.h"

void rcpp_ccl(IntegerMatrix data, int directions) {
  Ccl ccl(data, directions);
  ccl.ccl();
}

Ccl::Ccl(IntegerMatrix mat, const int directions) : directions(directions) {
  nrows = mat.nrow();
  ncols = mat.ncol();
  data = mat;
  if (directions == 4) {
    neig = {Pixel_coords(1, 0), Pixel_coords(0, 1), Pixel_coords(-1, 0),
            Pixel_coords(0, -1)};
    internal_contour_tracing_start = 2;
    external_contour_tracing_start = 0;
    opposite_direction = 2;
    init_search_pos_inc = 1;
    // the coordinates index:
    //    3
    //  2 X 0
    //    1
  } else {
    neig = {Pixel_coords(1, 0),  Pixel_coords(1, 1),  Pixel_coords(0, 1),
            Pixel_coords(-1, 1), Pixel_coords(-1, 0), Pixel_coords(-1, -1),
            Pixel_coords(0, -1), Pixel_coords(1, -1)};
    internal_contour_tracing_start = 3;
    external_contour_tracing_start = 7;
    opposite_direction = 4;
    init_search_pos_inc = 2;
    // the coordinates index:
    //  5 6 7
    //  4 X 0
    //  3 2 1
  }
}

/**
 * @brief Ccl::ccl
 * The Algorithm is based on:
 * Chang, Fu, Chun Jen Chen, and Chi Jen Lu. 2004.
 * “A Linear-Time Component-Labeling Algorithm Using Contour Tracing Technique.”
 * Computer Vision and Image Understanding 93 (2): 206–20.
 * https://doi.org/10.1016/j.cviu.2003.09.002.
 */
void Ccl::ccl() {

  // run first row seperately to avoid padding
  for (unsigned row = 0; row < nrows; row++) {
    const unsigned col = 0;
    // marked or unmarked white pixel?
    if (data[row] < black_pixel) {
      continue;
    }
    // (1) If the pixel above is a white pixel, this pixel must be an external
    // contour of a new label <- there are all pixels "white" for the first row
    if (data[row] == black_pixel) {
      labels++;
      cur_label = labels;
      contour_tracing(Pixel_coords(row, col), external_contour_tracing_start);
    }

    // (2) If the pixel below is an unmarked white pixel, this pixel must be a
    // new internal countour If this pixel is labelled already, it is also an
    // external countour
    if (below == white_pixel_unmarked) {
      if (data[col * nrows + row] == black_pixel) {
        cur_label = left;
      } else {
        cur_label = data[col * nrows + row]; // not sure if this is needed
      }
      contour_tracing(Pixel_coords(row, col), internal_contour_tracing_start);
    }
  }

  // first and last row are iterated seperately to avoid the need for padding
  for (unsigned col = 1; col < ncols - 1; col++) {
    for (unsigned row = 0; row < nrows; row++) {

      // marked or unmarked white pixel?
      if (data[col * nrows + row] < black_pixel) {
        continue;
      }

      // (1) If the pixel is a black pixel and above is a white pixel (marked or
      // unmarked), this pixel must be an external contour of a new label
      if (data[col * nrows + row] == black_pixel) {
        if (above <= white_pixel_marked) {
          labels++;
          cur_label = labels;
          contour_tracing(Pixel_coords(row, col),
                          external_contour_tracing_start);
        }
      }

      // (2) If the pixel below is an unmarked white pixel, this pixel must be a
      // new internal countour If this pixel is labelled already, it is also an
      // external countour
      if (below == white_pixel_unmarked) {
        if (data[col * nrows + row] == black_pixel) {
          cur_label = left;
        } else {
          cur_label = data[col * nrows + row];
        }
        contour_tracing(Pixel_coords(row, col), internal_contour_tracing_start);
      }

      // (3) If this pixel is not a contour pixel (i.e. still an unlabeled black
      // pixel), the left neighbor must be a labelled pixel
      if (data[col * nrows + row] == black_pixel) {
        data[col * nrows + row] = left;
      }
    }
  }

  // black pixels in last row are either labelled already or isolated new
  // patches
  for (unsigned row = 0; row < nrows; row++) {
    const unsigned col = ncols - 1;
    // marked or unmarked white pixel? Or labelled already?
    if (data[col * nrows + row] != black_pixel) {
      continue;
    }

    // has left black pixel? Or new label?
    if (static_cast<int>(col) - 1 >= 0) {
      if (left >= black_pixel) {
        cur_label = data[col * nrows + row - 1];
      } else {
        labels++;
        cur_label = labels;
      }
    } else {
      labels++;
      cur_label = labels;
    }
    data[col * nrows + row] = cur_label;
  }

  // Just re-label stuff to have labels [1,n] instead of [2, n+1]
  for (unsigned col = 0; col < ncols; col++) {
    for (unsigned row = 0; row < nrows; row++) {
      if (data[col * nrows + row] == white_pixel_unmarked) {
        continue;
      }
      if (data[col * nrows + row] == white_pixel_marked) {
        data[col * nrows + row] = white_pixel_unmarked;
        continue;
      }
      data[col * nrows + row]--;
    }
  }
}

void Ccl::contour_tracing(const Pixel_coords start,
                          unsigned tracing_direction) {

  data[start.col * nrows + start.row] = cur_label;
  const auto second = tracer(start, tracing_direction);

  // is it an isolated pixel?
  if (start == second) {
    return;
  }

  data[second.col * nrows + second.row] = cur_label;
  auto t = tracer(second, tracing_direction);
  while (true) {
    // stop criterion: tracing returns to start pixel and tracing continues to
    // second traced pixel for the second time
    if (t == start) {
      t = tracer(t, tracing_direction);
      if (t == second) {
        return;
      }
    }
    data[t.col * nrows + t.row] = cur_label;
    t = tracer(t, tracing_direction);
  }
}

Pixel_coords Ccl::tracer(const Pixel_coords start,
                         unsigned &tracing_direction) {
  for (unsigned i = 0; i < directions; i++) {
    const int row = start.row + neig[tracing_direction].row;
    const int col = start.col + neig[tracing_direction].col;

    if (row >= 0 && row < nrows && col >= 0 && col < ncols) {
      const auto val = data[col * nrows + row];
      if (val >= black_pixel) {
        tracing_direction =
            (tracing_direction + opposite_direction + init_search_pos_inc) %
            directions;
        return (Pixel_coords(row, col));
      }

      data[col * nrows + row] = white_pixel_marked;
    }
    tracing_direction = (tracing_direction + 1) % directions;
  }
  return start;
}
