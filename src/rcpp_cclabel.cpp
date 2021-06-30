#include "rcpp_cclabel.h"
#include <array>
#include <queue>

/**
 * @brief rcpp_ccl2 - the old variant
 * @param data
 * @param directions
 */
void rcpp_ccl2(IntegerMatrix data, int directions) {
  const int nrows = data.nrow();
  const int ncols = data.ncol();
  std::vector<std::vector<int>> neigCoordinates;
  if (directions == 4) {
    neigCoordinates = {{-1, 0}, {1, 0}, {0, 1}, {0, -1}};
    // the coordinates index:
    //    2
    //  0 X 1
    //    3
  } else {
    neigCoordinates = {{-1, 0}, {1, 0},   {-1, 1}, {0, 1},
                       {1, 1},  {-1, -1}, {0, -1}, {1, -1}};
    // the coordinates index:
    //  2 3 4
    //  0 X 1
    //  5 6 7
  }

  const unsigned nNeig = neigCoordinates.size();

  // it's convinient to have patch cells marked as 0 (i.e. un-labeled) and
  // matrix cells as NA
  for (int col = 0; col < ncols; col++) {
    for (int row = 0; row < nrows; row++) {
      if (data[col * nrows + row] == NA) {
        continue;
      }
      data[col * nrows + row] = 0;
    }
  }

  int label = 0;
  for (int col = 0; col < ncols; col++) {
    for (int row = 0; row < nrows; row++) {
      // ignore background cells and cells that are already labeled
      if (data[col * nrows + row] == NA || data[col * nrows + row] > 0) {
        continue;
      }

      // label the first cell of the patch
      data[col * nrows + row] = ++label;

      std::queue<std::array<const int, 2>> patchcells;
      patchcells.push(std::array<const int, 2>{col, row});

      while (!patchcells.empty()) {
        const auto col_row = patchcells.front();
        patchcells.pop();

        // check all neigbors
        for (unsigned i = 0; i < nNeig; i++) {
          const int col_neig = col_row[0] + neigCoordinates[i][0];
          const int row_neig = col_row[1] + neigCoordinates[i][1];

          // skip if not a patch or out of bounds
          if (col_neig < 0)
            continue;
          if (row_neig < 0)
            continue;
          if (col_neig >= ncols)
            continue;
          if (row_neig >= nrows)
            continue;

          // skip if background or already labeled
          if (data[col_neig * nrows + row_neig] == NA ||
              data[col_neig * nrows + row_neig] > 0)
            continue;

          // label cell and put in queue
          data[col_neig * nrows + row_neig] = label;
          patchcells.push(std::array<const int, 2>{col_neig, row_neig});
        }
      }
    }
  }
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
    // marked or unmarked white pixel? Or labelled already?
    if (data[row] != black_pixel) {
      continue;
    }
    // (1) If the pixel above is a white pixel, this pixel must be an external
    // contour of a new label <- there are all pixels "white" for the first row
    labels++;
    cur_label = labels;
    contour_tracing(Pixel_coords(row, col), external_contour_tracing_start);

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

  // first and last row are iterated seperately to avoid padding
  for (unsigned col = 1; col < ncols - 1; col++) {
    for (unsigned row = 0; row < nrows; row++) {
      // marked or unmarked white pixel? Or labelled already?
      if (data[col * nrows + row] != black_pixel) {
        continue;
      }
      // (1) If the pixel above is a white pixel (marked or unmarked), this
      // pixel must be an external contour of a new label
      if (above <= white_pixel_marked) {
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
          cur_label = data[col * nrows + row];
        }
        contour_tracing(Pixel_coords(row, col), internal_contour_tracing_start);
      }

      // (3) If this pixel is not a contour pixel (i.e. still an unlabeled black
      // pixel), the left neighbor must be a labelled pixel
      if (data[col * nrows + row] == black_pixel) {
        if (left <= black_pixel) { /// DEBUG
          Rcout << "col:" << col + 1 << " row:" << row + 1;
          return;
        }
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

// LANDMETRICS version

// global variables
static int SearchDirection[8][2] = {{0, 1},  {1, 1},   {1, 0},  {1, -1},
                                    {0, -1}, {-1, -1}, {-1, 0}, {-1, 1}};
int nrow, ncol;
int *out, *data;

/*
tdata is a matrix of binary data 0 for background and 1 for foreground
*/

void Tracer(int *cy, int *cx, int *tracingdirection) {
  int i, y, x, tval;
  for (i = 0; i < 7; i++) {
    y = *cy + SearchDirection[*tracingdirection][0];
    x = *cx + SearchDirection[*tracingdirection][1];

    if (y >= 0 && y < nrow && x >= 0 && x < ncol) {
      tval = data[y + nrow * x];
      if (tval == NA_INTEGER) {
        tval = 0;
      }
    } else {
      tval = 0;
    }

    if (tval == 0) {
      if (y >= 0 && y < nrow && x >= 0 && x < ncol) {
        out[y + nrow * x] = -1;
      }
      *tracingdirection = (*tracingdirection + 1) % 8;
    } else {
      *cy = y;
      *cx = x;
      break;
    }
  }
}

void ContourTracing(int cy, int cx, int labelindex, int tracingdirection) {
  char tracingstopflag = 0, SearchAgain = 1;
  int fx, fy, sx = cx, sy = cy;

  Tracer(&cy, &cx, &tracingdirection);

  if (cx != sx || cy != sy) {
    fx = cx;
    fy = cy;
    while (SearchAgain) {
      tracingdirection = (tracingdirection + 6) % 8;
      out[cy + nrow * cx] = labelindex;
      Tracer(&cy, &cx, &tracingdirection);

      if (cx == sx && cy == sy) {
        tracingstopflag = 1;
      } else if (tracingstopflag) {
        if (cx == fx && cy == fy) {
          SearchAgain = 0;
        } else {
          tracingstopflag = 0;
        }
      }
    }
  }
}

IntegerMatrix rcpp_ccl3(IntegerMatrix data) {
  auto out = clone(data);
  const auto nrow = out.nrow();
  const auto ncol = out.ncol();

  // cycle through and copy data to out
  int row, col;
  for (row = 0; row < nrow; row++) {
    for (col = 0; col < ncol; col++) {
      out[row + nrow * col] = 0;
    }
  }

  // cycle through the map and label the regions
  int tracingdirection, ConnectedComponentsCount = 0, labelindex = 0;
  for (row = 0; row < nrow; row++) {
    for (col = 0, labelindex = 0; col < ncol; col++) {
      if (data[row + nrow * col] == 1) { // black pixel
        if (labelindex != 0) {           // use pre-pixel label
          out[row + nrow * col] = labelindex;
        } else {
          labelindex = out[row + nrow * col];
          if (labelindex == 0) {
            labelindex = ++ConnectedComponentsCount;
            tracingdirection = 0;
            ContourTracing(row, col, labelindex,
                           tracingdirection); // external contour
            out[row + nrow * col] = labelindex;
          }
        }
      } else if (labelindex != 0) { // white pixel & pre-pixel has been labeled
        if (out[row + nrow * col] == 0) {
          tracingdirection = 1;
          ContourTracing(row, col - 1, labelindex,
                         tracingdirection); // internal contour
        }
        labelindex = 0;
      }
    }
  }

  // cycle through and replace -1 with 0 and insert NA where appropriate
  for (row = 0; row < nrow; row++) {
    for (col = 0; col < ncol; col++) {
      if (out[row + nrow * col] == -1) {
        out[row + nrow * col] = 0;
      }
    }
  }

  return (out);
}

#if 0
#include <R.h>
#include <Rinternals.h>

SEXP ccl(SEXP tdata) {
    // define the pointers for the data
    PROTECT(tdata = coerceVector(tdata, INTSXP));
    data = INTEGER(tdata); // this is a binary matrix of data
    int *dims =
            INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol),
                                 INTSXP)); // get the dimension of the input matrix
    nrow = dims[0];
    ncol = dims[1]; // assign the number of rows and columns in the matrix

    // setup the output matrix
    SEXP ans;
    PROTECT(ans = allocMatrix(INTSXP, nrow, ncol));
    out = INTEGER(ans); // pointer to output dataset

    // cycle through and copy data to out
    int row, col;
    for (row = 0; row < nrow; row++) {
        for (col = 0; col < ncol; col++) {
            out[row + nrow * col] = 0;
        }
    }

    // cycle through the map and label the regions
    int tracingdirection, ConnectedComponentsCount = 0, labelindex = 0;
    for (row = 0; row < nrow; row++) {
        for (col = 0, labelindex = 0; col < ncol; col++) {
            if (data[row + nrow * col] == 1) { // black pixel
                if (labelindex != 0) {           // use pre-pixel label
                    out[row + nrow * col] = labelindex;
                } else {
                    labelindex = out[row + nrow * col];
                    if (labelindex == 0) {
                        labelindex = ++ConnectedComponentsCount;
                        tracingdirection = 0;
                        ContourTracing(row, col, labelindex,
                                       tracingdirection); // external contour
                        out[row + nrow * col] = labelindex;
                    }
                }
            } else if (labelindex != 0) { // white pixel & pre-pixel has been labeled
                if (out[row + nrow * col] == 0) {
                    tracingdirection = 1;
                    ContourTracing(row, col - 1, labelindex,
                                   tracingdirection); // internal contour
                }
                labelindex = 0;
            }
        }
    }

    // cycle through and replace -1 with 0 and insert NA where appropriate
    for (row = 0; row < nrow; row++) {
        for (col = 0; col < ncol; col++) {
            if (data[row + nrow * col] == NA_INTEGER) {
                out[row + nrow * col] = NA_INTEGER;
            } else if (out[row + nrow * col] == -1) {
                out[row + nrow * col] = 0;
            }
        }
    }

    // return the output data
    UNPROTECT(2);
    return (ans);
}
#endif

void rcpp_ccl(IntegerMatrix data, int directions) {
  Ccl ccl(data, directions);
  ccl.ccl();
}
