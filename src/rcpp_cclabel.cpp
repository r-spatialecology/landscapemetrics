#include "rcpp_cclabel.h"
#include <queue>
#include <array>

// [[Rcpp::export]]
void rcpp_ccl(IntegerMatrix mat, int directions)
{
    const int nrows = mat.nrow();
    const int ncols = mat.ncol();
    std::vector<std::vector<int> > neigCoordinates;
    if (directions == 4) {
        neigCoordinates = {{-1,0},{1,0},
                           {0,1},{0,-1}};
        // the coordinates index:
        //    2
        //  0 X 1
        //    3
    } else {
        neigCoordinates = {{-1,0},{1,0},
                           {-1,1},{0,1},
                           {1,1},{-1,-1},
                           {0,-1},{1,-1}};
        // the coordinates index:
        //  2 3 4
        //  0 X 1
        //  5 6 7
    }

    const unsigned nNeig = neigCoordinates.size();

    // it's convinient to have patch cells marked as 0 (i.e. un-labeled) and matrix cells as NA
    for (int row = 0; row < nrows; row++) {
        for (int col = 0; col < ncols; col++) {
            if (mat[row * ncols + col] == NA) {
                continue;
            }
            mat[row * ncols + col] = 0;
        }
    }

    int label = 0;
    for (int row = 0; row < nrows; row++) {
        for (int col = 0; col < ncols; col++) {

            // ignore background cells and cells that are already labeled
            if (mat[row * ncols + col] == NA ||
                    mat[row * ncols + col] > 0) {
                continue;
            }

            // label the first cell of the patch
            mat[row * ncols + col] = ++label;

            std::queue<std::array<const int, 2> > patchcells;
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
                    if (mat[row_neig * ncols + col_neig] == NA ||
                            mat[row_neig * ncols + col_neig] > 0)
                        continue;

                    // label cell and put in queue
                    mat[row_neig * ncols + col_neig] = label;
                    patchcells.push(std::array<const int, 2>{col_neig, row_neig});
                }
            }
        }
    }
}
