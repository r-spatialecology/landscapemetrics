#include <Rcpp.h>
#include <vector>
#include <set>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix rcpp_ccl_multiclass(IntegerMatrix mat, int directions)
{
    const int nrows = mat.nrow();
    const int ncols = mat.ncol();

    static const int neig4[4][2] = {
        {-1,0},{1,0},{0,1},{0,-1}
    };

    static const int neig8[8][2] = {
        {-1,0},{1,0},
        {-1,1},{0,1},{1,1},
        {-1,-1},{0,-1},{1,-1}
    };

    const int (*neig)[2];
    int nNeig;

    if (directions == 4) {
        neig = neig4;
        nNeig = 4;
    } else {
        neig = neig8;
        nNeig = 8;
    }

    IntegerMatrix out(nrows, ncols);

    // Use a sorted set to process classes in ascending order
    std::set<int> classes;
    for(int i = 0; i < nrows * ncols; i++){
        if(mat[i] != NA_INTEGER) classes.insert(mat[i]);
    }

    int label_counter = 0;  // global counter across classes

    // Process each class independently, in ascending order
    for(int class_value : classes){

        for (int col = 0; col < ncols; col++) {
            for (int row = 0; row < nrows; row++) {

                int idx = col * nrows + row;

                if(mat[idx] != class_value)
                    continue;

                if(out[idx] != 0)
                    continue;

                out[idx] = ++label_counter;

                std::vector<std::pair<int,int>> stack;
                stack.emplace_back(col, row);

                while(!stack.empty()){
                    auto [cc, rr] = stack.back();
                    stack.pop_back();

                    for(int i = 0; i < nNeig; i++){
                        int col_neig = cc + neig[i][0];
                        int row_neig = rr + neig[i][1];

                        if(col_neig < 0 || row_neig < 0 || col_neig >= ncols || row_neig >= nrows)
                            continue;

                        int nidx = col_neig * nrows + row_neig;

                        if(mat[nidx] != class_value)
                            continue;

                        if(out[nidx] != 0)
                            continue;

                        out[nidx] = label_counter;
                        stack.emplace_back(col_neig, row_neig);
                    }
                }
            }
        }
    }

    // Preserve NA_INTEGERs
    for(int i = 0; i < nrows * ncols; i++){
        if(mat[i] == NA_INTEGER) out[i] = NA_INTEGER;
    }

    return out;
}
