#include "rcpp_get_boundaries.h"

IntegerMatrix rcpp_get_boundaries(const IntegerMatrix &xx,
                                  int directions) {

    IntegerMatrix x = add_padding(xx);

    if (directions == 4)
        get_boundaries_4(x);
    else
        get_boundaries_8(x);

    return rm_padding(x);
}

IntegerMatrix add_padding(const IntegerMatrix &xx)
{
    const unsigned ncol_orig = xx.ncol();
    const unsigned nrow_orig = xx.nrow();
    const unsigned ncol_pad = xx.ncol() + 2;
    const unsigned nrow_pad = xx.nrow() + 2;
    const long ncell_pad = ncol_pad * nrow_pad;
    const long ncell_orig = ncol_orig * nrow_orig;

    IntegerMatrix x = no_init_matrix(nrow_pad, ncol_pad);

    // corners
    x[0] = xx[0];
    x[nrow_pad - 1] = xx[nrow_orig - 1];
    x[nrow_pad * (ncol_pad - 1)] = xx[nrow_orig * (ncol_orig - 1)];
    x[ncell_pad - 1] = xx[ncell_orig - 1];

    // padding the cols
    for (unsigned i = 1; i < ncol_pad - 1; i++) {
        x[i * nrow_pad] = xx[(i - 1) * nrow_orig];
        x[i * nrow_pad + (nrow_pad - 1)] = xx[(i - 1) * nrow_orig + (nrow_orig - 1)];
        for (unsigned j = 1; j < nrow_orig + 1; j++) {
            x[i * nrow_pad + j] = xx[(i - 1) * nrow_orig + (j - 1)];
        }
    }

    // padding the rows
    for (unsigned i = 1; i < nrow_pad - 1; i++) {
        x[i] = x[i + nrow_pad];
        x[i + nrow_pad * (ncol_pad - 1)] = x[i + nrow_pad * (ncol_pad - 2)];
    }

    return x;
}

IntegerMatrix rm_padding(const IntegerMatrix &xx)
{
    const unsigned ncol_orig = xx.ncol() - 2;
    const unsigned nrow_orig = xx.nrow() - 2;
    const unsigned nrow_pad = xx.nrow();

    IntegerMatrix x = no_init_matrix(nrow_orig, ncol_orig);

    for (unsigned col = 0; col < ncol_orig; col++) {
        for (unsigned row = 0; row < nrow_orig; row++) {
            x[col * nrow_orig + row] = xx[(col + 1) * nrow_pad + (row + 1)];
        }
    }
    return x;
}

void get_boundaries_4(IntegerMatrix &x)
{
    const int na = NA_INTEGER;
    const unsigned core = 0;
    const unsigned boundary = 1;
    const unsigned ncol_pad = x.ncol();
    const unsigned nrow_pad = x.nrow();

    // iterate from cell ncols + 1 to ncells - (ncols + 1) because the landscape is padded
    for (long cell = nrow_pad + 1; cell < (nrow_pad * (ncol_pad - 1) - 1); cell++) {
        if (x[cell] == na)
            continue;
        // check the neighbors above and below
        // first because they are probably cached
        if (x[cell] != x[cell + 1]) {
            x[cell] = boundary;
            continue;
        }
        if (x[cell - 1] == na) {
            x[cell] = boundary;
            continue;
        }
        // the cell left
        if (x[cell - nrow_pad] == na) {
            x[cell] = boundary;
            continue;
        }
        // the cell right
        if (x[cell + nrow_pad] == na) {
            x[cell] = boundary;
            continue;
        }
        x[cell] = core;
    }
}

void get_boundaries_8(IntegerMatrix &x)
{
    const int na = NA_INTEGER;
    const unsigned core = 0;
    const unsigned boundary = 1;
    const unsigned ncol_pad = x.ncol();
    const unsigned nrow_pad = x.nrow();

    // iterate from cell ncols + 1 to ncells - (ncols + 1) because the landscape is padded
    for (long cell = nrow_pad + 1; cell < (nrow_pad * (ncol_pad - 1) - 1); cell++) {
        if (x[cell] == na)
            continue;
        // check the neighbors above and below
        // first because they are probably cached
        if (x[cell] != x[cell + 1]) {
            x[cell] = boundary;
            continue;
        }
        if (x[cell - 1] == na) {
            x[cell] = boundary;
            continue;
        }
        // the cell top - left
        if (x[cell - (nrow_pad + 1)] == na) {
            x[cell] = boundary;
            continue;
        }
        // the cell bottom - left
        if (x[cell - (nrow_pad - 1)] == na) {
            x[cell] = boundary;
            continue;
        }
        // the cell top - right
        if (x[cell + (nrow_pad - 1)] == na) {
            x[cell] = boundary;
            continue;
        }
        // the cell bottm - right
        if (x[cell + (nrow_pad + 1)] == na) {
            x[cell] = boundary;
            continue;
        }
        // the cell left
        if (x[cell - nrow_pad] == na) {
            x[cell] = boundary;
            continue;
        }
        // the cell right
        if (x[cell + nrow_pad] == na) {
            x[cell] = boundary;
            continue;
        }
        x[cell] = core;
    }
}
