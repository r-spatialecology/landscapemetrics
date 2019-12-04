/*
    this code was is a 1 pass implementation of connected components labelling

based on
Chang, F., C.-J. Chen, and C.-J. Lu. 2004. A linear-time component-labeling
 algorithm using contour tracing technique. Comput.
 Vis. Image Underst. 93:206-220.

@author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}
VanDerWal, J., Falconi, L., Januchowski, S., Shoo, L., and Storlie, C. 2014.
SDMTools: Species Distribution Modelling Tools: Tools for processing data
associated with species distribution modelling exercises.
R package version 1.1-221. <https://CRAN.R-project.org/package=SDMTools>

*/
#include <R.h>
#include <Rinternals.h>

void Tracer_8(int *out, const int *data, int *cy, int *cx, int *tracingdirection,
              const int nrow, const int ncol)
{
    const int SearchDirection_8[8][2] = {{0,1},{1,1},{1,0},{1,-1},{0,-1},{-1,-1},
                                         {-1,0},{-1,1}};

    int i, y, x, tval;
    for(i = 0; i < 7; i++)	{
        y = *cy + SearchDirection_8[*tracingdirection][0];
        x = *cx + SearchDirection_8[*tracingdirection][1];

        if (y>=0 && y<nrow && x>=0 && x<ncol)	{
            tval = data[y+nrow*x];
            if (tval==NA_INTEGER)	{tval=0;}
        } else	{
            tval = 0;
        }

        if(tval == 0)	{
            if (y>=0 && y<nrow && x>=0 && x<ncol)	{out[y+nrow*x] = -1;}
            *tracingdirection = (*tracingdirection + 1) % 8;
        } else	{
            *cy = y;
            *cx = x;
            break;
        }
    }
}

void ContourTracing_8(int *out, const int *data, int cy, int cx, int labelindex, int tracingdirection,
                      const int nrow, const int ncol)
{
    char tracingstopflag = 0, SearchAgain = 1;
    int fx, fy, sx = cx, sy = cy;

    Tracer_8(out, data, &cy, &cx, &tracingdirection, nrow, ncol);

    if(cx != sx || cy != sy)	{
        fx = cx;
        fy = cy;
        while(SearchAgain)	{
            tracingdirection = (tracingdirection + 6) % 8;
            out[cy+nrow*cx] = labelindex;
            Tracer_8(out, data, &cy, &cx, &tracingdirection, nrow, ncol);

            if(cx == sx && cy == sy)	{
                tracingstopflag = 1;
            } else if(tracingstopflag)	{
                if(cx == fx && cy == fy)	{
                    SearchAgain = 0;
                } else	{
                    tracingstopflag = 0;
                }
            }
        }
    }
}

SEXP ccl_8(SEXP tdata)
{
    //define the pointers for the data
    PROTECT(tdata = coerceVector(tdata, INTSXP));
    const int* restrict data = INTEGER(tdata); //this is a binary matrix of data
    const int* restrict dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    const int nrow = dims[0];
    const int ncol = dims[1]; //assign the number of rows and columns in the matrix

    //setup the output matrix
    SEXP ans;

    PROTECT(ans = allocMatrix(INTSXP, nrow, ncol));
    int* restrict out = INTEGER(ans); //pointer to output dataset

    //cycle through and copy data to out
    for (int col=0; col<ncol; col++)	{
        for (int row=0; row<nrow; row++)	{
            out[row+nrow*col]=0;
        }
    }

    //cycle through the map and label the regions
    int ConnectedComponentsCount = 0;
    for (int row=0; row<nrow; row++)	{
        int labelindex = 0;
        for (int col=0; col<ncol; col++)	{
            if(data[row+nrow*col]==1)	{// black pixel
                if(labelindex != 0) {// use pre-pixel label
                    out[row+nrow*col] = labelindex;
                } else	{
                    labelindex = out[row+nrow*col];
                    if(labelindex == 0)	{
                        labelindex = ++ConnectedComponentsCount;
                        const int tracingdirection = 0;
                        ContourTracing_8(out, data, row, col, labelindex, tracingdirection,
                                         nrow, ncol);// external contour
                        out[row+nrow*col] = labelindex;
                    }
                }
            } else if(labelindex != 0)	{// white pixel & pre-pixel has been labeled
                if(out[row+nrow*col] == 0)	{
                    const int tracingdirection = 1;
                    ContourTracing_8(out, data, row, col - 1, labelindex, tracingdirection,
                                     nrow, ncol);// internal contour
                }
                labelindex = 0;
            }
        }
    }

    //cycle through and replace -1 with 0 and insert NA where appropriate
    for (int col=0; col<ncol; col++)	{
        for (int row=0; row<nrow; row++)	{
            if (data[row+nrow*col]==NA_INTEGER)	{
                out[row+nrow*col]=NA_INTEGER;
            } else if (out[row+nrow*col]==-1)	{
                out[row+nrow*col]=0;
            }
        }
    }


    //return the output data
    UNPROTECT(2);
    return(ans);
}


//////////////////////////////////////////////////////////////

static int SearchDirection_8[8][2] = {{0,1},{1,1},{1,0},{1,-1},{0,-1},{-1,-1},
                                      {-1,0},{-1,1}};
int nrow, ncol;
int *out, *data;
SEXP ans;

/*
    tdata is a matrix of binary data 0 for background and 1 for foreground
*/

void Tracer_8_old(int *cy, int *cx, int *tracingdirection)
{
    int i, y, x, tval;
    for(i = 0; i < 7; i++)	{
        y = *cy + SearchDirection_8[*tracingdirection][0];
        x = *cx + SearchDirection_8[*tracingdirection][1];

        if (y>=0 && y<nrow && x>=0 && x<ncol)	{
            tval = data[y+nrow*x];
            if (tval==NA_INTEGER)	{tval=0;}
        } else	{
            tval = 0;
        }

        if(tval == 0)	{
            if (y>=0 && y<nrow && x>=0 && x<ncol)	{out[y+nrow*x] = -1;}
            *tracingdirection = (*tracingdirection + 1) % 8;
        } else	{
            *cy = y;
            *cx = x;
            break;
        }
    }
}

void ContourTracing_8_old(int cy, int cx, int labelindex, int tracingdirection)
{
    char tracingstopflag = 0, SearchAgain = 1;
    int fx, fy, sx = cx, sy = cy;

    Tracer_8_old(&cy, &cx, &tracingdirection);

    if(cx != sx || cy != sy)	{
        fx = cx;
        fy = cy;
        while(SearchAgain)	{
            tracingdirection = (tracingdirection + 6) % 8;
            out[cy+nrow*cx] = labelindex;
            Tracer_8_old(&cy, &cx, &tracingdirection);

            if(cx == sx && cy == sy)	{
                tracingstopflag = 1;
            } else if(tracingstopflag)	{
                if(cx == fx && cy == fy)	{
                    SearchAgain = 0;
                } else	{
                    tracingstopflag = 0;
                }
            }
        }
    }
}

SEXP ccl_8_old(SEXP tdata)
{
    //define the pointers for the data
    PROTECT(tdata = coerceVector(tdata, INTSXP));
    data = INTEGER(tdata); //this is a binary matrix of data
    int *dims = INTEGER(coerceVector(getAttrib(tdata, R_DimSymbol), INTSXP)); //get the dimension of the input matrix
    nrow = dims[0]; ncol = dims[1]; //assign the number of rows and columns in the matrix

    //setup the output matrix
    PROTECT(ans = allocMatrix(INTSXP, nrow, ncol));
    out = INTEGER(ans); //pointer to output dataset

    //cycle through and copy data to out
    int row, col;
    for (row=0; row<nrow; row++)	{
        for (col=0; col<ncol; col++)	{
            out[row+nrow*col]=0;
        }
    }

    //cycle through the map and label the regions
    int tracingdirection, ConnectedComponentsCount = 0, labelindex = 0;
    for (row=0; row<nrow; row++)	{
        for (col=0,labelindex=0; col<ncol; col++)	{
            if(data[row+nrow*col]==1)	{// black pixel
                if(labelindex != 0) {// use pre-pixel label
                    out[row+nrow*col] = labelindex;
                } else	{
                    labelindex = out[row+nrow*col];
                    if(labelindex == 0)	{
                        labelindex = ++ConnectedComponentsCount;
                        tracingdirection = 0;
                        ContourTracing_8_old(row, col, labelindex, tracingdirection);// external contour
                        out[row+nrow*col] = labelindex;
                    }
                }
            } else if(labelindex != 0)	{// white pixel & pre-pixel has been labeled
                if(out[row+nrow*col] == 0)	{
                    tracingdirection = 1;
                    ContourTracing_8_old(row, col - 1, labelindex, tracingdirection);// internal contour
                }
                labelindex = 0;
            }
        }
    }

    //cycle through and replace -1 with 0 and insert NA where appropriate
    for (row=0; row<nrow; row++)	{
        for (col=0; col<ncol; col++)	{
            if (data[row+nrow*col]==NA_INTEGER)	{
                out[row+nrow*col]=NA_INTEGER;
            } else if (out[row+nrow*col]==-1)	{
                out[row+nrow*col]=0;
            }
        }
    }


    //return the output data
    UNPROTECT(2);
    return(ans);

}
