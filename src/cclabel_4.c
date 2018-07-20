/*
 this code was is a 1 pass implementation of connected components labelling

 based on
 Chang, F., C.-J. Chen, and C.-J. Lu. 2004. A linear-time component-labeling
 algorithm using contour tracing technique. Comput. Vis.
 Image Underst. 93:206-220.

@author Jeremy VanDerWal \email{jjvanderwal@@gmail.com}
VanDerWal, J., Falconi, L., Januchowski, S., Shoo, L., and Storlie, C. 2014.
SDMTools: Species Distribution Modelling Tools: Tools for processing data
associated with species distribution modelling exercises.
R package version 1.1-221. <https://CRAN.R-project.org/package=SDMTools>

 */
#include <R.h>
#include <Rinternals.h>

//global variables
static int SearchDirection_4[4][2] = {{0,1},{1,0},{0,-1},{-1,0}};
int nrow, ncol;
int *out, *data;
SEXP ans;

/*
 tdata is a matrix of binary data 0 for background and 1 for foreground
 */

void Tracer_4(int *cy, int *cx, int *tracingdirection)
{
    int i, y, x, tval;
    for(i = 0; i < 3; i++)	{
        y = *cy + SearchDirection_4[*tracingdirection][0];
        x = *cx + SearchDirection_4[*tracingdirection][1];

        if (y>=0 && y<nrow && x>=0 && x<ncol)	{
            tval = data[y+nrow*x];
            if (tval==NA_INTEGER)	{tval=0;}
        } else	{
            tval = 0;
        }

        if(tval == 0)	{
            if (y>=0 && y<nrow && x>=0 && x<ncol)	{out[y+nrow*x] = -1;}
            *tracingdirection = (*tracingdirection + 1) % 4;
        } else	{
            *cy = y;
            *cx = x;
            break;
        }
    }
}

void ContourTracing_4(int cy, int cx, int labelindex, int tracingdirection)
{
    char tracingstopflag = 0, SearchAgain = 1;
    int fx, fy, sx = cx, sy = cy;

    Tracer_4(&cy, &cx, &tracingdirection);

    if(cx != sx || cy != sy)	{
        fx = cx;
        fy = cy;
        while(SearchAgain)	{
            tracingdirection = (tracingdirection + 3) % 4;
            out[cy+nrow*cx] = labelindex;
            Tracer_4(&cy, &cx, &tracingdirection);

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

SEXP ccl_4(SEXP tdata)
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
                        ContourTracing_4(row, col, labelindex, tracingdirection);// external contour
                        out[row+nrow*col] = labelindex;
                    }
                }
            } else if(labelindex != 0)	{// white pixel & pre-pixel has been labeled
                if(out[row+nrow*col] == 0)	{
                    tracingdirection = 1;
                    ContourTracing_4(row, col - 1, labelindex, tracingdirection);// internal contour
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
