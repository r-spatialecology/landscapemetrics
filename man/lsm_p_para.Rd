% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsm_p_para.R
\name{lsm_p_para}
\alias{lsm_p_para}
\title{PARA (patch level)}
\usage{
lsm_p_para(landscape, directions = 8)
}
\arguments{
\item{landscape}{A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters}

\item{directions}{The number of directions in which patches should be
connected: 4 (rook's case) or 8 (queen's case).}
}
\value{
tibble
}
\description{
Perimeter-Area ratio (Shape metric)
}
\details{
\deqn{PARA = \frac{p_{ij}} {a_{ij}}}
where \eqn{p_{ij}} is the perimeter in meters and \eqn{a_{ij}} is the
area in square meters.

PARA is a 'Shape metric'. It describes the patch complexity in a
straightforward way. However, because it is not standarised to a certain
shape (e.g. a square), it is not scale independent, meaning that increasing
the patch size while not changing the patch form will change the ratio.

Because the metric is based on distances or areas please make sure your data
is valid using \code{\link{check_landscape}}.

\subsection{Units}{None}
\subsection{Range}{PARA > 0}
\subsection{Behaviour}{Increases, without limit, as the shape complexity
increases.}
}
\examples{
landscape <- terra::rast(landscapemetrics::landscape)
lsm_p_para(landscape)

}
\references{
McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
Program for Categorical Maps. Computer software program produced by the authors;
available at the following web site: https://www.fragstats.org
}
\seealso{
\code{\link{lsm_p_area}},
\code{\link{lsm_p_perim}}, \cr
\code{\link{lsm_c_para_mn}},
\code{\link{lsm_c_para_sd}},
\code{\link{lsm_c_para_cv}}, \cr
\code{\link{lsm_l_para_mn}},
\code{\link{lsm_l_para_sd}},
\code{\link{lsm_l_para_cv}}
}
