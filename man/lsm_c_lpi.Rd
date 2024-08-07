% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsm_c_lpi.R
\name{lsm_c_lpi}
\alias{lsm_c_lpi}
\title{LPI (class level)}
\usage{
lsm_c_lpi(landscape, directions = 8)
}
\arguments{
\item{landscape}{A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.}

\item{directions}{The number of directions in which patches should be
connected: 4 (rook's case) or 8 (queen's case).}
}
\value{
tibble
}
\description{
Largest patch index (Area and Edge metric)
}
\details{
\deqn{LPI = \frac{\max \limits_{j = 1}^{n} (a_{ij})} {A} * 100}
where \eqn{max(a_{ij})} is the area of the patch in square meters and \eqn{A}
is the total landscape area in square meters.

The largest patch index is an 'Area and edge metric'. It is the percentage of the
landscape covered by the corresponding largest patch of each class i. It is a simple
measure of dominance.

Because the metric is based on distances or areas please make sure your data
is valid using \code{\link{check_landscape}}.

\subsection{Units}{Percentage}
\subsection{Range}{0 < LPI <= 100}
\subsection{Behaviour}{Approaches LPI = 0 when the largest patch is becoming small
and equals LPI = 100 when only one patch is present}
}
\examples{
landscape <- terra::rast(landscapemetrics::landscape)
lsm_c_lpi(landscape)

}
\references{
McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
Program for Categorical Maps. Computer software program produced by the authors;
available at the following web site: https://www.fragstats.org
}
\seealso{
\code{\link{lsm_p_area}},
\code{\link{lsm_l_ta}}, \cr
\code{\link{lsm_l_lpi}}
}
