% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsm_l_shei.R
\name{lsm_l_shei}
\alias{lsm_l_shei}
\title{SHEI (landscape level)}
\usage{
lsm_l_shei(landscape)
}
\arguments{
\item{landscape}{A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.}
}
\value{
tibble
}
\description{
Shannons's evenness index (Diversity metric)
}
\details{
\deqn{SHEI = \frac{- \sum \limits_{i = 1} ^ {m} (P_{i} * \ln P_{i})} {\ln m}}
where \eqn{P_{i}} is the proportion of class i and \eqn{m} is the
number of classes.

SHEI is a 'Diversity metric'. It is the ratio between the actual Shannon's diversity index
and and the theoretical maximum of the Shannon diversity index. It can be understood as a
measure of dominance.

\subsection{Units}{None}
\subsection{Range}{0 <= SHEI < 1}
\subsection{Behaviour}{Equals SHEI = 0 when only one patch  present and equals SHEI = 1
when the proportion of classes is completely equally distributed}
}
\examples{
landscape <- terra::rast(landscapemetrics::landscape)
lsm_l_shei(landscape)

}
\references{
McGarigal K., SA Cushman, and E Ene. 2023. FRAGSTATS v4: Spatial Pattern Analysis
Program for Categorical Maps. Computer software program produced by the authors;
available at the following web site: https://www.fragstats.org

Shannon, C., and W. Weaver. 1949. The mathematical theory of
communication. Univ. IllinoisPress, Urbana
}
\seealso{
\code{\link{lsm_c_pland}},
\code{\link{lsm_l_pr}}
}
