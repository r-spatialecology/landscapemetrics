% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_lsm.R
\name{extract_lsm}
\alias{extract_lsm}
\title{extract_lsm}
\usage{
extract_lsm(
  landscape,
  y,
  extract_id = NULL,
  metric = NULL,
  name = NULL,
  type = NULL,
  what = NULL,
  directions = 8,
  progress = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{landscape}{A categorical raster object: SpatRaster; Raster* Layer, Stack, Brick; stars or a list of SpatRasters.}

\item{y}{Point geometry as SpatVector or sf object or 2-column matrix with coordinates.}

\item{extract_id}{Vector with id of sample points. If not provided, sample
points will be labelled 1...n.}

\item{metric}{Abbreviation of metrics (e.g. 'area').}

\item{name}{Full name of metrics (e.g. 'core area')}

\item{type}{Type according to FRAGSTATS grouping (e.g. 'aggregation metrics').}

\item{what}{Selected level of metrics: either "patch", "class" or "landscape".
It is also possible to specify functions as a vector of strings, e.g. \code{what = c("lsm_c_ca", "lsm_l_ta")}.}

\item{directions}{The number of directions in which patches should be
connected: 4 (rook's case) or 8 (queen's case).}

\item{progress}{Print progress report.}

\item{verbose}{Print warning messages.}

\item{...}{Arguments passed to \code{calculate_lsm()}.}
}
\value{
tibble
}
\description{
Extract metrics
}
\details{
This functions extracts the metrics of all patches the spatial object(s) \code{y}
(e.g. spatial points) are located within. Only patch level metrics are possible
to extract. Please be aware that the output is slightly different to all
other \code{lsm}-function of \code{landscapemetrics}. Returns a tibble with chosen
metrics and the ID of the spatial objects.
}
\examples{
landscape <- terra::rast(landscapemetrics::landscape)

points <- matrix(c(10, 5, 25, 15, 5, 25), ncol = 2, byrow = TRUE)
extract_lsm(landscape, y = points)
extract_lsm(landscape, y = points, type = "aggregation metric")

\dontrun{
# use lines

}

}
\seealso{
\code{\link{calculate_lsm}}
}
