% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zmb_polygons.R
\name{zmb_polygons}
\alias{zmb_polygons}
\title{Fetch Zambia Polygons}
\usage{
zmb_polygons(shpdata = shpdata)
}
\arguments{
\item{shpdata}{path where the VcPepfarPolygons live}
}
\value{
list of spatial data frames
}
\description{
Fetch Zambia Polygons
}
\examples{
\dontrun{
shpdata <- glamr::si_path("path_vector")
zmb_polygons(shpdata)
}

}
