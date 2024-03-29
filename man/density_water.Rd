% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/density.R
\name{rhoh2o}
\alias{rhoh2o}
\title{Calculate density of water}
\usage{
rhoh2o(tdb)
}
\arguments{
\item{tdb}{A numeric vector of dry-bulb temperature in C.}
}
\value{
A numeric vector of water density in kg/m3.
}
\description{
Calculate density of water
}
\details{
`rhoh2o()` calculates density of water from temperature (in Celsius).
}
\examples{
rhoh2o(15)
}
