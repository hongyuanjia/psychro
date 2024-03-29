% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dry_bulb.R
\name{tdb_from_h_w}
\alias{tdb_from_h_w}
\title{Calculate dry-bulb temperature of air}
\usage{
tdb_from_h_w(h, w)
}
\arguments{
\item{h}{A numeric vector of air enthalpy in J/(kg.K).}

\item{w}{A numeric vector of humidity ratio in kgWater/kgDryAir.}
}
\value{
A numeric vector of air temperature in Celsius.
}
\description{
Calculate dry-bulb temperature of air
}
\details{
`tdb_from_h_w()` calculates temperature of air from enthalpy (in
J/kg) and humidity ratio (in kgWater/kgDryAir).
}
\examples{
tdb_from_h_w()
}
