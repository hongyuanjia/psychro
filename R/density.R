#' Calculate air density
#'
#' @details
#'
#' `rhoair_from_tdb_w_pb()` calculates air density from dry-bulb temperature (in
#' Celsius), humidity ratio (in kgWater/kgDryAir) and barometric pressure (in
#' Pascals).
#'
#' @param tdb A numeric vector of dry-bulb temperature in C.
#' @param w A numeric vector of humidity ratio in kgWater/kgDryAir.
#' @param pb A numeric vector of barometric pressure in Pa.
#' @return A numeric vector of air density (in kg/m3)
#' @references `PsyRhoAirFnPbTdbW()` in EnergyPlus `Psychrometrics` module.
#' @export
#' @rdname density_air
#' @examples
#' rhoair_from_tdb_w_pb(1, 2, 3)
# rhoair_from_tdb_w_pb {{{
rhoair_from_tdb_w_pb <- function (tdb, w, pb) {
    rho <- pb / (GasConst * (tdb + KelvinConv) * (1.0 + AirMolarMass/WaterMolarMass * clean_w(w)))

    check_abnormal(rho, rho < 0)
}
# }}}

#' Calculate vapor density in air
#'
#' @details
#'
#' `rhov_from_tdb_rh_lbnd0c()` calculates the vapor density in air from dry-bulb
#' temperature (in Celsius) and relative humidity (in 0.0 - 1.0). The name
#' signifies derivation and temperatures were used with 0C as minimum.
#'
#' `rhov_from_tdb_w_pb()` calculates vapor density in air from dry-bulb
#' temperature (in Celsius), humidity ratio (in kgWater/kgDryAir) and barometric
#' pressure (in Pascals).
#'
#' `rhov_from_tdb_rh()` calculates vapor density in air from dry-bulb
#' temperature (in Celcius) and relative humidity (in [0.0 - 1.0]).
#'
#' @param rh A numeric vector relative humidity in [0.0, 1.0].
#' @inheritParams rhoair_from_tdb_w_pb
#' @inheritParams rhov_from_tdb_rh_lbnd0c
#' @return A numeric vector of vapor density in air in kg/m3.
#' @export
#' @rdname density_vapor
#' @examples
#' rhov_from_tdb_rh_lbnd0c(13, 0.1)
# rhov_from_tdb_rh_lbnd0c {{{
rhov_from_tdb_rh_lbnd0c <- function (tdb, rh) {
    rh / (461.52 * (tdb + KelvinConv)) * exp(23.7093 - 4111.0 / ((tdb + KelvinConv) - 35.45))
}
# }}}

#' @rdname density_vapor
#' @export
#' @examples
#' rhov_from_tdb_w_pb(13, 0.002, 130000)
# rhov_from_tdb_w_pb {{{
rhov_from_tdb_w_pb <- function (tdb, w, pb) {
    w <- clean_w(w)
    w * pb / (VaporConst * (tdb + KelvinConv) * (w + WaterSpecHeat/AirMolarMass))
}
# }}}

#' @rdname density_vapor
#' @export
#' @examples
#' rhov_from_tdb_rh(25, 0.8)
# rhov_from_tdb_rh {{{
rhov_from_tdb_rh <- function (tdb, rh) {
    psat_from_tdb(tdb) * rh
}
# }}}

#' Calculate density of water
#'
#' @details
#'
#' `rhoh2o()` calculates density of water from temperature (in Celsius).
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @return A numeric vector of water density in kg/m3.
#' @rdname density_water
#' @export
#' @examples
#' rhoh2o(15)
# rhoh2o {{{
rhoh2o <- function (tdb) {
    1000.1207 + 8.3215874e-04 * tdb - 4.929976e-03 * pow_2(tdb) + 8.4791863e-06 * pow_3(tdb)
}
# }}}
