#' Calculate latent energy of air
#'
#' @details
#'
#' `hfgair_from_tdb()` calculates latent energy of air from temperature (in
#' Celsius).
#'
#' `hgair_from_tdb()` calculates latent energy of the moisture as a gas in
#' the air from temperature (in Celsius).
#'
#' The enthalpy of a mixture of perfect gases equals the sum of the individual
#' partial enthalpies of the components. Therefore, the specific enthalpy of
#' moist air can be calculated as a sum of the specific enthalpy for dry air and
#' the specific enthalpy for saturated water vapor at the temperature of the
#' mixture.
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @return `hfgair_from_tdb()` returns a numeric vector of heat of vaporization
#' for moist air in J/kg. `hgair_from_tdb()` returns a numeric vector of latent
#' energy of the moisture in J/kg.
#' @rdname latent_energy
#' @export
#' @examples
#' hfgair_from_tdb(13)
# hfgair_from_tdb {{{
hfgair_from_tdb <- function (tdb) {
    tdb <- pmax(tdb, 0)
    # enthalpy of the gas - enthalpy of the fluid
    hgair_from_w_tdb(tdb) - (4180.0 * tdb)
}
# }}}

#' @rdname latent_energy
#' @export
#' @examples
#' hgair_from_tdb(13)
# hgair_from_tdb {{{
hgair_from_tdb <- function (tdb) {
    2500940.0 + 1858.95 * tdb
}
# }}}

#' Calculate enthalpy of air
#'
#' @details
#'
#' `h_from_tdb_w()` calculates enthalpy of air from dry-bulb temperature (in
#' Celsius) and humidity ratio (in kgWater/kgDryAir).
#'
#' `h_from_tdb_rh_pb()` calculates enthalpy of air from dry-bulb temperature (in
#' Celsius), relative humidity (in [0.0, 1.0]) and barometric pressure (in
#' Pascals).
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @return A numeric vector of air enthalpy in J/kg.
#' @rdname enthalpy
#' @export
#' @examples
#' h_from_tdb_w(13, 0.002)
# h_from_tdb_w {{{
h_from_tdb_w <- function (tdb, w) {
    1.00484e3 * tdb + clean_w(w) * hgair_from_tdb(tdb)
}
# }}}

#' @rdname enthalpy
#' @export
#' @examples
#' h_from_tdb_rh_pb(13, 0.002)
# h_from_tdb_rh_pb {{{
h_from_tdb_rh_pb <- function (tdb, rh, pb) {
    h_from_tdb_w(tdb, pmax(w_from_tdb_rh_pb(tdb, rh, pb), MinHumRat))
}
# }}}

