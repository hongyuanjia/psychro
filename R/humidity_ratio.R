#' Calculate humidity ratio of air
#'
#' @details
#'
#' `w_from_tdb_h()` calculates humidity ratio of air from dry-bulb temperature
#' (in Celsius) and enthalpy (in J/kg).
#'
#' `w_from_tdb_rh_pb()` calculates humidity ratio of air from dry-bulb
#' temperature (in Celsius), relative humidity (in [0.0, 1.0]) and barometric
#' pressure (in Pascals).
#'
#' `w_from_tdb_twb_pb()` calculates humidity ratio of air from dry-bulb
#' temperature (in Celsius), wet-bulb temperature (in Celsius) and barometric
#' pressure (in Pascals).
#'
#' `w_from_tdp_pb()` calculates humidity ratio of air from dew-point temperature
#' (in Celsius) and barometric pressure (in Pascals).
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @param h A numeric vector of air enthalpy in J/kg.
#' @rdname humidity_ratio
#' @return A numeric vector of air humidity ratio in kgWater/kgDryAir.
#' @export
#' @examples
#' w_from_tdb_h(13, 0.002)
# w_from_tdb_h {{{
w_from_tdb_h <- function (tdb, h) {
    w <- (h - 1.00484e3 * tdb) / hgair_from_tdb(tdb)

    if (w <= -0.0001) warning()

    w[w < 0.0] <- MinHumRat

    w
}
# }}}

#' @rdname humidity_ratio
#' @export
#' @examples
#' w_from_tdp_pb(1, 130000)
# w_from_tdp_pb {{{
w_from_tdp_pb <- function (tdp, pb) {
    pdew <- psat_from_tdb(tdp)
    w <- pdew * 0.62198 / (pb - pdew)

    if (w <= -0.0001) warning()

    w[w < 0.0] <- MinHumRat

    w
}
# }}}

#' @rdname humidity_ratio
#' @export
#' @examples
#' w_from_tdb_rh_pb(1, 2, 3)
# w_from_tdb_rh_pb {{{
w_from_tdb_rh_pb <- function (tdb, rh, pb) {
    pdew <- rh * psat_from_tdb(tdp)

    # Numeric error check when the temperature and RH values cause Pdew to equal
    # or exceed barometric pressure which is physically impossible. An approach
    # limit of 1000 pascals was chosen to keep the numerics stable as the
    # denominator approaches 0.
    w <- pdew * 0.62198 / pmax(pb - pdew, 1000.0)

    if (w <= -0.0001) warning()

    w[w < 0.0] <- MinHumRat

    w
}
# }}}

#' @rdname humidity_ratio
#' @export
#' @examples
#' w_from_tdb_twb_pb(20, 13, 13000)
# w_from_tdb_twb_pb {{{
w_from_tdb_twb_pb <- function (tdb, twb, pb) {
    if (any(twb > (tdb + 0.01))) {
        warning()
    }

    twb[twb > tdb] <- tdb[twb > tdb]

    pwet <- psat_from_tdb(twb)
    wet <- 0.62198 * pwet / (pb - pwet)
    w <- ((2501.0 - 2.381 * twb) * wet - (tdb - twb)) / (2501.0 + 1.805 * tdb - 4.186 * twb)

    if (w <= -0.0001) warning()

    w[w < 0.0] <- MinHumRat

    w
}
# }}}

