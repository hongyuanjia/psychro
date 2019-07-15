#' Calculate relative humidity of air
#'
#' @details
#'
#' `rh_from_tdb_rhov_lbnd0c()` calculates relative humidity of air from dry-bulb
#' temperature (in Celsius) and vapor density (in kg/m3). The name signifies
#' derivation and temperatures were used with 0C as minimum.
#'
#' `rh_from_tdb_rhov()` calculates relative humidity of air from dry-bulb
#' temperature (in Celsius) and vapor density (in kg/m3).
#'
#' `rh_from_tdb_w_pb()` calculates relative humidity of air from dry-bulb
#' temperature (in Celsius), humidity ratio (in kgWater/kgDryAir) and barometric
#' pressure (in Pascals).
#'
#' @param rhov A numeric vector of vapor density in kg/m3.
#' @inheritParams rhoair_from_tdb_w_pb
#' @rdname relative_humidity
#' @return A numeric vector of air relative humidity in [0.0 - 1.0].
#' @export
#' @examples
#' h_from_tdb_w(13, 0.002)
# rh_from_tdb_rhov_lbnd0c {{{
rh_from_tdb_rhov_lbnd0c <- function (tdb, rhov) {
    rh <- rep(0.0, length(tdb))

    above_zero <- rhov > 0.0
    rh[above_zero] <- rhov[above_zero] * VaporConst * (tdb + KelvinConv) * exp(-23.7093 + 4111.0 / ((Tdb + KelvinConv) - 35.45))

    check_abnormal(rh, rh < -0.05, rh > 1.01)

    rh[rh < 0.01] <- 0.01
    rh[rh > 1.0] <- 1.0
    rh
}
# }}}

#' @rdname relative_humidity
#' @export
#' @examples
#' rhov_from_tdb_rhov(25, 0.8)
# rh_from_tdb_rhov {{{
rh_from_tdb_rhov <- function (tdb, rhov) {
    rh <- rep(0.0, length(tdb))

    above_zero <- rhov > 0.0
    rh[above_zero] <- rhov[above_zero] * VaporConst * (tdb + KelvinConv) / psat_from_tdb(tdb)

    check_abnormal(rh, rh < -0.05, rh > 1.01)

    rh[rh < 0.01] <- 0.01
    rh[rh > 1.0] <- 1.0
    rh
}
# }}}

#' @rdname relative_humidity
#' @export
#' @examples
#' rhov_from_tdb_rhov(25, 0.8)
# rh_from_tdb_w_pb {{{
rh_from_tdb_w_pb <- function (tdb, w, pb) {
    pws <- psat_from_tdb(tdb)

    # find degree of saturation
    w <- clean_w(w)
    u <- w / 0.62198 * pws / (pb - pws)

    rh <- u / (1.0 - (1.0 - u) * (pws / pb))

    check_abnormal(rh, rh < -0.05, rh > 1.01)

    rh[rh < 0.01] <- 0.01
    rh[rh > 1.0] <- 1.0
    rh
}
# }}}

