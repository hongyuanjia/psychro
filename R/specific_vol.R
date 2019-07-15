#' @include constants.R
NULL

#' Calculate specific volume of air
#'
#' `v_from_tdb_w_pb()` calculates specific volume from dry-bulb temperature (in
#' Celsius), humidity ratio (in kgWater/kgDryAir) and barometric pressure (in
#' Pascals).
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @rdname specific_vol
#' @return A numeric vector of air specific volume in m3/kg.
#' @export
#' @examples
#' v_from_tdb_w_pb(20, 0.02, 13000)
# v_from_tdb_w_pb {{{
v_from_tdb_w_pb <- function (tdb, w, pb) {
    w <- clean_w(w)
    v <- 1.59473e2 * (1.0 + 1.6078 * w) * (1.8 * tdb + 492.0) / pb

    if (any(v < -0.01)) {
        warning("Calculated specific volume out of range.")
    }

    v[v < 0.0] <- 0.83
    v
}
# }}}
