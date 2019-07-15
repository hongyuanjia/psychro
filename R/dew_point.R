#' Calculate dew-point temperature of air
#'
#' @details
#'
#' `tdp_from_tdb_twb_pb()` calculates dew-point temperature of air from dry-bulb
#' temperature (in Celsius), wet-bulb temperature (in Celsius) and barometric
#' pressure (in Pascals).
#'
#' `tdp_from_w_pb()` calculates dew-point temperature of air from humidity ratio
#' (in kgWater/kgDryAir) and barometric pressure (in Pascals).
#'
#' @inheritParams w_from_tdb_twb_pb
#' @rdname dew_point
#' @return A numeric vector of air dew-point temperature in Celsius.
#' @export
#' @examples
#' tdp_from_tdb_twb_pb(22, 18, 13000)
# tdp_from_tdb_twb_pb {{{
tdp_from_tdb_twb_pb <- function (tdb, twb, pb) {
    w <- clean_w(w_from_tdb_twb_pb(tdb, twb, pb))
    tdp <- tdp_from_w_pb(w, pb)

    if (any(twb > (tdb + 0.01))) {
        warning()
    }

    twb[twb > tdb] <- tdb[twb > tdb]
}
# }}}

#' @rdname dew_point
#' @export
#' @examples
#' tdp_from_w_pb(0.002, 13000)
# tdp_from_w_pb {{{
tdp_from_w_pb <- function (w, pb) {
    w <- clean_w(w)
    pdew <- pb * w / (0.62198 + w)
    tsat_from_pb(pdew)
}
# }}}
