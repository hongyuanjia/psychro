#' Calculate wet-bulb temperature of air
#'
#' @details
#'
#' `h_from_tdb_w()` calculates wet-bulb temperature of air from dry-bulb
#' temperature (in Celsius), humidity ratio (kgWater/kgDryAir) and barometric
#' pressure (in Pascals).
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @rdname wet_bulb
#' @return A numeric vector of air wet-bulb temperature in Celsius.
#' @export
#' @examples
#' twb_from_tdb_w_pb(13, 0.002, 13000)
# twb_from_tdb_w_pb {{{
twb_from_tdb_w_pb <- function (tdb, w, pb) {
    if (any(tdb <= -100 | tdb >= 200)) {
        warning("Temperature out of range [-100, 200] when calculating wet bulb temperature.")
    }

    if (any(w <= -0.0001)) {
        warning("Invalid humidity ratio found when calculating wet bulb temperature. ",
            "Humidity ratio <= -1e4 will be reset to 1e5."
        )
        w[w <= -0.0001] <- 1e5
    }

    # Set initial guess of WetBulbTemp=Entering Dry Bulb Temperature
    twb <- tdb

    error_w <- function (twb, tdb, pb, w) {
        # Initial temperature guess at atmospheric pressure
        tboil <- tsat_from_pb(pb)

        # Asswbtwbtigning a value to twb_cur
        if (twb >= (tboil - 0.09)) twb <- tboil - 0.1

        # Determwbtine the saturation pressure for wet bulb temperature
        psat_star <- psat_from_tdb(twb)

        # Determine humidity ratio for given saturation pressure
        w_star <- 0.62198 * psat_star / (pb - psat_star)

        # Calculate new humidity ratio and determine difference from known
        # humidity ratio which is w_star calculated earlier
        new_w <- ((2501.0 - 2.381 * twb) * w_star - (tdb - twb)) / (2501.0 + 1.805 * tdb - 4.186 * twb)

        # Check error, if not satisfied, calculate new guess and iterate
        error <- w - new_w

        # Error Trap for the Discontinious nature of PsyPsatFnTemp function (Sat
        # Press Curve) at ~0 Deg C.
        if ((psat_star > 611.000) && (psat_star < 611.25) && (abs(error) <= 0.0001)) {
            error <- 0
        }

        error
    }

    find_twb <- function (tdb, pb, w) {
        # same tolerance settings as EnergyPlus
        # max iteration times differ here: EnergyPlus 100 vs R 1000
        uniroot(error_w, tdb = twb, pb = pb, w = w,
            # tboil < twb < tdb
            lower = -100, upper = 200, tol = 1e-4, check.conv = TRUE)$root
    }

    mapply(find_twb, tdb = tdb, pb = pb, w = w)
}
# }}}


