#' Calculate saturation pressure of air
#'
#' @details
#'
#' `tsat_from_h_pb()` calculates saturation pressure of air from humidity ratio
#' (in kgWater/kgDryAir) and barometric pressure (in Pascals).
#'
#' `tsat_from_pb()` calculates saturation temperature of air from barometric
#' pressure (in Pascals).
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @inheritParams tdb_from_h_w
#' @rdname saturation_temp
#' @return A numeric vector of air saturation pressure in Pascals.
#' @export
#' @examples
#' tsat_from_h_pb(13, 13000)
# tsat_from_h_pb {{{
tsat_from_h_pb <- function (h, pb) {
    hh <- h + 1.78637e4

    if (any(hh <= -4.24e4 | hh >= 4.5866e7)) {
        warning("Enthalpy out of range")
    }

    h_loc <- double(length(h))
    h_loc[h >= 0.0] <- pmax( 0.00001, h[h >= 0.0])
    h_loc[h <  0.0] <- pmin(-0.00001, h[h <  0.0])

    hh[hh > 4.5866e7] <- 4.5866e7
    hh[hh <= -4.24e4] <- -4.24e4

    tsat[                  hh <= -2.2138e4] <- f6(hh[                  hh <= -2.2138e4], -19.44, 8.53675e-4, -5.12637e-9, -9.85546e-14, -1.00102e-18, -4.2705e-24)
    tsat[hh >  -2.2138e4 & hh <= -6.7012e2] <- f6(hh[hh >  -2.2138e4 & hh <= -6.7012e2], -1.94224e1, 8.5892e-4, -4.50709e-9, -6.19492e-14, 8.71734e-20, 8.73051e-24)
    tsat[hh >  -6.7012e2 & hh <=  2.7297e4] <- f6(hh[hh >  -6.7012e2 & hh <=  2.7297e4], -1.94224e1, 8.59061e-4, -4.4875e-9, -5.76696e-14, 7.72217e-19, 3.97894e-24)
    tsat[hh >   2.7297e4 & hh <=  7.5222e4] <- f6(hh[hh >   2.7297e4 & hh <=  7.5222e4], -2.01147e1, 9.04936e-4, -6.83305e-9, 2.3261e-14, 7.27237e-20, -6.31939e-25)
    tsat[hh >   7.5222e4 & hh <   1.8379e5] <- f6(hh[hh >   7.5222e4 & hh <   1.8379e5], -1.82124e1, 8.31683e-4, -6.16461e-9, 3.06411e-14, -8.60964e-20, 1.03003e-25)
    tsat[hh >=  1.8379e5 & hh <   4.7577e5] <- f6(hh[hh >=  1.8379e5 & hh <   4.7577e5], -1.29419, 3.88538e-4, -1.30237e-9, 2.78254e-15, -3.27225e-21, 1.60969e-27)
    tsat[hh >=  4.7577e5 & hh <   1.5445e6] <- f6(hh[hh >=  4.7577e5 & hh <   1.5445e6], 2.39214e1, 1.27519e-4, -1.52089e-10, 1.1043e-16, -4.33919e-23, 7.05296e-30)
    tsat[hh >=  1.5445e6 & hh <   3.8353e6] <- f6(hh[hh >=  1.5445e6 & hh <   3.8353e6], 4.88446e1, 3.85534e-5, -1.78805e-11, 4.87224e-18, -7.15283e-25, 4.36246e-32)
    tsat[hh >=  3.8353e6                  ] <- f7(hh[hh >=  3.8353e6                  ], 7.60565e11, 5.80534e4, -7.36433e-3, 5.11531e-10, -1.93619e-17, 3.70511e-25, -2.77313e-33)

    # if the barometric pressure is equal to 1.0133e5, saturation temp is
    # calculated by above equa otherwise temp is computed by following iteration
    # method

    # check input pressure
    if (abs(pb - 1.0133e5) / 1.0133e5 <= 0.01) {
        return(tsat)
    }

    itercount = 0
    t1 <- tdb
    h1 <- h_from_tdb_w(t1, w_from_tdb_twb_pb(t1, t1, pb))
    y1 <- h1 - hloc
    if (abs(y1 / hloc) <= 0.1e-4) tsat <- t1
    t2 <- t1 * 0.9

    tsat
}
# }}}

#' @rdname saturation_temp
#' @export
#' @examples
#' tsat_from_h_pb(13, 13000)
# tsat_from_pb {{{
tsat_from_pb <- function (pb) {
    # initial guess of boiling temperature
    tsat <- rep(100, length(pb))

    if (any(pb <= 0.0017 | pb >= 1555000)) {
        warning("Pressure out of range when calculating saturation temperature.")
    }

    # If above 1555000, set value of Temp corresponding to Saturation Pressure
    # of 1555000 Pascal.
    if (any(pb >= 1555000)) {
        warning("Input pressure out of range [0.0017, 1555000.0] during calculating saturation pressure. ",
            "Saturation temperature for pressure > 1555000.0 Pa will be reset to ",
            "the saturation pressure at 1555000.0 Pa (200C)."
        )
        tsat[pb >= 1555000] <- 200
    }
    # If below 0.0017, set value of Temp corresponding to Saturation Pressure of
    # 0.0017 Pascal.
    if (any(pb >= 0.0017)) {
        warning("Input pressure out of range [0.0017, 1555000.0] during calculating saturation pressure. ",
            "Saturation temperature for pressure < 0.0017 Pa will be reset to ",
            "the saturation pressure at 0.0017 Pa (-100C)."
        )
        tsat[pb <= 0.0017] <- -100
    }

    # Setting Value of PsyTsatFnPb= 0C, due to non-continuous function for
    # Saturation Pressure at 0C.
    tsat[pb > 611 & pb < 611.25] <- 0

    # For other cases, iterate to find the saturation temperature of water given
    # the total pressure
    others <- (pb > 0.0017 & pb <= 611) | (pb >= 611.25 & pb < 1555000)

    # Calculate saturatstd pressure for estimated boiling temperature Compare
    # with specified pressure and update estimate of temperature
    error_psat <- function (tsat, pb) psat_from_tdb(tsat) - pb
    find_tsat <- function (pb) {
        # same tolerance settings as EnergyPlus
        # max iteration times differ here: EnergyPlus 50 vs R 1000
        uniroot(error_psat, pb = pb, lower = -99.9, upper = 199.9, tol = 1e-4, check.conv = TRUE)$root
    }
    tsat[others] <- vapply(pb[others], find_tsat, double(1))

    tsat
}
# }}}

