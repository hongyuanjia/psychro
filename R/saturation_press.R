#' Calculate saturation pressure of air
#'
#' @details
#'
#' `psat_from_tdb()` calculates saturation pressure of air from dry-bulb
#' temperature (in Celsius).
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @return A numeric vector of air saturation pressure in Pascals.
#' @references ASHRAE HANDBOOK OF FUNDAMENTALS, 2005, Chap 6 (Psychrometrics),
#' Eqn 5 & 6. Compared to Table 3 values (August 2007) with average error of
#' 0.00%, max .30%, min -.39%. (Spreadsheet available on request - Lawrie).
#' @rdname saturation_press
#' @export
#' @examples
#' psat_from_tdb(25)
# psat_from_tdb {{{
psat_from_tdb <- function (tdb) {
    kel_conv <- 273.15
    t_kel <- tdb + kel_conv

    # Inital
    pascal <- rep(NA_real_, length(tdb))

    below_m100     <- tdb <  - 100
    below_freezing <- !below_m100  & tdb <  0
    above_freezing <- tdb >=   0   & tdb <= 200
    above_200      <- tdb >    200

    # If below -100C, set value of Pressure corresponding to Saturation
    # Temperature of -100C.
    if (any(below_m100)) {
        pascal[below_m100] <- 0.0017
        warning("Input temperature out of range [-100, 200] during calculating saturation pressure. ",
            "Saturation pressure for temperature below -100C will be reset to ",
            "the saturation pressure at -100C (0.0017 Pa)."
        )
    }

    # If below freezing, calculate saturation pressure over ice.
    if (any(below_freezing)) {
        C1 <- -5674.5359
        C2 <- 6.3925247
        C3 <- -0.9677843e-2
        C4 <- 0.62215701e-6
        C5 <- 0.20747825e-8
        C6 <- -0.9484024e-12
        C7 <- 4.1635019

        t_ <- t_kel[below_freezing]
        pascal[below_freezing] <- exp(C1 / t_ + C2 + t_ * (C3 + t_ * (C4 + t_ * (C5 + C6 * t_))) + C7 * log(t_))
    }

    # If above freezing, calculate saturation pressure over liquid water.
    if (any(above_freezing)) {
        C8 <- -5800.2206
        C9 <- 1.3914993
        C10 <- -0.048640239
        C11 <- 0.41764768e-4
        C12 <- -0.14452093e-7
        C13 <- 6.5459673

        t_ <- t_kel[above_freezing]
        pascal[above_freezing] <- exp(C8 / t_ + C9 + t_ * (C10 + t_ * (C11 + t_ * C12)) + C13 * log(t_))
    }

    # If above 200C, set value of Pressure corresponding to Saturation
    # Temperature of 200C.
    if (any(above_200)) {
        pascal[above_200] <- 1555000
        warning("Input temperature out of range [-100, 200] during calculating saturation pressure. ",
            "Saturation pressure for temperature above 200C will be reset to ",
            "the saturation pressure at 200C (1555000.0 Pa)."
        )
    }

    pascal
}
# }}}

