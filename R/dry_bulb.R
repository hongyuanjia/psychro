#' Calculate dry-bulb temperature of air
#'
#' @details
#'
#' `tdb_from_h_w()` calculates temperature of air from enthalpy (in
#' J/kg) and humidity ratio (in kgWater/kgDryAir).
#'
#' @param h A numeric vector of air enthalpy in J/(kg.K).
#' @inheritParams rhoair_from_tdb_w_pb
#' @rdname dry_bulb
#' @return A numeric vector of air temperature in Celsius.
#' @export
#' @examples
#' tdb_from_h_w()
# tdb_from_h_w {{{
tdb_from_h_w <- function (h, w) {
    (h - 2.50094e6 * w) / (1.00484e3 + 1.85895e3 * w)
}
# }}}

