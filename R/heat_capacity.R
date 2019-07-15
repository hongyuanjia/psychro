#' Calculate heat capacity of air at constant pressure
#'
#' @details
#'
#' `cpair_from_w_tdb()` calculates the heat capacity of air at constant pressure
#' from  temperature (in Celsius) and humidity ratio (in kgWater/kgDryAir).
#'
#' @inheritParams rhoair_from_tdb_w_pb
#' @rdname heat_capacity
#' @return A numeric vector of heat capacity of air at constant pressure in
#' J/(kg.C).
#' @export
#' @examples
#' cpair_from_tdb_w(20, 0.002)
# cpair_from_tdb_w {{{
cpair_from_w_tdb <- function (tdb, w) {
    w <- clean_w(w)
    (h_from_tdb_w(tdb + 0.1, w) - h_from_tdb_w(tdb, w)) * 10.0
}
# }}}


