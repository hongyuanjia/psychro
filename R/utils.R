# check_abnormal {{{
check_abnormal <- function (results, check, env = parent.frame()) {
    if (all(!check)) return(results)

    fn <- paste0(as.character(sys.call(1)[[1L]]), "()")

    args(as.name(fn))
}
# }}}

# f6 {{{
f6 <- function (x, a0, a1, a2, a3, a4, a5) {
    a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * a5))))
}
# }}}

# f7 {{{
f7 <- function (x, a0, a1, a2, a3, a4, a5, a6) {
    (a0 + x * (a1 + x * (a2 + x * (a3 + x * (a4 + x * (a5 + x * a6)))))) / 1.0e10
}
# }}}

# clean_w {{{
clean_w <- function (w) {
    pmax(w, MinHumRat)
}
# }}}
