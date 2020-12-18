# Function to find optimum Fmult
#' @importFrom stats optimize
#' @importFrom FLCore harvest m stock.n catch.wt
f1 <- function(fmult, stk, year, tac, iter) {
    stk0 <- stk[, as.character(year),,,, iter]
    z <- (harvest(stk0) * fmult) + m(stk0)
    yield <- stock.n(stk0) * (harvest(stk0) * fmult) / z * (1 - exp(-z)) * catch.wt(stk0)
    return((tac - as.numeric(colSums(yield)))^2)
}