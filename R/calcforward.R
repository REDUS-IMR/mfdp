# Process a year's catch and populate the next year stock given an FLStock, year, and fmult
#' @importFrom FLCore expand catch.n<- catch<- computeCatch harvest<- stock.n<-
calcForward <- function(stk, fmult_tmp, yr, years) {
	# Populate this year's catch
	stk0 <- stk[, as.character(yr)]
	y <- harvest(stk0) * expand(fmult_tmp[, as.character(yr)], age = dimnames(stk0)$age)
	z <- y + m(stk0)
	catch.n(stk0) <- stock.n(stk0) * y / z * (1 - exp(-z))
	catch(stk0) <- computeCatch(stk0)

	# Put back harvest * fmult into FLStock
	harvest(stk0) <- y

	stk[, as.character(yr)] <- stk0

	# Populate next year's stock
	stk0_shifted <- stk0

	nage <- dim(stk0)[1]
	plusgroup <- range(stk0_shifted)["plusgroup"]

	stock.n(stk0_shifted)[as.character(plusgroup - 1), , ] <- stock.n(stk0_shifted)[as.character(plusgroup), , ] + stock.n(stk0_shifted)[as.character(plusgroup - 1), , ]
	stk0_shifted[2:nage, , ] <- stk0_shifted[1:(nage - 1), , ]
	stk0_shifted <- stock.n(stk0_shifted[2:nage, , ])
	z[2:nage, , ] <- z[1:(nage - 1), , ]
	z <- z[2:nage, , ]

	# Process next year stock
	if (tail(years, 1) != as.character(yr)) {
		stock.n(stk[2:nage, as.character(yr + 1)]) <- as.numeric(stk0_shifted * exp(-z))
	}

	return(stk)
}
