# Do forecast!
#' @importFrom FLCore propagate fbar
doForecast <- function(stk, parameters, management.options.table = FALSE, single.option.prediction = FALSE) {

	if(as.logical(management.options.table) == as.logical(single.option.prediction)) {
		print("You have to choose either TAC management.options.table or single.option.prediction")
		return(NA)
	}

	if(management.options.table == TRUE) {
		fmin <- 0
		fmax <- 2
		increment <- 0.1

		managementSet <- seq(fmin, fmax, by = increment)

		# Prepare iter slots (e.g., 41 slots for F range between 0 and 2, with 0.05 increments)
		stk <- propagate(stk, length(managementSet))
	}
	# Get years
	years <- c(as.numeric(dims(stk)$minyear):as.numeric(dims(stk)$maxyear))

	# Placeholder for fmult
	fmult_tmp <- catch(stk)
	fmult_tmp[] <- 0

	# Placeholder for Ftarget
	ftgt_tmp <- catch(stk)
	ftgt_tmp[] <- 0

	# We need flag to indicate under bpa
	parameters$ssbunder <- catch(stk)
	parameters$ssbunder[] <- 0

    # Loop years
	for (y in seq_along(years)) {

		yr <- years[y]

		# Get parameters flag & target
		if(y <= length(parameters$flag)) {
			yr_flag <- parameters$flag[y]
			yr_trgt <- parameters$target[y]
		} else {
			yr_flag <- 0
			yr_trgt <- 1
		}

	    # Do Fmult optimize based on TAC in either all year of management.options.table or only first year of single.option.prediction
	    if ( yr %in% head(years, -2) || single.option.prediction == TRUE) {
			if(yr_flag == 1) {
				tac <- yr_trgt
				result1 <- optimize(f1, stk, yr, tac, 1, interval = c(0, 2), tol = 0.00001)
				# Get fmult for all iters
				fmult_tmp[, as.character(yr)] <- result1[[1]]
				ftgt_tmp[, as.character(yr)] <- fbar(stk[, as.character(yr)]) * fmult_tmp[, as.character(yr)]
			} else if(yr_flag == 0) {
				fmult_tmp[, as.character(yr)] <- yr_trgt
				ftgt_tmp[, as.character(yr)] <- fbar(stk[, as.character(yr)]) * fmult_tmp[, as.character(yr)]
			} else if(yr_flag == 3) {
				# Simple Ftgt with Bpa limit

				## Check SSB < Bpa
				ssb <- as.numeric(ssb(stk)[, as.character(yr)])[1]
				bpa <- parameters$hcrObj$args$bpa
				if(ssb < bpa) {
					parameters$ssbunder[, as.character(yr)] <- 1
					yr_trgt <- (ssb / bpa) * yr_trgt
				}

				# Apply Ftarget
				ftgt_tmp[, as.character(yr)] <- yr_trgt
				fmult_tmp[, as.character(yr)] <- ftgt_tmp[, as.character(yr)] / fbar(stk[, as.character(yr)])
			}
	    }

	    # For management.options.table, we multiple FMult in the forecast years
	    if (management.options.table == TRUE && yr %in% tail(years, 2)) {
			fmult_tmp[, as.character(yr),,,,] <- managementSet
	    }

	    stk <- calcForward(stk, fmult_tmp, yr, years)
	}

	return(list(stk = stk, fmult = fmult_tmp, ftgt = ftgt_tmp, parameters = parameters))
}