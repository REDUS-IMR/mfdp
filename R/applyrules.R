# Apply various rules
#' @importFrom FLCore dims catch
applyRules <- function(obj, fwd = 1) {

	stk <- obj$stk
	hcrObj <- obj$parameters$hcrObj
	ssbunder <- obj$parameters$ssbunder
	fmult_tmp <- obj$fmult
	ftgt_tmp <- obj$ftgt

	# Get (max/min)TACVar
	if(!is.null(hcrObj$args$maxTACVar)) {
		maxVar <- hcrObj$args$maxTACVar
	} else {
		maxVar <- NA
	}

	if(!is.null(hcrObj$args$minTACVar)) {
		minVar <- hcrObj$args$minTACVar
	} else {
		minVar <- NA
	}

	years <- c(as.numeric(dims(stk)$minyear):as.numeric(dims(stk)$maxyear))
	maxyear <- tail(years, 1)

	# Get year
	yr <-  years[1 + fwd]

	# Get catch yields
	catchYields <- catch(stk)

	# Handle three year rule
	if(!is.null(hcrObj$args$threeYrRule) && hcrObj$args$threeYrRule == TRUE) {
		if( yr <= maxyear - 2) {
			catchYields[, as.character(yr)] <- rowMeans(catchYields[,as.character(yr:(yr+2))])
		}
	}

	# Don't apply limit if ssb is under for this year
	if( ssbunder[, as.character(yr)] == 0) {
		maxTAC <- catchYields[, as.character(yr - 1)] + catchYields[, as.character(yr - 1)] * maxVar
		minTAC <- catchYields[, as.character(yr - 1)] - catchYields[, as.character(yr - 1)] * minVar
		TAC <- catchYields[, as.character(yr)]

		if(TAC > maxTAC) {
			catchYields[, as.character(yr)] <- maxTAC
		} else if(TAC < minTAC) {
			catchYields[, as.character(yr)] <- minTAC
		}
	}

	return(as.integer(catchYields[, as.character(yr)]))
}

# Apply various rules
#' @importFrom FLCore dims catch
applyRulesOld <- function(obj) {

	stk <- obj$stk
	hcrObj <- obj$parameters$hcrObj
	ssbunder <- obj$parameters$ssbunder
	fmult_tmp <- obj$fmult
	ftgt_tmp <- obj$ftgt

	# Get (max/min)TACVar
	if(!is.null(hcrObj$args$maxTACVar)) {
		maxVar <- hcrObj$args$maxTACVar
	} else {
		maxVar <- NA
	}

	if(!is.null(hcrObj$args$minTACVar)) {
		minVar <- hcrObj$args$minTACVar
	} else {
		minVar <- NA
	}

	# Get last year's assessment advice
	if(!is.null(hcrObj$args$tac)) {
		realtac <- hcrObj$args$tac
	} else {
		realtac <- NA
	}

	years <- c(as.numeric(dims(stk)$minyear):as.numeric(dims(stk)$maxyear))
	maxyear <- tail(years, 1)

	for(yr in years[2:length(years)]) {

		catchYields <- catch(stk)

		# Handle three year rule
		if(!is.null(hcrObj$args$threeYrRule) && hcrObj$args$threeYrRule == TRUE) {
			if( yr <= maxyear - 2) {
				catchYields[, as.character(yr)] <- rowMeans(catchYields[,as.character(yr:(yr+2))])
			}
		}

		# Handle max TAC difference
		if(!is.na(realtac)) {

			# Look for last year assessment TAC
			if(!is.na(maxVar)) {
				catchYields[, 1] <- realtac
			}

			maxTAC <- catchYields[, as.character(yr - 1)] + catchYields[, as.character(yr - 1)] * maxVar
			minTAC <- catchYields[, as.character(yr - 1)] - catchYields[, as.character(yr - 1)] * minVar
			TAC <- catchYields[, as.character(yr)]

			if(TAC > maxTAC) {
				catchYields[, as.character(yr)] <- maxTAC
			} else if(TAC < minTAC) {
				catchYields[, as.character(yr)] <- minTAC
			}
		}

		# Repopulate if necessary
		if(catchYields[, as.character(yr)] != catch(stk)[, as.character(yr)]) {

			res <- optimize(f1, stk, yr, catchYields[, as.character(yr)], 1, interval = c(0, 2), tol = 0.00001)

			# Get fmult for all iters
			fmult_tmp[, as.character(yr)] <- res[[1]]

			# Re-populate target
			ftgt_tmp[, as.character(yr)] <- fbar(stk[, as.character(yr)]) * fmult_tmp[, as.character(yr)]

			# Re-populate FLStock
			stk <- calcForward(stk, fmult_tmp, yr, years)
		}
	}
	return(list(stk = stk, fmult = fmult_tmp, ftgt = ftgt_tmp, hcrObj = hcrObj))
}