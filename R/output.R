# Generate Table 1 (management.options.table)
#' @importFrom grDevices dev.off pdf
#' @importFrom ggplot2 ggsave
#' @importFrom FLCore stock<- computeStock stock ssb
#' @importFrom data.table as.data.table := setcolorder
#' @importFrom gridExtra arrangeGrob tableGrob grid.arrange
#' @importFrom grid textGrob gList 
generateTable1 <- function(res, filename) {

	# Getting data
	stk <- res$stk
	fmult <- res$fmult
	tac <- res$tac
	ftgt <- res$ftgt

	# Populate missing aggregate values
	stock(stk) <- computeStock(stk)
	catch(stk) <- computeCatch(stk)

	# Collect all
	out <- cbind(as.data.table(round(stock(stk))), type = "Biomass")
	out <- rbind(out, cbind(as.data.table(round(ssb(stk))), type = "SSB"))
	out <- rbind(out, cbind(as.data.table(round(fmult, 4)), type = "FMult"))
	out <- rbind(out, cbind(as.data.table(round(fbar(stk), 4)), type = "FBar"))
	out <- rbind(out, cbind(as.data.table(round(catch(stk))), type = "Landings"))
	#out <- rbind(out, cbind(as.data.table(round(tac)), type = "TAC"))

	ret <- dcast(out, year + iter ~ type, , value.var = "value" )
	ret <- ret[order(as.numeric(year), as.numeric(iter)),]
	ret[, iter := NULL]
	ret <- ret[!duplicated(ret), ]

	setcolorder(ret, c("year", "Biomass", "SSB", "FMult", "FBar", "Landings"))

	out <- list()
	outGrob <- list()
	formattedOut <- list()

	# Print all years
	yrList <- unlist(unique(ret[, "year"]))

	# All except last two years
	out[[1]] <-ret[year %in% head(yrList, -2), ]

	# 2nd last year
	out[[2]] <-  ret[year == head(tail(yrList, 2), 1) ]

	# last year
	out[[3]] <- ret[year == tail(yrList, 1)][, `:=`(FBar = NULL, FMult = NULL, Landings = NULL)]

	for(i in seq_len(length(out))) {
		outGrob[[i]] <- arrangeGrob(tableGrob(out[[i]], rows = NULL), top=(textGrob(yrList[[i]])))
		formattedOut[[i]] <- list(content = out[[i]])
	}

	pdf(filename, height = 13, width = 6 * length(outGrob))

	nCol <- length(outGrob)

	do.call(grid.arrange, c(do.call(gList, outGrob), ncol = nCol))

	dev.off()

	return(formattedOut)
}

# Generate Table 2 (single.option.prediction)
#' @importFrom grDevices dev.off pdf
#' @importFrom FLCore stock.wt m.spwn mat catch.n iter harvest.spwn
#' @importFrom data.table dcast := copy
#' @importFrom gridExtra gtable_combine
#' @importFrom grid grid.newpage grid.draw
generateTable2 <- function(res, filename) {

	# Getting data
	stk <- res$stk
	fmult <- res$fmult
	tac <- res$tac
	ftgt <- res$ftgt

	# Get just first iter
	stk_1 <- iter(stk, 1)

	out <- cbind(as.data.table(stock.n(stk_1)), type = "StockN (Jan)")
	out <- rbind(out, cbind(as.data.table(stock.n(stk_1) * stock.wt(stk_1)), type = "StockBiomass (Jan)"))
	out <- rbind(out, cbind(as.data.table(stock.n(stk_1) * exp(-(harvest(stk_1) *
		    harvest.spwn(stk_1) + m(stk_1) * m.spwn(stk_1))) *
		    stock.wt(stk_1) * mat(stk_1)), type = "SSB (Jan)"))
	out <- rbind(out, cbind(as.data.table(harvest(stk_1)), type = "F"))
	out <- rbind(out, cbind(as.data.table(catch.n(stk_1)), type = "CatchN"))
	out <- rbind(out, cbind(as.data.table(catch.n(stk_1) * catch.wt(stk_1)), type = "CatchYield"))

	ret <- dcast(out, year + age + iter ~ type, , value.var = "value" )
	ret <- ret[order(as.numeric(year), as.numeric(age)),]
	ret[, iter := NULL]

	formattedOut <- list()

	yrList <- unlist(unique(ret[, "year"]))

	for(yr in yrList) {
		tbl0 <- copy(ret[year == yr,])
		tbl0[, year := NULL]
		
		header <- rbind(c("Year:", yr, "F multiplier:", as.numeric(fmult[, yr]), "Fbar:", as.numeric(ftgt[, yr])))

		sfunc <- function(z) if (is.numeric(z)) sum(z) else ''
		footer <- as.data.table(lapply(tbl0, sfunc))
		footer[, `:=`(F="", age = "TOTAL")]

		# Prepare output
		formattedOut[[yr]] <- list(header = header, content = rbind(tbl0, footer)) 
	}

	# Generate pdf
	pdf(filename, width = 17)
	for(yr in yrList) {
		# Combine tables
		outGrob <- gtable_combine(tableGrob(formattedOut[[yr]][["header"]]), tableGrob(formattedOut[[yr]][["content"]], rows = NULL), along = 2)
		grid.newpage()
		grid.draw(outGrob)
	}
	dev.off()

	return(formattedOut)
}

# Generate XLSX file
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
generateXlsx <- function(table1 = NA, table2 = NA, table3 = NA, filename) {

	wb <- createWorkbook()
	if(!is.na(table1[1])) {
		for(i in names(table1)) {
			wsName <- paste0(filename, "_prm_", i)
			addWorksheet(wb, wsName)
			writeData(wb, wsName, table1[[i]][["content"]])
		}
	}

	if(!is.na(table2[1])) {
		for(i in names(table2)) {
			wsName <- paste0(filename, "_prs_", i)
			addWorksheet(wb, wsName)
			writeData(wb, wsName, table2[[i]][["header"]], colNames = FALSE, startRow = 1)
			writeData(wb, wsName, table2[[i]][["content"]], startRow = 2)
		}
	}

	if(!is.na(table3[1])) {
		for(i in names(table3)) {
			wsName <- paste0(filename, "_prs_rules_", i)
			addWorksheet(wb, wsName)
			writeData(wb, wsName, table3[[i]][["header"]], colNames = FALSE, startRow = 1)
			writeData(wb, wsName, table3[[i]][["content"]], startRow = 2)
		}
	}
	saveWorkbook(wb, file = paste0(filename, ".xlsx") , overwrite = TRUE)
}

# Generate plots
#' @importFrom ggplotFL plot
generatePlots <- function(stk1 = NA, stk2 = NA, stk3 = NA, filename) {
	if(isS4(stk1[[1]])) {
		plName <- paste0(filename, "_prm_plot.pdf")
		plot(stk1$stk)
		ggsave(plName)
	}
	if(isS4(stk2[[1]])) {
		plName <- paste0(filename, "_prs_plot.pdf")
		plot(stk2$stk)
		ggsave(plName)
	}
	if(isS4(stk3[[1]])) {
		plName <- paste0(filename, "_prs_rules_plot.pdf")
		plot(stk3$stk)
		ggsave(plName)
	}
}