# Read single-file MFDP input table file
read_mfdp_input_table <- function(inputfile) {

	# Check input file
	if(!file.exists(inputfile))
		return(NA)

	raw <- readLines(inputfile)

	output <- list()
	start <- 0

	for (i in seq_len(length(raw))) {
	    line <- strsplit(raw[i], "\\s+")[[1]]

	    # Assume we have 9 columns
	    if (length(line) > 8 && start == 0) {
		start <- i
		year <- trimws(raw[i - 1])
	    }

	    if (length(line) <= 8 && start != 0) {
		output[[year]] <- read.csv(text = paste(raw[start:(i - 1)], collapse = "\n"), sep = "\t", stringsAsFactors = FALSE)
		start <- 0
	    }
	}

	# Last line
	if (length(line) > 8 && start != 0) {
	    output[[year]] <- read.csv(text = paste(raw[start:i], collapse = "\n"), sep = "\t", stringsAsFactors = FALSE)
	    start <- 0
	}

	# Get Fbar
	Fidx <- grep("Fbar", raw)
	Fbar <- regmatches(raw[Fidx], gregexpr("[0-9]+", raw[Fidx], perl = TRUE))[[1]]

	# Get years
	years <- sort(names(output))

	# Get ages
	ages <- sort(unique(unlist(lapply(output, function(x) {
	    return(x$Age)
	}))))

	# Construct FLStock
	convertVals <- function(input, column, ages, years) {
	    ret <- FLQuant(as.matrix(as.data.frame(lapply(input, function(x) {
		return(x[[column]])
	    }))), dimnames = list(age = ages, year = years))
	    return(ret)
	}

	stock.n <- suppressWarnings(convertVals(output, "N", ages, years))

	stk <- FLStock(stock.n)
	stock.n(stk) <- stock.n
	range(stk)[c("minfbar", "maxfbar")] <- as.numeric(Fbar)

	mat(stk) <- convertVals(output, "Mat", ages, years)
	m(stk) <- convertVals(output, "M", ages, years)
	catch.wt(stk) <- convertVals(output, "CWt", ages, years)
	stock.wt(stk) <- convertVals(output, "SWt", ages, years)
	harvest(stk) <- convertVals(output, "Sel", ages, years)
	m.spwn(stk) <- convertVals(output, "PM", ages, years)
	harvest.spwn(stk) <- convertVals(output, "PF", ages, years)
	units(harvest(stk)) <- "f"

	return(stk)
}