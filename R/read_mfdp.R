# Read MFDP index file
#' @importFrom FLCore FLStock FLQuant range<- mat<- m<- stock.wt<- m.spwn<- harvest.spwn<- catch.wt<- units<- window
#' @importFrom data.table fread rbindlist
read_mfdp <- function(indexfile) {
    read_inner <-function(inputfile) {
		# Check input file
		if(!file.exists(inputfile))
			return(NA)
        # Read lines
        raw <- readLines(inputfile)
        # Get Sex and File IDs
        ids <- fread(text = paste0(trimws(raw[2]), "\n"), header = FALSE)
		# Maybe it's a control file
		if(length(ids) < 2) {
			return(NULL)
		}
		sexid <- ids[[1]]
		fileid <- ids[[2]]
        # Get Year
        year <- fread(text = paste0(trimws(raw[3]), "\n"), header = FALSE)
		yearLen <- length(c(as.numeric(year[[1]]):as.numeric(year[[2]])))
        # Get Age
        age <- fread(text = paste0(trimws(raw[4]), "\n"), header = FALSE)
		ageLen <- length(c(as.numeric(age[[1]]):as.numeric(age[[2]])))
        # Get matrices
		data <- lapply(raw[6:(6 + yearLen - 1)], function(x) {
				if(nchar(x) > 0)
					fread(text = paste0(trimws(x), "\n"), header = FALSE, stringsAsFactors = FALSE)
				else
					return(NULL)
			})
        data_1 <- rbindlist(data)
		data_1 <- data_1[, 1:ageLen]
        # Construct FLQuant
        ret <- FLQuant(t(as.matrix(data_1)), dimnames = list(age = c(age[[1]]:age[[2]]), year = c(year[[1]]:year[[2]])))
		attr(ret, "sexid") <- sexid
		attr(ret, "fileid") <- fileid
        return(ret)
    }

	# Check input file
	if(!file.exists(indexfile))
		return(NA)

    # Get path
    path <- dirname(indexfile)

    # Read index
    raw <- readLines(indexfile)

	# Translate fileindex
	translate_table <- c("2" = "CN", "3" = "CWt", "4" = "SWt", "5" = "M", "6" = "Mat", "7" = "PF", "8" = "PM", "12" = "F", "13" = "N")

    # Get data
    raw_data <- list()
	control_files <- list()
	for(i in (seq_len(length(raw) - 2) + 2)) {
		tmpdat <- read_inner(paste0(path, "/", raw[[i]]))
		if(!is.null(tmpdat)) {
			if(is(tmpdat, "FLQuant")) {
				# Read fileid
				fileid <- attr(tmpdat, "fileid")
				# Translate into category string
				category <- translate_table[as.character(fileid)]
				if (is.na(category)) {
					print("Invalid index category of input file: ")
					print(raw[[i]])
				}
				raw_data[[category]] <- tmpdat
			}
		} else {
			control_files <- c(control_files, raw[[i]])
		}
	}

	if(length(control_files) == 0)
		stop("Unable to find a control file!")

    # Read control file
	for (cf in control_files) {
    	ctrl_raw <- readLines(paste0(path, "/", cf))
		## Get num of year
		nyr <- as.numeric(ctrl_raw[2])
		yrdiff <- nyr - 3
		## Get fbar
		fbar <- read.csv(text = ctrl_raw[3], header = FALSE)
		## Get recruitment
		rec <- read.csv(text = ctrl_raw[4], header = FALSE)
		## Get constraint type
		flag <- as.numeric(read.csv(text = paste(ctrl_raw[5:(5 + yrdiff)], collapse = ","), header = FALSE))
		## Get intermediate year's tac
		target <- as.numeric(read.csv(text = paste(ctrl_raw[(5 + yrdiff + 1):(5 + yrdiff * 2 + 1)], collapse = ","), header = FALSE))
	}

    # Create FLStock
	alldims <- rbindlist(lapply(raw_data, dims))
	age <- c(min(alldims[["min"]]), max(alldims[["max"]]))
	year <- c(min(alldims[["minyear"]]), max(alldims[["maxyear"]]))
    stk <- FLStock(FLQuant(NA, dimnames = list(age = c(age[[1]]:age[[2]]), year = c(year[[1]]:year[[2]]))))

	# Adjust dimension
	raw_data <- lapply(raw_data, window, start = year[[1]], end = year[[2]])

	# Fill recruitment values
	raw_data[["N"]][1,] <- as.numeric(rec[,1:length(raw_data[["N"]][1,])])

	# Set Fbar
    range(stk)[c("minfbar", "maxfbar")] <- as.numeric(fbar)
	# Fill values
	stock.n(stk) <- raw_data[["N"]]
    mat(stk) <- raw_data[["Mat"]]
	m(stk) <- raw_data[["M"]]
	if(!is.null(raw_data[["CN"]]))
		catch.n(stk) <- raw_data[["CN"]]
	catch.wt(stk) <- raw_data[["CWt"]]
	stock.wt(stk) <- raw_data[["SWt"]]
	harvest(stk) <- raw_data[["F"]]
	m.spwn(stk) <- raw_data[["PM"]]
	harvest.spwn(stk) <- raw_data[["PF"]]
	units(harvest(stk)) <- "f"

    return(list(stk = stk, nyr = nyr, flag = flag, target = target))
}