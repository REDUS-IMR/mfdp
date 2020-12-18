# Read MFDP index file
#' @importFrom FLCore FLStock FLQuant range<- mat<- m<- stock.wt<- m.spwn<- harvest.spwn<- catch.wt<- units<-
read_mfdp <- function(indexfile) {
    read_inner <-function(inputfile) {
        # Read lines
        raw <- readLines(inputfile)
        # Get Sex and File IDs
        ids <- read.csv(text = raw[2], header = FALSE)
		# Maybe it's a control file
		if(length(ids) < 2) {
			return(NULL)
		}
		sexid <- ids[,1]
		fileid <- ids[,2]
        # Get Year
        year <- read.csv(text = raw[3], header = FALSE)
		yearLen <- length(c(as.numeric(year[,1]):as.numeric(year[,2])))
        # Get Age
        age <- read.csv(text = raw[4], header = FALSE)
		ageLen <- length(c(as.numeric(age[,1]):as.numeric(age[,2])))
        # Get matrices
		data <- lapply(raw[6:(6 + yearLen - 1)], function(x) {
				if(nchar(x) > 0)
					read.csv(text = x, header = FALSE, stringsAsFactors = FALSE)
				else
					return(NULL)
			})
        data_1 <- do.call(rbind, data)
		data_1 <- data_1[, c(1:ageLen)]
        # Construct FLQuant
        ret <- FLQuant(t(as.matrix(data_1)), dimnames = list(age = c(age[,1]:age[,2]), year = c(year[,1]:year[,2])))
		attr(ret, "sexid") <- sexid
		attr(ret, "fileid") <- fileid
        return(ret)
    }
	
    raw <- readLines(indexfile)

	# Translate fileindex
	translate_table <- c("3" = "CWt", "4" = "SWt", "5" = "M", "6" = "Mat", "7" = "PF", "8" = "PM", "12" = "F", "13" = "N")

    # Get data
    raw_data <- list()
	control_files <- list()
	for(i in (seq_len(length(raw) - 3) + 3)) {
		tmpdat <- read_inner(raw[[i]])
		if(!is.null(tmpdat)) {
			# Read fileid
			fileid <- attr(tmpdat, "fileid")
			# Translate into category string
			category <- translate_table[as.character(fileid)]
			if (is.na(category)) {
				print("Invalid index category of input file: ")
				print(raw[[i]])
			}
			raw_data[[category]] <- tmpdat
		} else {
			control_files <- c(control_files, raw[[i]])
		}
	}

    # Read control file
	for (cf in control_files) {
    	ctrl_raw <- readLines(cf)
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

    # Re-adjust N
    tmp <- raw_data[["SWt"]]
    tmp[] <- NA
    tmp[,1] <- raw_data[["N"]]
    tmp[1,] <- as.numeric(rec[,c(1:length(tmp[1,]))])
    raw_data[["N"]] <- tmp

    # Create FLStock
    stk <- FLStock(raw_data[["N"]])
	# Set Fbar
    range(stk)[c("minfbar", "maxfbar")] <- as.numeric(fbar)
	# Fill values
	stock.n(stk) <- raw_data[["N"]]
    mat(stk) <- raw_data[["Mat"]]
	m(stk) <- raw_data[["M"]]
	catch.wt(stk) <- raw_data[["CWt"]]
	stock.wt(stk) <- raw_data[["SWt"]]
	harvest(stk) <- raw_data[["F"]]
	m.spwn(stk) <- raw_data[["PM"]]
	harvest.spwn(stk) <- raw_data[["PF"]]
	units(harvest(stk)) <- "f"

    return(list(stk = stk, nyr = nyr, flag = flag, target = target))
}